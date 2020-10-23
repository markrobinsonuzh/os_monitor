#' Retrieve a table of records from orcid.org
#'
#' @param orcid ORCID to retrieve records for
#' @param exclude what type of ORCID records to exclude from the list
#'
#' @return
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
retrieve_from_orcid <- function(orcid, exclude = "data-set") {
  if(!check_if_likely_orcid(orcid)){
    return(tibble::tibble(title=character(),
                   journal=character(),
                   type=character(),
                   doi=character(),
                   year=integer(),
                   in_orcid=logical()))
  }
  works <- tryCatch({rorcid::orcid_works(orcid)},error=function(e) {
    return(NA)
  })
  if (is.na(works)){
    return(tibble::tibble(title=character(),
                          journal=character(),
                          type=character(),
                          doi=character(),
                          year=integer(),
                          in_orcid=logical()))
  }
  works <- works[[1]]$works
  if(nrow(works)==0) {
    df <- data.frame(title = character(0), journal=character(0),
                     type = character(0), doi = character(0), 
                     year = integer(0))
    return(df)
  }
  works$doi <- sapply(works$`external-ids.external-id`, 
                      function(u) ifelse(nrow(u)>0, u$`external-id-value`[u$`external-id-type`=="doi"], NA))
  works$doi <- tolower(works$doi)
  works$doi <- gsub("http://dx.doi.org/", "", works$doi)
  works <- works %>% dplyr::filter(!(type %in% exclude)) %>%
    dplyr::mutate(title = title.title.value, 
                  journal = `journal-title.value`,
                  year = `publication-date.year.value`) %>%
    dplyr::select(title, journal, type, doi, year)
  works$title <- sub("\\.$","",works$title)
  works <- unique(works)
  works_split <- split(works, works$doi)
  n <- sapply(works_split, nrow)
  z <- lapply(works_split[n>=2], function(u) {
    nas <- is.na(u$journal)
    if( sum(nas)>0 & sum(!nas)>0 )
      return(u[!nas,,drop=FALSE][1,,drop=FALSE])
    else
      return(u[1,,drop=FALSE])
  })
  df <- do.call(rbind,c(z,works_split[n==1])) %>% tibble::as_tibble()
  rownames(df) <- NULL
  df$in_orcid <- TRUE
  df$year <- as.integer(df$year)
  return(df)
}


check_if_likely_orcid <- function(orcid){
  splitorc <- stringr::str_split(orcid,"-",simplify = TRUE)
  if(length(splitorc) != 4){
    return(FALSE)
  }
  if(!all(stringr::str_length(splitorc)==4)){
    return(FALSE)
  }
  
  locorc <- stringr::str_locate_all(splitorc,"[:digit:]")
  locdig <- sapply(seq_len(4), function(i) dim(locorc[[i]])[1]==4)
  if(all(locdig) || (!locdig[4] &&  (stringr::str_split(splitorc[4],"",simplify = TRUE)[4] == "X"))){
    return(TRUE)
  } else{
    return(FALSE)
  }
}
# author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
# con <- dbConnect(odbc::odbc(), "PostgreSQL")
# create_tbl_author(author_vec,con)
# 
# family_given_names <- tbl(con, "authorkeys") %>% filter(authorkey_fullname==author_vec) %>% 
#   pull(authorname) %>% stringr::str_split(" ", n=2) %>% unlist()
# 
# 
# rorcid::orcid_search(given_name = family_given_names[2],family_name = family_given_names[1], affiliation_org = "zurich")




