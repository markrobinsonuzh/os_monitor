#' Empty orcid tibble
#' 
#' @importFrom magrittr %>% 
#' @export
empty_orcid <- function(){
  tibble::tibble(title=character(),
                 journal=character(),
                 type=character(),
                 doi=character(),
                 year=integer(),
                 in_orcid=logical()) %>% 
    as_tibble_reac(name="orcid")
}

#' Retrieve a table of records from orcid.org
#'
#' @param orcid ORCID to retrieve records for
#' @param orcid_access_token Access Token for orcid, 
#'  See \code{\link[rorcid]{orcid_auth}}
#' @param exclude what type of ORCID records to exclude from the list
#'
#' @return tibble
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
retrieve_from_orcid <- function(orcid, orcid_access_token="8268867c-bf2c-4841-ab9c-bfeddd582a9c", exclude = "data-set") {
  Sys.setenv(ORCID_TOKEN=orcid_access_token)
  if(!check_if_likely_orcid(orcid)){
    return(empty_orcid())
  }
  works <- tryCatch({rorcid::orcid_works(orcid)},error=function(e) {
    return(NA)
  })
  if (is.na(works)){
    return(empty_orcid())
  }
  works <- tibble::as_tibble(works[[1]]$works)
  if(nrow(works)==0) {
    return(empty_orcid())
  }
  works$doi <- sapply(works$`external-ids.external-id`, 
                      function(u) ifelse(nrow(u)>0, u$`external-id-value`[u$`external-id-type`=="doi"], NA))
  works$doi <- tolower(works$doi)
  works$doi <- gsub("http://dx.doi.org/", "", works$doi)
  works$doi <- gsub("doi: ", "", works$doi)
  works <- works %>% dplyr::filter(!(type %in% exclude)) %>%
    dplyr::mutate(title = title.title.value, 
                  journal = `journal-title.value`,
                  year = `publication-date.year.value`) %>%
    dplyr::select(title, journal, type, doi, year)
  works$title <- sub("\\.$","",works$title)
  works <- unique(works)
  works$doi[works$doi == "logical(0)"] <- NA
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


#' Check for correct structure of orcid (not if exists!)
#'
#' @param orcid potential Orcid
#'
#' @return logical
#' 
#' @export
#'
#' @examples
#' check_if_likely_orcid("0000-0002-3048-551X")
#' check_if_likely_orcid("0000-0002-3048-5511")
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

#' check if orcid is in local db
#'
#' @param orcid orcid
#' @param con db connection
#'
#' @return
#' @importFrom magrittr %>% 
#' @import DBI
#' 
#' @export
#'
#' @examples
#' con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' in_orcid_local("0000-0002-3048-5518",con)
in_orcid_local <- function(orcid, con = NULL){
  if(is.null(con)){
    return(FALSE)
  } else{
    sql_con_cont(con)
    dplyr::tbl(con, "orcids") %>% 
      dplyr::filter(orcid %in% !!orcid) %>% 
      dplyr::collect() %>% 
      nrow() %>% 
      as.logical()
  }
}

# microbenchmark::microbenchmark(
#   check_if_likely_orcid("0000-0002-3048-5518"),
#   in_orcid_local("0000-0002-3048-5518",con),
#   any(tryCatch(rorcid::as.orcid(x = "0000-0002-3048-5518"),error=function(e) "") != "")
# )

# microbenchmark::microbenchmark(
#   in_orcid("0000-0002-3048-55189", con),
#   in_orcid("0000-0002-3048-5518", con),
#   in_orcid("1000-0002-3048-5518", con)
# )


#' check if orcid exists
#'
#' @param orcid orcid
#' @param con db connection
#'
#' @return
#' @export
#'
#' @examples
in_orcid <- function(orcid, con=NULL){
  if(check_if_likely_orcid(orcid)){
    if(in_orcid_local(orcid, con)){
      return(TRUE)
    } else{
      return(any(tryCatch(rorcid::as.orcid(x = orcid),error=function(e) "") != ""))
    }
  } else{
    return(FALSE)
  }
}


