#' Retrieve a table of records from orcid.org
#'
#' @param orcid ORCID to retrieve records for
#' @param exclude what type of ORCID records to exclude from the list
#'
#' @return
#' @export
#'
#' @examples
#' mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
retrieve_from_orcid <- function(orcid, exclude = "data-set") {
  works <- rorcid::orcid_works(orcid)
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
  works <- works %>% filter(!(type %in% exclude)) %>%
    mutate(title = title.title.value, journal = `journal-title.value`,
           year = `publication-date.year.value`) %>%
    select(title, journal, type, doi, year)
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
  df <- do.call(rbind,c(z,works_split[n==1]))
  rownames(df) <- NULL
  df$in_orcid <- TRUE
  df
}