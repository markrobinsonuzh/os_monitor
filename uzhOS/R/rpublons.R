#' @importFrom magrittr %>% 
#' @export
empty_publons <- function(){
  tibble::tibble(doi=character(),
                 title=character(),
                 date=character(),
                 year=integer(),
                 in_publons=logical()) %>% 
    as_tibble_reac(name="publons")
}

#' retrieve from publons
#'
#' @param id either publons ID, ORCID, ResearcherID or TRUID
#' @param token for access on publons
#'
#' @return data.frame with columns doi, title, date, year
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' orcid <- "0000-0002-3048-5518"
#' publonsid <- "A-6432-2015"
#' retrieve_from_publons(orcid)
retrieve_from_publons <- function(id,token="a8850f6014654476058d29dbf5a42b2b20db8b38"){
  if (in_publons(id,token)){
    auth_header <- httr::add_headers(Authorization = paste0("Token ", token))
    
    publget <- httr::GET(url=paste0("https://publons.com/api/v2/academic/publication/?academic=",id),auth_header)
    httr::stop_for_status(publget)
    df_publons <- tibble::tibble(doi=character(),
                             title=character(),
                             date=character())
    publls <- httr::content(publget, encoding = "UTF-8")
    while (TRUE) {
      tmp_df_publons <- lapply(publls[[3]], function(x) 
        data.frame(doi=x$publication$ids$doi,
                   title=x$publication$title,
                   date=x$publication$date_published)) %>% 
        purrr::reduce(rbind)
      df_publons <- rbind(df_publons,tmp_df_publons)
      if (is.null(publls[["next"]])) break
      publget <- httr::GET(url=publls[["next"]],auth_header)
      httr::stop_for_status(publget)
      publls <- httr::content(publget, encoding = "UTF-8")
    }
    
    df_publons$year <- stringr::str_extract(df_publons$date,"[:digit:]{4}") %>% 
      as.integer()
    df_publons$in_publons <- TRUE
    return(tibble::as_tibble(df_publons))
  } else {
    return(empty_publons())
  }

}


#' get ids from publons
#'
#' @param id either publons ID, ORCID, ResearcherID or TRUID
#' @param token token for access on publons
#'
#' @return data.frame with columns publons_id, orcid, rid and truid
#' @export
#'
#' @examples
#' orcid <- "0000-0002-3048-5518"
#' get_ids_from_publons(orcid)
get_ids_from_publons <- function(id,token="a8850f6014654476058d29dbf5a42b2b20db8b38"){
  if (in_publons(id,token)){
    auth_header <- httr::add_headers(Authorization = paste0("Token ", token))
    publget <- httr::GET(url=paste0("https://publons.com/api/v2/academic/",id),auth_header)
    httr::stop_for_status(publget)
    publls <- httr::content(publget)
    return(data.frame(publonsid=publls$ids$id,
               orcid=publls$ids$orcid,
               rid=publls$ids$rid,
               truid=publls$ids$truid))
  } else {
    return(data.frame(publonsid=character(),
                      orcid=character(),
                      rid=character(),
                      truid=character()))
  }
  

}


#' check if id in publons
#'
#' @param id either publons ID, ORCID, ResearcherID or TRUID
#' @param token token for access on publons
#'
#' @return logical
#' @export
#'
#' @examples
#' orcid <- "0000-0002-3048-5518"
#' in_publons(orcid)
in_publons <- function(id,token="a8850f6014654476058d29dbf5a42b2b20db8b38"){
  auth_header <- httr::add_headers(Authorization = paste0("Token ", token))
  publget <- httr::GET(url=paste0("https://publons.com/api/v2/academic/",id),auth_header)
  return(!httr::http_error(publget))
}


