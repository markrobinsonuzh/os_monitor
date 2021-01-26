#' retrieve metadata using zotero translator container
#'
#' @param url url to get metadata from
#' @param zotero_container_name docker container name where the zotero translater is running
#'  and exposing port 1969. If run locally you can use 'http://127.0.0.1'
#'
#' @return data frame of varying entries 
#' @export
#' @importFrom magrittr %>% 
#' @examples
#' metadata_from_url("https://www.nature.com/articles/nprot.2013.099.pdf?origin=ppub")
metadata_from_url <- function(url, zotero_container_name = "translation-server"){
  tryCatch({
    glue::glue("curl -d '{url}'    -H 'Content-Type: text/plain' {zotero_container_name}:1969/web") %>% 
      system(intern = TRUE,
             ignore.stderr = TRUE) %>% 
      jsonlite::fromJSON() %>% 
      tibble::as_tibble()
    },
  error=function(e) tibble::tibble()
  )
}


#' extract DOI field from metadata
#'
#' @param metadata tibble containing metadata
#'
#' @return tibble with column DOI
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
doi_from_metadata <- function(metadata){
  names(metadata) <- toupper(names(metadata))
  doi_ind <- which(names(metadata) == "DOI")
  if(length(doi_ind) == 0){
    return(tibble::tibble(DOI=character(0)))
  } else if (length(doi_ind) == 1){
    return(metadata[ ,doi_ind] %>% dplyr::mutate(DOI=tolower(DOI)))
  } else {
    stop(call. = FALSE, "More than one column with name 'doi' in function 'get_doi_from_metadata'!")
  }
}


#' get url of publications for a google scholar cid
#'
#' @param cid publication identifier
#' @param all logical, if FALSE only the first page in Google scholar is browsed, if
#'  TRUE all pages are searched
#' @param handle handle
#' @param flush delete cache
#'
#' @return character vector of urls
#' @export
#' @importFrom magrittr %>% 
#' @examples
#' get_urls_from_scholar_cid("7747102342361266349")
get_urls_from_scholar_cid <- function(cid, all=FALSE, 
                                      handle = httr::GET("https://scholar.google.com/citations?user=B7vSqZsAAAAJ"),
                                      flush = FALSE){
  cache.dir <- file.path(tempdir(), "r-scholar-publications")
  R.cache::setCacheRootPath(cache.dir)
  # get first page of google scholar
  pg_1 <- glue::glue("https://scholar.google.com/scholar?oi=bibs&hl=de&cluster={cid}")
  if (flush) {
    R.cache::saveCache(NULL, key = list(pg_1))
  }
  html_resp <- R.cache::loadCache(list(pg_1))
  if (is.null(html_resp)){
    html_resp <- httr::GET(pg_1, handle = handle)
    R.cache::saveCache(html_resp, key = list(pg_1))
  }
 
  urls <- NULL
  # get urls
  filelist <- html_resp %>%
    httr::content() %>% 
    rvest::html_nodes(xpath = '//*[@id="gs_res_ccl_mid"]') %>% 
    xml2::xml_children()
  if(length(filelist) == 0){
    stop(.call = glue::glue("No results for cid {cid} found using 'get_urls_from_scholar_cid'!"))
  }
  urls <- purrr::map_chr(seq_along(filelist), ~{
    filelist %>% 
      xml2::xml_child(.x) %>% 
      xml2::xml_child(1) %>% 
      xml2::xml_child(1) %>% 
      xml2::xml_attr("href")
    })
  urls <- urls[!is.na(urls)]
  if(!all && length(urls)>0){
    return(urls)
  }
  # if want to retrieve all urls or none found on first page
  if(all || length(urls)==0){
    # get number urls for that publication
    lenchr <- html_resp %>%
      httr::content() %>% 
      rvest::html_nodes(xpath = '/html/body/div/div[9]/div[3]/div') %>% 
      xml2::xml_contents() %>% 
      xml2::as_list()
    nr_pages <- as.integer(stringr::str_extract(lenchr[[1]], "[:digit:]+"))/10 %>% 
      ceiling()
    urls <- c(urls,purrr::map(seq_len(nr_pages)[-1], ~ {
      # get first page of google scholar
      pg_2 <- glue::glue("https://scholar.google.com/scholar?start={(.x-1)*10}&hl=de&as_sdt=0,5&cluster={cid}")
      if (flush) {
        R.cache::saveCache(NULL, key = list(pg_2))
      }
      html_resp <- R.cache::loadCache(list(pg_2))
      if (is.null(html_resp)){
        html_resp <- httr::GET(pg_2, handle = handle)
        R.cache::saveCache(html_resp, key = list(pg_2))
      }
      
      filelist <- html_resp %>%
        httr::content() %>% 
        rvest::html_nodes(xpath = '//*[@id="gs_res_ccl_mid"]') %>% 
        xml2::xml_children()
      urls <- purrr::map_chr(seq_along(filelist), ~{
        filelist %>% 
          xml2::xml_child(.x) %>% 
          xml2::xml_child(1) %>% 
          xml2::xml_child(1) %>% 
          xml2::xml_attr("href")
      })
      urls[!is.na(urls)]
    }) %>% purrr::reduce(c))
  }
  return(urls)
}
  


#' Wrapper to get doi from google scholar cid
#'
#' @param cid publication identifier
#' @param handle handle
#' @param ... additional arguments to \code{\link{doi_from_metadata}} 
#'
#' @return tibble with column DOI
#' @export
#'
#' @examples
#' doi_from_cid("7747102342361266349")
doi_from_cid <- function(cid, handle = httr::GET("https://scholar.google.com/citations?user=B7vSqZsAAAAJ"), ...){
  urls_gs <- get_urls_from_scholar_cid(cid, handle = handle)
  for(i in seq_along(urls_gs)){
    meta_gs <- metadata_from_url(urls_gs[i])
    doi <- doi_from_metadata(meta_gs, ...)
    if(!is.null(doi)){
      return(doi)
    }
  }
  urls_gs_all <- get_urls_from_scholar_cid(cid, all = TRUE)
  for(i in seq_along(urls_gs_all)[-seq_along(urls_gs)]){
    meta_gs <- metadata_from_url(urls_gs[i], ...)
    doi <- doi_from_metadata(meta_gs)
    if(!is.null(doi)){
      return(doi)
    }
  }
  return(tibble::tibble(DOI=character(0)))
}






#' search for missing dois in google scholar using zotero
#'
#' @param df_scholar tibble from \code{\link{retrieve_from_scholar}}
#' @param handle handle
#' 
#' @return
#' @export
#'
#' @examples
#' df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
#' search_doi_in_scholar_using_zotero(df_scholar[1:2,])
search_doi_in_scholar_using_zotero <- function(df_scholar, 
                                               handle = httr::GET("https://scholar.google.com/citations?user=B7vSqZsAAAAJ")){
  if(!("doi" %in% names(df_scholar))){
    df_scholar$doi <- NA
  }
  doi_is_na <- which(is.na(df_scholar$doi))
  cids <- df_scholar[doi_is_na,][["cid"]]
  for(i in seq_along(cids)){
    if(!is.na(cids[i])){
      one_cid <- stringr::str_split(cids[i],",")[[1]]
      dois <- purrr::map(one_cid, ~{
        tryCatch(doi_from_cid(.x, handle = handle), error=function(e) tibble::tibble(DOI=character(0)))
      }) %>% purrr::reduce(rbind)
      if(dim(dois)[1] > 1){
        dois <- dois[1, ]
      }
      if(dim(dois)[1] == 1){
        df_scholar$doi[doi_is_na[i]] <- dois$DOI
      }
    }
  }
  return(df_scholar)
}

# scholar::set_scholar_mirror()
# options("scholar_site")
# sample_url <- paste0(options("scholar_site"), "/citations?user=B7vSqZsAAAAJ")
# scholar_handle <- httr::GET("https://scholar.google.com/citations?user=B7vSqZsAAAAJ")
# 
# resp <- httr::GET(url, handle = scholar_handle)
# 
# scholar::scholar_handle()
# 
# sh <- scholar::get_scholar_resp("scholar.google.com")
# 
# devtools::install_github("jkeirstead/scholar")
# df_scholar <- uzhOS::retrieve_from_scholar("XPfrRQEAAAAJ")
# out <- uzhOS::search_doi_in_scholar_using_zotero(df_scholar[1:2,])
