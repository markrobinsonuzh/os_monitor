#' get fulltext pdflink from sci-hub of doi
#'
#' @param doi doi
#' @param sci_hub_base_url base url sci-hub
#'
#' @return url
#' 
#' @importFrom magrittr %>% 
#' @import httr
#' @export
#'
#' @examples
#' orcid <- "0000-0002-3048-5518"
#' ws <- retrieve_from_orcid(orcid) %>%
#'   mutate(doi = tolower(doi))
#'   closed_dois <- oaf %>% dplyr::slice(1)%>% pull(doi)
#' fns <- pdf_link_from_scihub_singledoi(ws)
pdf_link_from_scihub_singledoi <- function(doi, sci_hub_base_url = "https://sci-hub.se/") {
  stopifnot(length(doi)==1)
  sci_url <- httr::parse_url(sci_hub_base_url)
  sci_url$scheme <- "https"
  sci_url$path <- doi
  z <- httr::build_url(sci_url) %>% 
    httr::GET()
  if (httr::http_error(z)){
    return("")
  }
  z_cont <- httr::content(z)
  if (is.null(z_cont)){
    return("")
  }
  n <- rvest::html_node(z_cont, xpath = '//*[@id="article"]')
  n <- as.character(n)
  
  # hacky parse of html
  fn <- strsplit(strsplit(n, 'iframe src=\"')[[1]][2], "#view")[[1]][1]
  fn <- gsub("^https://", "", fn)
  fn <- gsub("^/", "", fn)
  fn <- gsub("^/", "", fn)
  if(is.na(fn)) return("")
  paste0("https://", fn)
}


#' get fulltext pdflink from sci-hub of multiple dois
#'
#' @param dois dois
#' @param sci_hub_base_url base url sci-hub
#'
#' @return vector of urls
#' @export
#'
#' @examples
#'  orcid <- "0000-0002-3048-5518"
#' ws <- retrieve_from_orcid(orcid) %>%
#'   mutate(doi = tolower(doi))
#'   closed_dois <- ws %>% dplyr::slice(1:5)%>% pull(doi)
#' pdf_link_from_scihub(closed_dois)
pdf_link_from_scihub <- function(dois, sci_hub_base_url = "https://sci-hub.se/"){
  sapply(dois, function(doi){pdf_link_from_scihub_singledoi(doi,sci_hub_base_url)} )
}


#' create DT of scihub data
#'
#' @param m_sub_sel_closed_pdflink data.frame
#'
#' @return \code{\link[DT]{datatable}}
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' m_sub_sel_closed_pdflink <- tibble::tibble(pdflink=1:10,
#'                                            doi=1:10)
#' uzhOS::sci_hub_datatable(m_sub_sel_closed_pdflink)
sci_hub_datatable <- function(m_sub_sel_closed_pdflink){
  m_sub_sel_closed_pdflink %>% 
    dplyr::mutate(pdflink = paste0("<a href='",pdflink,"' target='_blank'>", pdflink, "</a>"),
                  doi = paste0("<a href='https://www.doi.org/",doi, "' target='_blank'>", doi, "</a>")) %>% 
    DT::datatable(extensions = 'Buttons',
                  options = list(dom = 'Blfrtip',
                                 # pageLength = 200,
                                 buttons = list('copy', 'csv', 'excel')),
                  escape = FALSE, rownames = FALSE)
}
