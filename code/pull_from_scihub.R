
library(rvest)
library(rorcid)
library(dplyr)
library(roadoi)


retrieve_from_orcid <- function(orcid) {
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
  works <- works %>% filter(type != "data-set") %>%
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
  df
}


pdf_link_from_scihub <- function(doi, sci_hub_base_url = "https://sci-hub.se/") {
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
  n <- html_node(z_cont, xpath = '//*[@id="article"]')
  n <- as.character(n)
  
  # hacky parse of html
  fn <- strsplit(strsplit(n, 'iframe src=\"')[[1]][2], "#view")[[1]][1]
  fn <- gsub("^https://", "", fn)
  fn <- gsub("^/", "", fn)
  fn <- gsub("^/", "", fn)
  if(is.na(fn)) return("")
  paste0("https://", fn)
}

orcid <- "0000-0002-3048-5518"

ws <- retrieve_from_orcid(orcid) %>%
  mutate(doi = tolower(doi))

oaf <- oadoi_fetch(unique(na.omit(ws$doi)),
                   email="mark.robinson@mls.uzh.ch")

closed_dois <- oaf %>% dplyr::filter(oa_status=="closed") %>% pull(doi)

fns <- sapply(closed_dois, pdf_link_from_scihub)
