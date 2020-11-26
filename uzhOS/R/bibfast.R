#' Get bib entry
#'
#' @param doi vector of doi's
#'
#' @return future, which will return list of bib entries
#' @export
#' @import shiny
#'
#' @examples
#' doi <- "10.1177/000271625529700159"
#' GetBibEntryWithDOI_no_temp(doi,progress=NULL)
GetBibEntryWithDOI_no_temp <- function (doi) 
{
  future(seed=NULL,{
    bibout <- sapply(seq_along(doi), function(i){
      temp <- httr::GET(httr::modify_url("https://data.crossref.org/", path = doi[i]), 
                        config = list(followlocation = TRUE), httr::add_headers(Accept = "application/x-bibtex", mailto = "retogerber93@gmail.com"))
      return(httr::content(temp, as = "text", encoding = "UTF-8"))
    })
  })
}





