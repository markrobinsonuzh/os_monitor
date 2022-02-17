#' Get bib entry
#'
#' @param doi vector of doi's
#'
#' @return future, which will return list of bib entries
#' @export
#' @import shiny
#' @import promises 
#' @import future
#'
#' @examples
#' doi <- "10.1177/000271625529700159"
#' out <- GetBibEntryWithDOI_no_temp(doi)
#' future::value(out)
GetBibEntryWithDOI_no_temp <- function (doi) 
{
  future(seed=NULL,{
    outls <- sapply(seq_along(doi), function(i){
      temp <- httr::GET(httr::modify_url("https://doi.org/", path = doi[i]),
                        config = list(followlocation = TRUE), httr::add_headers(Accept = "application/x-bibtex", mailto = "retogerber93@gmail.com"))
      if(httr::status_code(temp) == 404){
        return(NULL)
      } else if(httr::status_code(temp) == 503){
        RefManageR::GetBibEntryWithDOI(doi)
      } else {
        return(httr::content(temp, as = "text", encoding = "UTF-8"))
      }
    })
    unlist(outls[!sapply(outls,is.null)])
  })
}





