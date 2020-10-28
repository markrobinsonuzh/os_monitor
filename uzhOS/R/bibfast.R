#' Get bib entry
#'
#' @param doi vector of doi's
#' @param progress \code{\link[shiny]{Progress}} object
#'
#' @return vector of bib entries as vector
#' @export
#' @import shiny
#'
#' @examples
#' doi <- "10.1177/000271625529700159"
#' GetBibEntryWithDOI_no_temp(doi,progress=NULL)
GetBibEntryWithDOI_no_temp <- function (doi, progress = shiny::Progress$new()) 
{
  # if (!is.null(progress)){
  #   progress$set(message = "Retrieving bibtex", value = 0)
  #   # Close the progress when this reactive exits (even if there's an error)
  #   on.exit(progress$close())
  # }
  future(seed=NULL,{
    bibout <- sapply(seq_along(doi), function(i){
      # if (!is.null(progress)) {
      #   tmpval <- progress$getValue()
      #   progress$set( value = tmpval + 1/length(doi), 
      #                                       message=paste("entry number", (tmpval+1/length(doi))*length(doi)))
      # }
      
      # temp <- GET(modify_url("https://doi.org/", path = doi[i]), 
      # config = list(followlocation = TRUE), add_headers(Accept = "application/x-bibtex"))
      temp <- httr::GET(httr::modify_url("https://data.crossref.org/", path = doi[i]), 
                        config = list(followlocation = TRUE), httr::add_headers(Accept = "application/x-bibtex", mailto = "retogerber93@gmail.com"))
      return(httr::content(temp, as = "text", encoding = "UTF-8"))
    })
  })

    # return(bibout)
}





