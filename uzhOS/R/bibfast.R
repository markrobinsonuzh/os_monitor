#' Get bib entry
#'
#' @param doi vector of doi's
#' @param progress \code{\link[shiny]{Progress}} object
#'
#' @return vector of bib entries as vector
#' @export
#'
#' @examples
#' doi <- "10.1177/000271625529700159"
#' GetBibEntryWithDOI_no_temp(doi,progress=NULL)
GetBibEntryWithDOI_no_temp <- function (doi, progress = shiny::Progress$new()) 
{
  if (!is.null(progress)){
    progress$set(message = "Retrieving bibtex", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
  }
  bibout <- sapply(seq_along(doi), function(i){
    if (!is.null(progress)) {
      tmpval <- progress$getValue()
      progress$set( value = tmpval + 1/length(doi), 
                                          message=paste("entry number", (tmpval+1/length(doi))*length(doi)))
    }
    
    # temp <- GET(modify_url("https://doi.org/", path = doi[i]), 
                # config = list(followlocation = TRUE), add_headers(Accept = "application/x-bibtex"))
    temp <- GET(modify_url("https://data.crossref.org/", path = doi[i]), 
                config = list(followlocation = TRUE), add_headers(Accept = "application/x-bibtex"))
    return(content(temp, as = "text", encoding = "UTF-8"))
  })
    return(bibout)
  }

# library(httr)
# doi <- "10.1177/000271625529700159"
# dois <- rep(doi,20)
# system.time({
#   tmp1 <- GetBibEntryWithDOI(dois)
#   writeLines(toBiblatex(tmp1),"/srv/shiny-server/os_monitor/bibtest_normal.bib")
#   
# })
# system.time({
#   tmp2 <- GetBibEntryWithDOI_no_temp(dois)
#   writeLines( paste(tmp2,collapse = "\n"),"/srv/shiny-server/os_monitor/bibtest_fast.bib")
# })
#   
# system.time({
#   rcrossref::cr_cn(dois = dois, format = "bibtex", "bibentry") 
# })
#   
# 
# temp <- GET(modify_url("https://data.crossref.org/", path = doi[i]), 
#             config = list(followlocation = TRUE), add_headers(Accept = "application/x-bibtex"))
# content(temp, as = "text", encoding = "UTF-8")
# toBiblatex(tmp)
# 
# bibtex_from_doi <- GetBibEntryWithDOI(doi)
# print("all found")
# # toBiblatex(bibtex_from_doi)
# writeLines(toBiblatex(bibtex_from_doi),"/srv/shiny-server/os_monitor/bibtest_normal.bib")
# print("all written")
# getwd()




