#' UI module Orcid input
#'
#' @param id namespace
#'
#' @import shiny
#' @export
inputOrcidUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("orcid"),
              label = a("Orcid",
                        href="https://orcid.org",
                        target="_blank"), 
              value="0000-0002-3048-5518")
  )
}

#' UI module Pubmed input
#'
#' @param id namespace
#'
#' @import shiny
#' @export
inputPubmedUI <- function(id) {
  ns <- NS(id)
  tagList(
    splitLayout(cellWidths = c("75%", "25%"),
                textAreaInput(ns("pubmed"),
                              label = a("Pubmed Query",
                                        href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",
                                        target="_blank"), 
                              value="") %>% 
                  shinyjs::disabled(),
                actionButton(ns("activate_pubmed"),
                             HTML("Generate <br/> Pubmed <br/> Query")))
  )
}

#' UI module Scholar input
#'
#' @param id namespace
#'
#' @import shiny
#' @export
inputScholarUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("scholar"),
              label = a("Google Scholar id",
                        href="https://scholar.google.ch",
                        target="_blank"), 
              value="") %>% 
      shinyhelper::helper(type="markdown",
                          title = "Google scholar id help",
                          content = 'Google_scholar_help'),
  )
}

#' UI module Publons input
#'
#' @param id namespace
#'
#' @import shiny
#' @export
inputPublonsUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("publons"),
              label = tags$div(tags$a("Publons id",
                                      href="https://publons.com",target="_blank"),
                               tags$span(class="help-block","(or if linked: ORCID, ResearcherID or TRUID)")), 
              value="")
  )
}
