#' Bibtex confirmation modal server
#'
#' @param id namespace
#' @param d reactive values object
#'
#' @import shiny
#' @export
#'
BibtexConfirmationServer <- function(id, d) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$create_bibtex,{
        shiny_print_logs(paste("BibtexConfirmationServer"), d$sps)
        if (!is.null(d$m_sub_sel$doi) && length(d$m_sub_sel$doi) > 0){
          shinyWidgets::ask_confirmation(
            inputId = "create_bibtex_confirmation",
            title = "Confirm Bibtex retrieval",
            text = paste("You are about to retrieve the bibtex entries of ",
                         length(d$m_sub_sel$doi), "publications. This will take approximately",
                         lubridate::seconds(length(d$m_sub_sel$doi)*0.5)," and will run in the background."
            )
          )
        } else {
          shinyWidgets::show_alert(
            title = "Bibtex retrieval error",
            text = "No valid doi's selected from Table. (The Table is likely empty)",
            type = "error"
          )
        }
        
      })
    }
  )
}


#' retrieve from bibtex server module
#'
#' @param id namespace
#' @param d reactive values object
#' @param bibtex_ls \code{\link[shiny]{reactiveVal}}
#'
#' @import shiny
#' @import future
#' @import promises
#' @importFrom magrittr %>% 
#' @export
#'
BibtexRetrieveServer <- function(id, d, bibtex_ls) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$create_bibtex_confirmation,{
        if(input$create_bibtex_confirmation){
          shiny_print_logs(paste("BibtexRetrieveServer"), d$sps)
          shinyjs::disable("create_bibtex")
          to_update <- d$m_sub_sel$doi
          if(length(to_update) > 0) {
            GetBibEntryWithDOI_no_temp(to_update) %...>% 
              bibtex_ls()
          }
        }
      })
    }
  )
}

#' update data on bibtex retrieval server module
#'
#' @param id namespace
#' @param d reactive values object
#' @param bibtex_ls \code{\link[shiny]{reactiveVal}}
#'
#' @import shiny
#' @importFrom magrittr %>% 
#' @export
#'
BibtexObserveResultServer <- function(id, d, bibtex_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(bibtex_ls(),{
        shiny_print_logs(paste("BibtexObserveResultServer"), d$sps)
        req(bibtex_ls())
        showTab("author_plots_tables", "Bibtex")
        shinyjs::enable("create_bibtex")
        output$bibsummary <-  renderText(bibtex_ls())
        shinyjs::show("bibtex_download")
        updateTabsetPanel(session, "author_plots_tables", selected = "Bibtex")
      })
    }
  )
}

#' Download button bibtex citation server module
#'
#' @param id namespace
#' @param bibtex_ls reactive value object
#'
#' @import shiny
#' @export
#'
BibtexDownloadButtonServer <- function(id, bibtex_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$bibtex_download <- downloadHandler(
        filename = function() {paste0("Bibtex_selection.bib")},
        content = function(file){
          writeLines( paste(bibtex_ls(),collapse = "\n"),file)
        }
      )
    }
  )
}