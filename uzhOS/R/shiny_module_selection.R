#' UI module for 'in selection'
#'
#' @param id for namespace
#'
#' @return taglist of checkboxgroupinput
#' @export
#' @import shiny
#' @importFrom magrittr %>% 
#'
#' @examples
in_selection_UI <- function(id) {
  ns <- NS(id)
  tagList(
    splitLayout(
      checkboxGroupInput(ns("bib_in_selection"),label = "In","",inline = TRUE) %>% 
        shinyjs::hidden(),
      checkboxGroupInput(ns("bib_not_in_selection"),label = "Not in","",inline = TRUE) %>% 
        shinyjs::hidden()
    )
  )
}


#' Server module for 'in selection' to update checkbosgroupinput
#'
#' @param id 
#' @param m 
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
in_selection_Server <- function(id,m) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyjs::show(id = "bib_in_selection")
      updateCheckboxGroupInput(session,"bib_in_selection",
                               choices =  names(m)[grep("in_",names(m))],
                               selected = names(m)[grep("in_",names(m))][1])
      shinyjs::show(id = "bib_not_in_selection")
      updateCheckboxGroupInput(session,"bib_not_in_selection",
                               choices =  names(m)[grep("in_",names(m))],
                               selected = names(m)[grep("in_",names(m))][1])
    }
  )
}

#' Server module for 'in selection' to return selected rows
#'
#' @param id for namespace
#' @param m tbl_merge
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
in_selection_bib_Server <- function(id,m) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        if (!is.null(input$bib_in_selection) & !is.null(input$bib_not_in_selection)){
          ind <- upset_selection(m,input$bib_in_selection,input$bib_not_in_selection)
          to_update <- m[ind,] %>% dplyr::pull(doi)
          return(to_update)
        } else {
          return(NULL)
        }
      })
    }
  )
}