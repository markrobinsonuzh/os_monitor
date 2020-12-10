#' oa summary ui
#'
#' @param id namesapce
#'
#' @import shiny
#' @export
#'
oaSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("pgb_closed"))
  )
}



#' oa summary server
#' 
#' @param id namespace
#' @param d reactiveValues
#' 
#' @import shiny
#' @import ggplot2
#' @export
#' 
oaSummaryServer <- function(id, d) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({d$m_sub},{
        if(dim(d$m_sub)[1] == 0){
          output$sub_summary <- renderPrint({
            "No publications in selection."
          })
          output$pgb_closed <- renderUI(
            boxPad(color = "teal",
                   # to change the color of "teal"
                   tags$style(HTML(".bg-teal {
                   background-color:#F0F8FF!important;
                   color:#000000!important;
                  }")),
                   h4("Summary filtered data"),
                   descriptionBlock(
                     text = verbatimTextOutput(NS(id, "sub_summary"))
                   )
            )
          )
        } else {
          # summary of subset table
          overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa") %>% as.character()
          output$sub_summary <- renderPrint({
            table(overall_oa_status,useNA = "ifany")
          })
        
          # total number of publications
          tmp_total <- ifelse(!is.numeric(length(overall_oa_status)), 
                              0, 
                              length(overall_oa_status[overall_oa_status!="unknown"]))
          # total number of open publications
          tmp_open <- ifelse(tmp_total != 0, ((tmp_total-sum(overall_oa_status == "closed"))/tmp_total)*100, 0)
          # total number of open publications without blue
          tmp_open_blue <- ifelse(tmp_total != 0,((tmp_total-sum(overall_oa_status %in% c("closed","blue")))/tmp_total)*100, 0)
          # simple summary bar of oa status
          output$oa_summary_histogram_simple <- renderPlot({
            simple_oa_summary_histogram(overall_oa_status)
          },height = 50)
          
        
          output$pgb_closed <- renderUI(
            boxPad(color = "teal",
                   # to change the color of "teal"
                   tags$style(HTML(".bg-teal {
                     background-color:#F0F8FF!important;
                     color:#000000!important;
                    }")),
                   h4("Summary filtered data"),
                   descriptionBlock(
                     text = verbatimTextOutput(NS(id, "sub_summary"))
                   ),
                   plotOutput(NS(id, "oa_summary_histogram_simple"),height = 50),
                   descriptionBlock(
                     text = paste("Percentage open:", 
                                  signif(tmp_open,digits=3),
                                  "%")
                   ),
                   shinydashboardPlus::progressBar(value = tmp_open)#,
                   # descriptionBlock(
                   #   text = paste("Percentage open (without blue):",
                   #                signif(tmp_open_blue,digits=3),
                   #                "%")
                   # ),
                   # shinydashboardPlus::progressBar(value = tmp_open_blue)
            )
          )
        }
      })
    }
  )
}



