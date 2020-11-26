#' Shiny app 
#'
#' @param con db connection
  
#' @return
#' 
#' @export
#' @import shiny 
#' @import dplyr 
#' @import ggplot2 
#' @import stringr 
#' @import shinyjs 
#' @import plotly 
#' @import DBI 
#' @import shinyTree 
#' @import shinyWidgets 
#' @import promises 
#' @import future 
#' @import shinydashboard 
#' @import shinydashboardPlus
#'
#' @examples
#' shinyApp_general()
shinyApp_general <- function(con = dbConnect(odbc::odbc(), "PostgreSQL"),
                          fac_dep_filt = NULL){
  plan(multisession)
  require("shinyTree")  
  
  # token to get acces to orcid (currently Reto's token)
  Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")
  
  message("Start application ...")
  shinyApp(ui = shiny_general_ui(), 
           server = shiny_general_server(con=con))
}
