#' Shiny app for open access
#'
#' @param con db connection
#' @param orcid_access_token Access Token for orcid, 
#'  See \code{\link[rorcid]{orcid_auth}}
#'   
#' @return shiny.appobj
#' 
#' @export
#' @importFrom magrittr %>% 
#' @import shiny 
#' @import promises 
#' @import future 
#'
#' @examples
#' shinyApp_general()
shinyApp_general <- function(con = dbConnect(odbc::odbc(), "PostgreSQL"),
                             orcid_access_token = "8268867c-bf2c-4841-ab9c-bfeddd582a9c"){
  plan(multisession)
  
  # token to get acces to orcid (currently Reto's token)
  Sys.setenv(ORCID_TOKEN=orcid_access_token)
  
  message("Start application ...")
  shinyApp(ui = shiny_general_ui(), 
           server = shiny_general_server(con=con, orcid_access_token=orcid_access_token))
}
