#' Shiny app for open access comparison of a researcher
#'
#' @param con db connection function call, e.g. odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' @param orcid_access_token Access Token for orcid, 
#'  See \code{\link[rorcid]{orcid_auth}}
#' @param docfile filename of '.Rmd' documentation file
#' @param future_plan \code{\link[future]{plan}}, e.g. future::plan(multisession, workers=2)
#'   
#' @return shiny.appobj
#' 
#' @export
#' @importFrom magrittr %>% 
#' @import shiny 
#' @import promises 
#' @import future 
#' 
#' @details 
#' ## Orcid access Token
#'  For access to the orcid API a Token needs to be generated. For details see 
#'  \code{\link[rorcid]{orcid_auth}}. Or in short, just run `rorcid::orcid_auth()`
#'  to get a Token.
#' 
#' ## Database
#'  Argument 'con' requires a function call to the database connection and not just an
#'  database object. E.g. do
#'  ```
#'  shinyApp_general(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"))
#'  ```
#'  and NOT!:
#'  ```
#'  con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#'  shinyApp_general(con = con)
#'  ```
#'  
#'  the reason is the use of \code{\link[future]{future}} where the database connection 
#'  has to be newly generated within the new session.
#'  
#'  The database itself is derived from a [unpaywall snapshot](https://unpaywall.org/products/snapshot)
#'  with just two columns: 'doi' and 'oa_status'. The unpaywall data table is expected to have the name
#'  'unpaywall'.
#'  
#'  
#' @examples
#' shinyApp_general()
shinyApp_general <- function(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"),
                             orcid_access_token = "8268867c-bf2c-4841-ab9c-bfeddd582a9c",
                             docfile = file.path(system.file("extdata","helpfiles",package = "uzhOS"),"OA_monitor_documentation.Rmd"),
                             future_plan=plan(multisession,workers=10)){
  con_quosure <- rlang::enquo(con)
  future_plan
  print("future plan:")
  print(plan())
  # token to get acces to orcid (currently Reto's token)
  Sys.setenv(ORCID_TOKEN=orcid_access_token)
  
  message("Render Documentation page")
  #savedir <- "/srv/shiny-server/uzhOS/inst/extdata/helpfiles"
  savedir <- dirname(docfile)
  mdfile <- file.path(savedir,"OA_monitor_documentation.md")
  knitr::knit(docfile, mdfile)
  message("Start application ...")
  shinyApp(ui = shiny_general_ui(docfile=mdfile), 
           server = shiny_general_server(con=con_quosure, orcid_access_token=orcid_access_token))
}
