#' Shiny app including zora
#'
#' @param con db connection
#' @param fac_dep_filt tibble from \code{\link{all_org_unit_fac}}, if NULL (default), will
#'  be recomputed which increases startup time
#' @param orcid_access_token Access Token for orcid, 
#'  See \code{\link[rorcid]{orcid_auth}}
#' @param docfile filename of '.Rmd' documentation file
#' @param future_plan \code{\link[future]{plan}}, e.g. future::plan(multisession, workers=2)
#'   
#' @return shiny.appobj
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
#' @import magrittr
#' @import odbc
#' @examples
#' shinyApp_zora()
shinyApp_zora <- function(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"),
                          fac_dep_filt = NULL,
                          orcid_access_token = "8268867c-bf2c-4841-ab9c-bfeddd582a9c",
                          docfile = file.path(system.file("extdata","helpfiles",package = "uzhOS"),"OA_monitor_documentation.Rmd"),
                          future_plan=plan(multisession,workers=10)){
  con_quosure <- rlang::enquo(con)
  future_plan
  print("future plan:")
  print(plan())
  
  message("Render Documentation page")
  #savedir <- "/srv/shiny-server/uzhOS/inst/extdata/helpfiles"
  savedir <- dirname(docfile)
  mdfile <- file.path(savedir,"OA_monitor_documentation.md")
  knitr::knit(docfile, mdfile)
  
  require("shinyTree")  

  # token to get acces to orcid (currently Reto's token)
  Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")
  
  message("Get unique author keys ...")
  unique_authorkeys_processed <- dplyr::tbl(con, "authorkeys") %>%
    dplyr::pull(authorname)
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
  
  
  # summary of faculty and department oa status
  if(is.null(fac_dep_filt)){
    message("Get summary of faculties ...")
    fac_dep_filt <- all_org_unit_fac(con)
  }
  
  # all oa status in data
  all_oa_status <- unique(fac_dep_filt$oa_status)
  mat_oa <- match(all_oa_status,names(open_cols_fn()))
  ord_oa <- order(mat_oa)
  all_oa_status <- names(open_cols_fn())[mat_oa[ord_oa]]
  
  message("Get faculty hierarchy ...")
  orgfile <- system.file("data","orgtree.RData", package = "uzhOS")
  load(file=orgfile)
  options(shinyTree.defaultParser="tree")
  
  message("Start application ...")
  shinyApp(ui = shiny_zora_ui(fac_dep_filt=fac_dep_filt, docfile=mdfile), 
           server = shiny_zora_server(con=con_quosure,
                                      unique_authorkeys_processed=unique_authorkeys_processed,
                                      all_oa_status=all_oa_status,
                                      orgtree=orgtree,
                                      fac_dep_filt=fac_dep_filt,
                                      orcid_access_token=orcid_access_token))
}
