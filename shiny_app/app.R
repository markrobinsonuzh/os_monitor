# startup script for shiny app zora
setwd("/srv/shiny-server/")
maindir <- getwd()
# devtools::load_all(file.path(maindir,"uzhOS"))
con <-  odbc::dbConnect(odbc::odbc(),"PostgreSQL")
fac_dep_filt <- tryCatch(readRDS(file.path(file.path(maindir,"data"), "fac_dep_filt.rds")),
                         error = function(e){ uzhOS::all_org_unit_fac(con)})

uzhOS::shinyApp_zora(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"), 
              fac_dep_filt = fac_dep_filt,
              docfile = "/srv/shiny-server/uzhOS/inst/extdata/helpfiles/OA_monitor_documentation.Rmd")

# setwd("/srv/shiny-server/os_monitor")
# maindir <- getwd()
# # devtools::load_all(file.path(maindir,"uzhOS"))
# con <-  odbc::dbConnect(odbc::odbc(),"PostgreSQL")
# fac_dep_filt <- tryCatch(readRDS(file.path(file.path(maindir,"data"), "fac_dep_filt.rds")),
#                          error = function(e){ uzhOS::all_org_unit_fac(con)})
# 
# uzhOS::shinyApp_zora(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"), 
#                      fac_dep_filt = fac_dep_filt,
#                      docfile = "/srv/shiny-server/os_monitor/uzhOS/inst/extdata/helpfiles/OA_monitor_documentation.Rmd")
