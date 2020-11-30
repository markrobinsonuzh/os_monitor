# startup script for shiny app zora
setwd("/srv/shiny-server/")
maindir <- getwd()
# devtools::load_all(file.path(maindir,"uzhOS"))
con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
fac_dep_filt <- tryCatch(readRDS(file.path(file.path(maindir,"data"), "fac_dep_filt.rds")),
                         error = function(e){ uzhOS::all_org_unit_fac(con)})

uzhOS::shinyApp_zora(con = con, 
              fac_dep_filt = fac_dep_filt)
