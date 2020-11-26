# startup script for shiny app zora
setwd("/srv/shiny-server/")
maindir <- getwd()
devtools::load_all(file.path(maindir,"uzhOS"))
fac_dep_filt <- tryCatch(readRDS(file.path(file.path(maindir,"data"), "fac_dep_filt.rds")),
                         error = function(e){ all_org_unit_fac(con)})

shinyApp_zora(con = dbConnect(odbc::odbc(), "PostgreSQL"), 
              fac_dep_filt = fac_dep_filt)
