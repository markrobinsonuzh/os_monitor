# load required packages
suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
})
on_rstudio <- FALSE
if(on_rstudio){
  setwd("/srv/shiny-server/os_monitor/shiny_app")
  maindir <- file.path(getwd(),"..")
} else {
  setwd("/srv/shiny-server/")
  maindir <- getwd()
}
# functions for backend
devtools::load_all(file.path(maindir,"uzhOS"))
outdir <- file.path(maindir,"data")
con <- dbConnect(odbc::odbc(), "PostgreSQL")

# summary of faculty and department oa status
fac_dep_filt <- all_org_unit_fac(con)
saveRDS(fac_dep_filt, file.path(outdir, "fac_dep_filt.rds"))
