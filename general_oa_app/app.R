# startup script for shiny app zora
# setwd("/srv/shiny-server/")
# maindir <- getwd()
# devtools::load_all(file.path(maindir,"uzhOS"))
uzhOS::shinyApp_general(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"), docfile = "/srv/shiny-server/uzhOS/inst/extdata/helpfiles/OA_monitor_documentation.Rmd")


# uzhOS::shinyApp_general(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"), docfile = "/srv/shiny-server/os_monitor/uzhOS/inst/extdata/helpfiles/OA_monitor_documentation.Rmd")
# devtools::install("uzhOS/", update="never")
