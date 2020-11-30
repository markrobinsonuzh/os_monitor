# startup script for shiny app zora
# setwd("/srv/shiny-server/")
# maindir <- getwd()
# devtools::load_all(file.path(maindir,"uzhOS"))
uzhOS::shinyApp_general(con = odbc::dbConnect(odbc::odbc(), "PostgreSQL"))
