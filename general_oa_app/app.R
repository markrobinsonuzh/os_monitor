# startup script for shiny app zora
setwd("/srv/shiny-server/")
maindir <- getwd()
devtools::load_all(file.path(maindir,"uzhOS"))
shinyApp_general(con = dbConnect(odbc::odbc(), "PostgreSQL"))