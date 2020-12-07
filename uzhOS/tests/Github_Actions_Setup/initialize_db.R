library(RPostgres)
library(odbc)
library(DBI)
library(dplyr)
library(magrittr)
# Connect to a specific postgres database i.e. Heroku
# con <- dbConnect(RPostgres::Postgres(),
#                  dbname = 'oa', 
#                  host = process.env.POSTGRES_HOST, # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
#                  port = process.env.POSTGRES_PORT, # or any other port specified by your DBA
#                  user = 'shiny',
#                  password = 'postgres')
con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")

sample_tbls <- readRDS("data/sample_tbls.rds")

authorstablename = "authors"
authorkeystablename = "authorkeys"
eprintstablename = "eprints"
subjectstablename = "subjects"
unpaywalltablename = "unpaywall"

dbWriteTable(con, authorstablename, sample_tbls$tbl_author)
dbWriteTable(con, authorkeystablename, sample_tbls$tbl_authorkeys)
dbWriteTable(con, eprintstablename, sample_tbls$tbl_eprints)
dbWriteTable(con, subjectstablename, sample_tbls$tbl_subjects)
dbWriteTable(con, unpaywalltablename, sample_tbls$tbl_unpaywall)

print(tbl(con, unpaywalltablename) %>% collect())
# Disconnect from the database
dbDisconnect(con)