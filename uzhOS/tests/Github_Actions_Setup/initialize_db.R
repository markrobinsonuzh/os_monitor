library(odbc)
library(DBI)
library(dplyr)
library(magrittr)

con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")

sample_tbls <- readRDS("tests/Github_Actions_Setup/sample_tbls.rds")

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
