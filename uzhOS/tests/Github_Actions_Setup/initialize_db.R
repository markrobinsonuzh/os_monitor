library(odbc)
library(DBI)
library(dplyr)
if (!requireNamespace("dbplyr", quietly = TRUE))
  install.packages("dbplyr")
library(dbplyr)
library(magrittr)

con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")

sample_tbls <- readRDS("tests/Github_Actions_Setup/sample_tbls.rds")

authorstablename = "authors"
authorkeystablename = "authorkeys"
eprintstablename = "eprints"
subjectstablename = "subjects"
unpaywalltablename = "unpaywall"

print(authorstablename)
dbWriteTable(con, authorstablename, sample_tbls$tbl_author, overwrite=TRUE)
tbl(con, authorstablename)

print(authorkeystablename)
dbWriteTable(con, authorkeystablename, sample_tbls$tbl_authorkeys, overwrite=TRUE)
tbl(con, authorkeystablename)

print(eprintstablename)
dbWriteTable(con, eprintstablename, sample_tbls$tbl_eprints, overwrite=TRUE)
tbl(con, eprintstablename)

print(subjectstablename)
dbWriteTable(con, subjectstablename, sample_tbls$tbl_subjects, overwrite=TRUE)
tbl(con, subjectstablename)

print(unpaywalltablename)
dbWriteTable(con, unpaywalltablename, sample_tbls$tbl_unpaywall, overwrite=TRUE)
tbl(con, unpaywalltablename)
# Disconnect from the database
dbDisconnect(con)
