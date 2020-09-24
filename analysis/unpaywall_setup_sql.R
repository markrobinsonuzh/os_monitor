library(dplyr)
library(DBI)
library(magrittr)
library(tibble)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'oa',
                 host = 'db',
                 port = 5432, 
                 user = 'shiny',
                 password = 'flora',
                 options="-c search_path=oa")

tbl_unpaywall <- read.csv("dois_unpaywall.csv",header = FALSE,col.names = c("doi","oa_status"))
tbl_unpaywall <- tbl_unpaywall %>% as.tibble() %>% mutate(doi=tolower(doi))
tbl_unpaywall <- tbl_unpaywall %>% unique()
dbExecute(con, "DELETE FROM unpaywall;")
dbWriteTable(con, "unpaywall", tbl_unpaywall,overwrite=FALSE, append=TRUE)
dbExecute(con, "CREATE INDEX idx_doi_2 ON unpaywall(doi);")
# dbExecute(con,"ALTER TABLE tbl_unpaywall ADD CONSTRAINT unique_doi UNIQUE (doi);")

tbl(con,"unpaywall")
