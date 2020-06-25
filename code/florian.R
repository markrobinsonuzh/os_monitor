
library(dplyr)
library(rcrossref)
library(roadoi)
source("code/functions.R")

outdir <- "output"
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))

author <- c("altermatt f 0000 0002 4831 6958", "altermatt f")
orcid <- "0000-0002-4831-6958"

tbl_author <- tbl_authorkeys %>% filter(authorkey %in% author) %>%
  left_join(tbl_eprints)

ws <- retrieve_from_orcid("0000-0002-4831-6958")

m <- full_join(ws, tbl_author, 
               by="doi", suffix=c(".orcid",".zora"))

m %>% filter(doi == "10.5061/dryad.78g8h50")
m <- m %>% filter(doi != "10.5061/dryad.78g8h50")

oaf <- oadoi_fetch(unique(na.omit(m$doi)),
                   email="mark.robinson@mls.uzh.ch")

m <- m %>% left_join(oaf %>% select(doi, oa_status), by = "doi", suffix=c(".zora", ".unpaywall"))


with(m, table(oa_status.zora, oa_status.unpaywall, useNA="ifany"))

m <- m %>% select(-authorkey, -institution)

write.table(m, "florian_altermatt.txt", quote=FALSE, sep="\t", row.names=FALSE)

# oaf %>% filter(oa_status == "closed") %>% select(title, year) %>% arrange(year)




