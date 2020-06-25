
library(orcid)
library(dplyr)
library(roadoi)


source("code/functions.R")

ws <- retrieve_from_orcid("0000-0002-3048-5518")

oaf <- oadoi_fetch(ws$doi, 
                   email="mark.robinson@mls.uzh.ch")

oaf %>% filter(oa_status == "closed") %>% 
  select(title, year) %>% arrange(year)
