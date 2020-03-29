library(dplyr)

outdir <- "output"
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))

# dmls <- tbl_subjects %>% filter(subjects==10124) %>% left_join(tbl_eprints)
geo <- tbl_subjects %>% filter(subjects==10122) %>% left_join(tbl_eprints) %>%
  left_join(tbl_authorkeys)

tg <- table(geo$authorkey)
sort(tg, decreasing = TRUE)[1:20]
grep("schaepman", names(tg), value=TRUE)

paste0(names(nm[]),collapse=",")

tbl_authors


# tbl_authorkeys <- readRDS(file.path("output", "tbl_authorkeys.rds")) %>%
#   filter(authorkey %in% zora_key)
# tbl_subjects <- readRDS(file.path("output", "tbl_subjects.rds")) %>%
#   filter(subjects %in% 10124)
# tbl_eprints_filtered <- tbl_eprints %>% select(-date,-institution) %>%
#   filter(eprintid %in% intersect(tbl_authorkeys$eprintid,tbl_subjects$eprintid))
