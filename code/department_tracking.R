library(dplyr)

outdir <- "output"
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))

View(tbl_subjects %>% group_by(subjects, name) %>% tally())

# dmls <- tbl_subjects %>% filter(subjects==10124) %>% left_join(tbl_eprints)
geo <- tbl_subjects %>% filter(subjects==10124) %>% left_join(tbl_eprints) %>%
  left_join(tbl_authorkeys)



ieu <- tbl_subjects %>% filter(subjects==10127) %>% left_join(tbl_eprints) %>%
  left_join(tbl_authorkeys)

author <- c("altermatt f 0000 0002 4831 6958", "altermatt f")
orcid <- "0000-0002-4831-6958"

tbl_author <- tbl_authorkeys %>% filter(authorkey %in% author) %>%
  left_join(tbl_eprints) # %>%  left_join(tbl_subjects)


tg <- table(ieu$authorkey)
head(sort(tg, decreasing = TRUE), 50)


tg <- table(geo$authorkey)
sort(tg, decreasing = TRUE)
grep("robinson m", names(tg), value=TRUE)

mr <- grep("robinson m", names(tg))


# paste0(names(nm[]),collapse=",")

tbl_authors


# tbl_authorkeys <- readRDS(file.path("output", "tbl_authorkeys.rds")) %>%
#   filter(authorkey %in% zora_key)
# tbl_subjects <- readRDS(file.path("output", "tbl_subjects.rds")) %>%
#   filter(subjects %in% 10124)
# tbl_eprints_filtered <- tbl_eprints %>% select(-date,-institution) %>%
#   filter(eprintid %in% intersect(tbl_authorkeys$eprintid,tbl_subjects$eprintid))
