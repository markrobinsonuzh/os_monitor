
library(dplyr)

outdir <- "output"

tbl <- readRDS(file.path(outdir, "tbl.rds"))

tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_eprints$in_crossref <- tbl_eprints$doi %in% tbl$doi
tbl_eprints <- tbl_eprints %>% filter(!is.na(doi), in_crossref)

lookup_bak <- lookup <- readRDS(file.path(outdir, "lookup.rds"))
lookup <- unique(lookup[,1:2])
lookup <- lookup %>% filter(!is.na(journal))

jour2pub <- unique(lookup_bak[,2:3]) %>% filter(!is.na(journal))

# read in all the journals covered

z <- tbl_eprints %>% left_join(tbl, by="doi") 

z$issn <- z$issn.y
y_na <- is.na(z$issn.y)
z$issn[y_na] <- z$issn.x[y_na]
z$issn.x <- NULL
z$issn.y <- NULL

# k <- lookup$issn %in% z$issn

m <- match(z$issn, lookup$issn)
z$journal <- lookup$journal[m]
z$doaj <- as.logical(z$doaj)
z$doaj[is.na(z$doaj)] <- FALSE

# z <- z %>% left_join(lookup, by="issn")

rp_journals <- readRDS("rp_journals.rds")
z$pr_cover <- z$issn %in% rp_journals$ISSN

x <- z %>% filter(!pr_cover, !doaj, !is.na(journal)) %>%
  left_join(jour2pub, by="journal")

is_dup <- duplicated(x[,c("eprintid","doi")])
x <- x[!is_dup,]

head(sort(table(x$journal), decreasing = TRUE), 20)

# View(x %>% group_by(journal, publisher) %>% tally %>% arrange(desc(n)))

x %>% group_by(journal, publisher) %>% tally %>% arrange(desc(n)) %>% filter(n>20)

x %>% filter(grepl("Springer", publisher))


a <- x %>% group_by(journal, publisher) %>% 
  tally %>% arrange(desc(n)) %>% 
  filter(n>1) %>% as.data.frame()

library(readr)

write_delim(a, file="uzh_published_not_OA_covered.txt", 
            delim="\t", col_names = TRUE)

