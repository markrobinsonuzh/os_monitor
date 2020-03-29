
# source("../code/functions.R")
source("code/functions.R")

# set these
# email <- "mark.robinson@mls.uzh.ch"
# orcid <- "0000-0002-3048-5518"
# scholar_id <- "XPfrRQEAAAAJ"
# pubmed_search <- "Robinson Mark D[au]"
# pmid_remove <- c("28824762", "26295592", "23288288", "21516278", "18830830",
#                  "18025499", "15786672", "15779224", "15322224", "30039500")
# pmid_add <- c("11743205", "15761153", "23857251", "26493315",
#               "31178352", "18772890", "18573797")

# zora_key <- "robinson m 0000 0002 3048 5518;robinson m d;robinson m"
# zora_key <- strsplit(zora_key, ";")[[1]]
# zora_subjects <- 10124

# paste0(pmid_add, collapse=";")

# email <- "wong@immunology.uzh.ch"
# orcid <- "0000-0003-3155-1211"
# pubmed_search <- "wong, w wei-lynn[author] OR Wong, Wendy W-L[Author] OR Wong, Wendy Wei-Lynn[Author] OR (gerondakis s and wong l)"
# scholar_id <- "JynlX_8AAAAJ"
# pmid_remove <- NULL
# pmid_add <- NULL

# email <- "mering@imls.uzh.ch"
# orcid <- "0000-0001-7734-9102"
# pubmed_search <- "(von mering c[au]) NOT Von Mering, Christine[Author]"
# scholar_id <- "Av-VeeEAAAAJ"
# pmid_remove <- pmid_add <- NULL

email <- "esther.stoeckli@mls.uzh.ch"
orcid <- "0000-0002-8485-0648"
pubmed_search <- "(stoeckli esther[au]) or (stoeckli e[au] AND zurich[affiliation]) or Stoeckli ET[au]"
scholar_id <- "Lp_wEokAAAAJ"
pmid_remove <- pmid_add <- NULL


# setup output file names
master_file <- paste0("PUBMASTERTABLE_", gsub(".uzh.ch","", email), ".txt")
bibtex_file <- paste0("BIBTEX_FOR_ORCID_", gsub(".uzh.ch","", email), ".bib")


# PUBMED
df_pubmed <- retrieve_from_entrez(pubmed_search, pmid_remove, pmid_add)
dim(df_pubmed)

# ORCID
df_orcid <- retrieve_from_orcid(orcid)
dim(df_orcid)

# In ORCID, not in PubMed (mostly preprints?)
setdiff(df_orcid$doi, df_pubmed$doi)

# In Pubmed, not in ORCID
(to_update <- na.omit(setdiff(df_pubmed$doi, df_orcid$doi)))

if(length(to_update) > 0) {
  df_pubmed[df_pubmed$doi %in% to_update,] %>% select(-authors,-pmid)
  
  # write missing entries to bibtex file
  bibtex_from_doi <- GetBibEntryWithDOI(to_update)
  # toBiblatex(bibtex_from_doi)
  writeLines(toBiblatex(bibtex_from_doi), bibtex_file)
}


# merge with ZORA
tbl_eprints <- readRDS(file.path("output", "tbl_eprints.rds"))

tbl_merge <- full_join(df_orcid, df_pubmed, by="doi", suffix = c(".orcid", ".pubmed"))
tbl_merge$doi[is.na(tbl_merge$doi)] <- "no_doi"
tbl_merge <- tbl_merge %>% left_join(tbl_eprints %>% select(-institution), 
                                     by="doi", suffix=c("",".zora"))


# SCHOLAR
df_scholar <- retrieve_from_scholar(scholar_id)
dim(df_scholar)


m1 <- match(toupper(df_scholar$title), toupper(tbl_merge$title.orcid))
m2 <- match(toupper(df_scholar$title), toupper(tbl_merge$title.pubmed))
df_scholar$doi <- df_orcid$doi[pmin(m1,m2)]

# non-matching titles
w <- is.na(df_scholar$doi)
df_scholar$title[w]

# bit of faffing to match titles from SCHOLAR to ORCID
scores <- calcScore(df_scholar$title[w], tbl_merge$title.orcid)
top_score <- apply(scores$dist, 1, max)
which_top_score <- apply(scores$dist, 1, which.max)

scholar_to_orcid_matches <- data.frame(top_score, scholar_title=df_scholar$title[w], 
                                       orcid_title=scores$cols[which_top_score])
scholar_to_orcid_matches

# View(scholar_to_orcid_matches)
cutoff <- .7999
keep <- top_score > cutoff
df_scholar$doi[which(w)[keep]] <- tbl_merge$doi[which_top_score[keep]]


# non-matching titles
w <- is.na(df_scholar$doi)
df_scholar$title[w]

# bit of faffing to find what is in Google Scholar, not in PUBMED
scores <- calcScore(df_scholar$title[w], tbl_merge$title.pubmed)
top_score <- apply(scores$dist, 1, max)
which_top_score <- apply(scores$dist, 1, which.max)

scholar_to_pubmed_matches <- data.frame(top_score, scholar_title=df_scholar$title[w], 
                                       pubmed_title=scores$cols[which_top_score])
# scholar_to_pubmed_matches

# View(scholar_to_pubmed_matches)
cutoff <- .80
keep <- top_score > cutoff
df_scholar$doi[which(w)[keep]] <- tbl_merge$doi[which_top_score[keep]]


tbl_master <- full_join(tbl_merge, 
                        df_scholar %>% select(-number,-cites,-year,-cid),
                        by="doi", suffix=c("",".scholar"))
write.table(tbl_master, master_file, sep="\t", quote=FALSE, row.names = FALSE)


# # found in Google Scholar, but unable to match to DOI/ORCID
# with(df_scholar, title[is.na(doi) & is.na(probable_doi)])
# write.table(df_scholar, scholar_file, sep="\t", quote=FALSE, row.names = FALSE)

# # what is in ORCID, but not in Google Scholar?
# scholar_dois <- unname(apply(df_scholar[,c("doi","probable_doi")], 
#                              1, function(u) na.omit(u)[1]))
# m <- match(df_orcid$doi, scholar_dois)
# df_orcid$scholar_id <- df_scholar$pubid[m]
# 
# df_orcid %>% filter(is.na(scholar_id))
# 
# write.table(df_orcid, orcid_file, sep="\t", quote=FALSE, row.names = FALSE)



# my_search <- "trypsinosome"
# grep(my_search, df_scholar$title[w], value=TRUE)
# grep(my_search, df_orcid$title, value=TRUE)

