
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
df_pubmed <- retrieve_from_pubmed(pubmed_search, pmid_remove, pmid_add)
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
tbl_eprints <- readRDS(file.path("..","output", "tbl_eprints.rds"))

tbl_merge <- full_join(df_orcid, df_pubmed, by="doi", suffix = c(".orcid", ".pubmed"))
tbl_merge$doi[is.na(tbl_merge$doi)] <- "no_doi"
tbl_merge <- tbl_merge %>% left_join(tbl_eprints %>% select(-institution), 
                                     by="doi", suffix=c("",".zora"))



# SCHOLAR
df_scholar <- retrieve_from_scholar(scholar_id)
dim(df_scholar)


m1 <- match(toupper(df_scholar$title), toupper(tbl_merge$title.orcid))
m1[is.na(m1)] <- Inf

m2 <- match(toupper(df_scholar$title), toupper(tbl_merge$title.pubmed))
m2[is.na(m2)] <- Inf

doi_is_na <- which(pmin(m1,m2)==Inf)
ld <- adist(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.orcid))
ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
m3 <- rep(Inf,length(df_scholar$title))
m3[doi_is_na] <- unlist(apply(ld_rel, 2, function(x) {
  tmpind <- which(x==min(na.omit(x)) & x < 0.1)
  ifelse(length(tmpind)==0,Inf,tmpind)}))

ld <- adist(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.pubmed))
ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
m4 <- rep(Inf,length(df_scholar$title))
m4[doi_is_na] <- unlist(apply(ld_rel, 2, function(x) {
  tmpind <- which(x==min(na.omit(x)) & x < 0.1)
  ifelse(length(tmpind)==0,Inf,tmpind)}))


doi_is_na <- which(pmin(m1,m2,m3,m4)==Inf)

# ld <- adist(toupper(df_scholar$journal[doi_is_na]),toupper(tbl_merge$title.orcid))
# ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
# m3 <- rep(Inf,length(df_scholar$title[doi_is_na]))
# m3[doi_is_na] <- unlist(apply(ld_rel, 2, function(x) {
#   tmpind <- which(x==min(na.omit(x)) & x < 0.1)
#   ifelse(length(tmpind)==0,Inf,tmpind)}))





# ldm <- apply(ld,1, which.min)
# ld_o <- adist(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.orcid))
# ldm_o <- apply(ld_o,1, which.min)
# 
# match_2 <- which(ldm==ldm_o)
# 
# i <- match_2[2]
# df_scholar$title[i]
# ld[i,ldm[i]]
# tbl_merge$title.orcid[ldm[i]]
# tbl_merge$title[ldm[i]]

# m3 <- lapply(seq_along(df_scholar$journal), 
#              function(i) which(sapply(seq_along(tbl_merge$journal.orcid), 
#                                       function(j) match(toupper(df_scholar$journal[i]),toupper(tbl_merge$journal.orcid[j])))==1))

# m5 <- lapply(seq_along(df_scholar$journal[doi_is_na]), 
#              function(i) which(sapply(seq_along(tbl_merge$journal.orcid), 
#                                 function(j) match(toupper(df_scholar$journal[doi_is_na][i]),toupper(tbl_merge$journal.orcid[j])))==1))


# match year, authors and journal
# journal
ld <- adist(toupper(df_scholar$journal[doi_is_na]),toupper(tbl_merge$journal.orcid))
ld_rel <- t(sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$journal[doi_is_na][i])))
# m5 <- rep(Inf,length(df_scholar$journal[doi_is_na]))
m5 <- lapply(seq_len(dim(ld_rel)[1]), function(i) {
  tmpind <- which(ld_rel[i,] < 0.05)
  if(length(tmpind)==0) {Inf
    }else {tmpind}})

# year
m7 <- lapply(seq_along(df_scholar$year[doi_is_na]),
             function(i) which(as.integer(df_scholar$year[doi_is_na][i]) == as.integer(tbl_merge$year)))

# authors
uncomplete_authors <- str_detect(df_scholar$author[doi_is_na],"\\.{3}")
df_scholar$author[doi_is_na][uncomplete_authors] <-  sapply(df_scholar$pubid[doi_is_na][uncomplete_authors], function(pubid) get_complete_authors(scholar_id,pubid))

s_auth <- df_scholar$author[doi_is_na] %>% 
  str_to_upper() %>% 
  str_replace_all(" ?, ?",",") %>% 
  str_split(",")

m_auth <- tbl_merge$authors %>% 
  str_to_upper() %>% 
  str_replace_all(" ?, ?",",") %>% 
  str_split(",")

tmp <- lapply(seq_along(m_auth), function(i) m_auth[[i]] %>% stringi::stri_extract_all_words())
m_auth_family <- lapply(seq_along(m_auth), function(i) unlist(lapply(seq_along(m_auth[[i]]), function(j) tmp[[i]][[j]][which.max(str_length(tmp[[i]][[j]]))])))

tmp <- lapply(seq_along(s_auth), function(i) s_auth[[i]] %>% stringi::stri_extract_all_words())
s_auth_family <- lapply(seq_along(s_auth), function(i) unlist(lapply(seq_along(s_auth[[i]]), function(j) tmp[[i]][[j]][which.max(str_length(tmp[[i]][[j]]))])))


m8 <- lapply(seq_along(s_auth_family),function(j){
  which(sapply(seq_along(s_auth_family), function(i) {
    tryCatch({
    tmpld <- adist(s_auth_family[[j]],m_auth_family[[i]])
    all(apply(tmpld< 3,1,function(x)sum(x)>0)) &
    dim(tmpld)[1]==dim(tmpld)[2]
    },error=function(e) FALSE)
    }))
})

mmm <- lapply(seq_along(doi_is_na), function(i) intersect(intersect(m5[[i]],m7[[i]]),m8[[i]]))
mmm_b <- sapply(mmm, function(x) length(x)>0)
which(mmm_b)
mmm[mmm_b]


# apply
df_scholar$doi <- tbl_merge$doi[pmin(m1,m2,m3,m4)]
df_scholar$doi[doi_is_na[mmm_b]] <- tbl_merge$doi[unlist(mmm[mmm_b])]


doi_is_na <- doi_is_na[!mmm_b]



# df_scholar$journal[doi_is_na][4]
# df_scholar$year[doi_is_na][4]
# tbl_merge$journal.orcid[m5[[4]]]
# tbl_merge$year[m7[[4]]]


# df_scholar$pubid[doi_is_na][1]
# 
# df_scholar$author[doi_is_na] <-  sapply(df_scholar$pubid[doi_is_na], function(pubid) get_complete_authors(scholar_id,pubid))
# df_scholar
# tmp_authors <- 
# 
# df_scholar$author[doi_is_na][2]
# 
# which(mm)
# if(m5[[i]] != Inf){ m5[[i]] %in% m7[[j]]}
# 
# m5[[1]] %in% m7
# 
# m5[[1]] %in% m6[[1]]
# m4[[1]]
# m5[[1]]
# df_scholar$title[1]
# tbl_merge$title.orcid[29]
# 
# df_scholar$year
# tbl_merge$year


# df_scholar$
# tbl_merge$journal.orcid
# df_s_a_split <- str_split(str_to_upper(df_scholar$author),",") 
# df_m_a_split <- str_split(str_to_upper(tbl_merge$authors),",")
# 
# lapply(seq_along(df_s_a_split), function(j) which.max(unlist(lapply(seq_along(df_m_a_split), function(i) sum(df_s_a_split[[j]] %in% df_m_a_split[[i]])))))
# 
# m3 <- match(toupper(df_scholar$author), toupper(tbl_merge$authors))
# df_scholar$doi <- df_orcid$doi[pmin(m1,m2)]

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
df_scholar$doi[df_scholar$doi=="no_doi"] <- NA


doi_is_na <- is.na(df_scholar$doi)
# get info from rcrossref
qrows <- df_scholar[doi_is_na,]
Sys.setenv(crossref_email="reto.gerber@uzh.ch")
out <- lapply(seq_len(doi_is_na),function(i){
  sq <- cr_works(flq=list(query.bibliographic=paste(qrows$title[i],qrows$year[i], qrows$journal[i]),
                          query.author=qrows$author[i]),limit = 3)
  scores <- as.numeric(sq$data$score)
  if (scores[1]/scores[2]>1.5 & scores[1]>70 & !is.null(sq)){
    sq$data[1,] %>% select(doi,container.title,published.print,title)
  } else{
    data.frame(doi=character(),container.title=character(),published.print=character(),title=character())
  }
})
empty_r <- sapply(seq_along(out), function(i) nrow(out[[i]])>0)
out_tib <- out %>% reduce(rbind)

df_scholar$doi[doi_is_na][which(empty_r)] <- out_tib$doi
df_scholar$doi[df_scholar$doi=="no_doi"] <- NA



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

