library(dplyr)
library(rentrez)
library(RefManageR)
library(scholar)

# set these
orcid <- "0000-0002-3048-5518"
scholarid <- "XPfrRQEAAAAJ"
pmid_search <- "Robinson Mark D[au]"
pmid_remove <- c("28824762", "26295592", "23288288", "21516278", "18830830",
                 "18025499", "15786672", "15779224", "15322224", "30039500",
                 "31925006")
pmid_add <- c("11743205", "15761153", "23857251", "26493315", 
              "31178352", "18772890", "18573797")

# orcid <- "0000-0003-2278-120X"
# pmid_search <- "Greber Urs[au]"
# pmid_remove <- NULL
# pmid_add <- NULL


# grab everything from Entrez
x <- entrez_search(db = "pubmed", term = pmid_search, retmax = 1000)
x$ids <- unique(c(base::setdiff(x$ids, pmid_remove), pmid_add))
summ <- entrez_summary(db = "pubmed", id = x$ids)

fix_null <- function(x) {
  if (is.null(x) || length(x) == 0) NA
  else x
}

summ <- lapply(summ, function(w) {
  data.frame(pubyear = fix_null(strsplit(w$pubdate, " ")[[1]][1]), 
             title = fix_null(w$title), 
             authors = fix_null(paste(w$authors$name, collapse = ", ")),
             journal = fix_null(w$source), 
             doi = fix_null(w$articleids$value[w$articleids$idtype == "doi"]),
             pmid = fix_null(w$articleids$value[w$articleids$idtype == "pubmed"]),
             stringsAsFactors = FALSE)
})
summ <- do.call(rbind, summ)
summ$title <- sub("\\.$","",summ$title)
summ$doi <- tolower(summ$doi)
write.table(summ, "records_from_pubmed.txt", sep="\t", quote=FALSE, row.names = FALSE)

# grab everything from ORCID
works <- rorcid::orcid_works(orcid)
works <- works[[1]]$works
dim(works)
works$pub_doi <- sapply(works$`external-ids.external-id`, 
                        function(u) ifelse(nrow(u)>0, u$`external-id-value`[u$`external-id-type`=="doi"], NA))
works$pub_doi <- tolower(works$pub_doi)
works %>% #filter(type=="journal-article") %>% 
  select(title.title.value, `journal-title.value`, pub_doi) -> orcid_doi
write.table(orcid_doi, "records_from_orcid.txt", sep="\t", quote=FALSE, row.names = FALSE)


# m <- full_join(summ, orcid_doi, by=c("doi"="pub_doi"))

setdiff(orcid_doi$pub_doi, summ$doi)

(to_update <- na.omit(setdiff(summ$doi, orcid_doi$pub_doi)))



# write missing entries to bibtex file
z <- GetBibEntryWithDOI(to_update,
                        temp.file = tempfile(fileext = ".bib"),
                        delete.file = TRUE)
toBiblatex(z)
writeLines(toBiblatex(z), "upload_to_orcid.tex")



starts <- seq(0,1000,by=100)
scholar_pubs <- lapply(starts, function(u) {
  get_publications("XPfrRQEAAAAJ", cstart = u, pagesize = 100, flush = FALSE)  
})
scholar_pubs <- do.call(rbind, scholar_pubs)
scholar_pubs$title <- as.character(scholar_pubs$title)
dim(scholar_pubs)

m <- match(scholar_pubs$title, summ$title)
scholar_pubs$lookup <- m

# many appear to be not match b/w Pubmed list and Google scholar list because:
# - weird markup (in either title)
# - published somewhere not indexed by pubmed
# - is a preprint
scholar_pubs %>% filter(is.na(lookup), journal != "bioRxiv")


my_search <- "Comparison"
grep(my_search, summ$title, value=TRUE)
grep(my_search, scholar_pubs$title, value=TRUE)


write.table(scholar_pubs, "records_from_googlescholar.txt", sep="\t", 
            quote=FALSE, row.names = FALSE)

