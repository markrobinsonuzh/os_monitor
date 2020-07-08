library(dplyr)
library(rentrez)
library(RefManageR)
library(scholar)


fix_null <- function(x) {
  if (is.null(x) || length(x) == 0) NA
  else x
}

# grab everything from Entrez
retrieve_from_entrez <- function(pmid_search, pmid_remove=NULL, pmid_add=NULL, just_ids=NULL) {
  if(is.null(just_ids)) {
    x <- entrez_search(db = "pubmed", term = pmid_search, retmax = 1000)
    x$ids <- unique(c(base::setdiff(x$ids, pmid_remove), pmid_add))
  } else {
    x <- list(ids=just_ids)
  }
  summ <- entrez_summary(db = "pubmed", id = x$ids)
  
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
  summ$title <- gsub("&lt;/u&gt;","",gsub("&lt;u&gt;","",summ$title, fixed=TRUE))
  summ$title <- gsub("&lt;/i&gt;","",gsub("&lt;i&gt;","",summ$title, fixed=TRUE))
  summ$doi <- tolower(summ$doi)
  summ$doi <- gsub("&lt;","<", summ$doi)
  summ$doi <- gsub("&gt;",">", summ$doi)
  summ
}


# grab everything from ORCID
# > with(works, table(type, `display-index`))
# display-index
# type               -1   0   1
# data-set          0   9   0
# journal-article   2 117  44
# other             2  22   0
# report            0   4   0
# NOTE: should we be filtering to only 'journal-article' ?
retrieve_from_orcid <- function(orcid) {
  works <- rorcid::orcid_works(orcid)
  works <- works[[1]]$works
  if(nrow(works)==0) {
    df <- data.frame(title = character(0), journal=character(0),
                     type = character(0), doi = character(0), 
                     year = integer(0))
    return(df)
  }
  works$doi <- sapply(works$`external-ids.external-id`, 
                          function(u) ifelse(nrow(u)>0, u$`external-id-value`[u$`external-id-type`=="doi"], NA))
  works$doi <- tolower(works$doi)
  works$doi <- gsub("http://dx.doi.org/", "", works$doi)
  works <- works %>% filter(type != "data-set") %>%
    mutate(title = title.title.value, journal = `journal-title.value`,
           year = `publication-date.year.value`) %>%
    select(title, journal, type, doi, year)
  works$title <- sub("\\.$","",works$title)
  works <- unique(works)
  works_split <- split(works, works$doi)
  n <- sapply(works_split, nrow)
  z <- lapply(works_split[n>=2], function(u) {
    nas <- is.na(u$journal)
    if( sum(nas)>0 & sum(!nas)>0 )
      return(u[!nas,,drop=FALSE][1,,drop=FALSE])
    else
      return(u[1,,drop=FALSE])
  })
  df <- do.call(rbind,c(z,works_split[n==1]))
  rownames(df) <- NULL
  df
}


retrieve_from_scholar <- function(scholar_id) {
  starts <- seq(0,1000,by=100)
  scholar_pubs <- lapply(starts, function(u) {
    get_publications(scholar_id, cstart = u, pagesize = 100, flush = FALSE)  
  })
  scholar_pubs <- do.call(rbind, scholar_pubs)
  scholar_pubs <- unique(scholar_pubs)
  scholar_pubs$title <- as.character(scholar_pubs$title)
  scholar_pubs
}


split_to_rank <- function(u) {
  ss <- strsplit(u, "[ -/\\]")
  ss <- lapply(ss, function(v) {
    v <- v[nchar(v)>0]
    n <- length(v)
    setNames(1:n, toupper(v))
  })
  setNames(ss, u)
}
sentence_Dist <- function(a, b) {
  jaccard <- length(intersect(names(a),names(b))) / min(length(a),length(b),length(union(names(a),names(b))))
  n <- intersect(names(a), names(b))
  if(length(n) <= 4) return(0)
  return(cor(a[n],b[n], method = "spearman") * jaccard)
}

# calculate score of similarity between two vectors of titles
calcScore <- function(x,y) {
  ss_x <- split_to_rank(x)
  ss_y <- split_to_rank(y)
  dist <- matrix(0, nrow=length(ss_x), ncol=length(ss_y))
  for(i in seq_along(ss_x))
    for(j in seq_along(ss_y))
        dist[i,j] <- sentence_Dist(ss_x[[i]], ss_y[[j]])
  list(dist=dist, rows=x, cols=y)
  # keep_x <- rowSums(dist)>0
  # keep_y <- colSums(dist)>0
  # list(dist=dist[keep_x,keep_y,drop=FALSE], 
  #      rows=x[keep_x], 
  #      cols=y[keep_y])
}






