#' Trim null values or length 0 vectors to return single NA
#'
#' @param x value to fix
#'
#' @return the fixed values
#' @export
#'
#' @examples
#' fix_null(NULL)
#' fix_null(10)
#' fix_null(character(0))
fix_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  else return(x)
}


# pri_author <- "robinson m 0000 0002 3048 5518"
# sec_author <- "robinson m d"
# orcid <- "0000-0002-3048-5518"
# pubmed_search <- "(stoeckli esther[au]) or (stoeckli e[au] AND zurich[affiliation]) or Stoeckli ET[au]"
# 
# tib_co <- create_combined_data_wrapper(tbl_authorkeys,tbl_unique_authorkeys,tbl_eprints,tbl_subjects,pri_author,sec_author,orcid,pubmed_search,unpaywall,progress=NULL)

# names(tib_co)
# tib_co %>% select(starts_with("in_"))

#' Create pubmed search query
#'
#' @param authorname author id
#' @param tbl_unique_authorkeys_fullname mongodb connection of unique authorkeys
#' @param cutoff_year year, everything below will be excluded
#'
#' @return
#' @export
#' @import mongolite
#' @importFrom magrittr %>% 
#'
#' @examples
#' pri_author <- "robinson m 0000 0002 3048 5518"
#' pubmed_search_string_from_zora_id(pri_author,tbl_unique_authorkeys)
pubmed_search_string_from_zora_id <- function(authorname, con, authorkeystablename = "authorkeys", cutoff_year=2001, orcid = NULL){
  scaffold <- "(%s[au] or %s[au] or %s[au]) AND (%i:%i[pdat]) AND (zurich[affiliation])"
  auth_name <- tbl(con, authorkeystablename) %>% filter(authorkey_fullname %in% authorname) %>% collect() 
  full_name <- auth_name$authorname
  split_name <- strsplit(full_name," ")
  pubmed_search <- sprintf(scaffold,
                           tolower(full_name), 
                           tolower(paste(split_name[[1]][1],str_sub(split_name[[1]][2],end=1))),
                           paste(split_name[[1]][1],paste0(sapply(split_name[[1]][-1],function(elem) str_sub(elem,end=1)),collapse = "")),
                           cutoff_year,
                           as.integer(str_extract(Sys.Date(),"[:digit:]{4}")))
  if (!is.null(orcid)){
    pubmed_search <- paste(pubmed_search, "OR (orcid", orcid, "[auid])")
  }
  return(pubmed_search)
}


#' Retrieve a table of records from PubMed
#'
#' @param pmid_search search term to search PubMed
#' @param pmid_remove list of PMIDs to remove from list (that comes from search term) 
#' @param pmid_add list of PMIDs to add to list (that comes from search term)
#' @param just_ids set of PMIDs to just retrieve records for (in this case, `pmid_search`, `pmid_remove`, `pmid_add` are ignored)
#' @importFrom rentrez entrez_search entrez_summary
#'
#' @return
#' @export
#'
#' @examples
#' cvm <- retrieve_from_pubmed("von mering c[au]) NOT Von Mering, Christine[Author]")
#' 
#' pmids <- c("11743205", "15761153", "23857251", "26493315", 
#'            "30002819", "30356428", "31178352", "31857895")
#' pms <- retrieve_from_entrez(just_ids = pmids)
# grab everything from Entrez
retrieve_from_pubmed <- function(pmid_search, pmid_remove=NULL, pmid_add=NULL, just_ids=NULL) {
  if(is.null(just_ids)) {
    x <- entrez_search(db = "pubmed", term = pmid_search, retmax = 1000)
    x$ids <- unique(c(base::setdiff(x$ids, pmid_remove), pmid_add))
  } else {
    x <- list(ids=just_ids)
  }
  summ <- tryCatch({
    entrez_summary(db = "pubmed", id = x$ids)
    },error=function(e) NULL)
  if (is.null(summ)){
    return(NULL)
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
  # bunch of hacks to clean stuff up
  summ$title <- sub("\\.$","",summ$title)
  summ$title <- gsub("&lt;/u&gt;","",gsub("&lt;u&gt;","",summ$title, fixed=TRUE))
  summ$title <- gsub("&lt;/i&gt;","",gsub("&lt;i&gt;","",summ$title, fixed=TRUE))
  summ$doi <- tolower(summ$doi)
  summ$doi <- gsub("&lt;","<", summ$doi)
  summ$doi <- gsub("&gt;",">", summ$doi)
  summ$in_pubmed <- TRUE
  return(summ)
}
