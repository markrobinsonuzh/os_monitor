library(dplyr)
library(rentrez)
library(RefManageR)
library(scholar)


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
  if (is.null(x) || length(x) == 0) NA
  else x
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
  # bunch of hacks to clean stuff up
  summ$title <- sub("\\.$","",summ$title)
  summ$title <- gsub("&lt;/u&gt;","",gsub("&lt;u&gt;","",summ$title, fixed=TRUE))
  summ$title <- gsub("&lt;/i&gt;","",gsub("&lt;i&gt;","",summ$title, fixed=TRUE))
  summ$doi <- tolower(summ$doi)
  summ$doi <- gsub("&lt;","<", summ$doi)
  summ$doi <- gsub("&gt;",">", summ$doi)
  summ
}


#' Retrieve a table of records from orcid.org
#'
#' @param orcid ORCID to retrieve records for
#' @param exclude what type of ORCID records to exclude from the list
#'
#' @return
#' @export
#'
#' @examples
#' mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
retrieve_from_orcid <- function(orcid, exclude = "data-set") {
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
  works <- works %>% filter(!(type %in% exclude)) %>%
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


#' Retrieve a table of records from Google Scholar
#'
#' @param scholar_id 
#'
#' @return
#' @export
#'
#' @examples
#' mr_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
retrieve_from_scholar <- function(scholar_id) {
  starts <- seq(0,1000,by=100)
  scholar_pubs <- lapply(starts, function(u) {
    scholar::get_publications(scholar_id, cstart = u, pagesize = 100, flush = FALSE)  
  })
  scholar_pubs <- do.call(rbind, scholar_pubs)
  scholar_pubs <- unique(scholar_pubs)
  scholar_pubs$title <- as.character(scholar_pubs$title)
  scholar_pubs
}


#' Split title into named vector
#'
#' @param u character string of title
#'
#' @return named vector
#'
#' @examples
#' split_to_rank("Here is a title")
split_to_rank <- function(u) {
  ss <- strsplit(u, "[ -/\\]")
  ss <- lapply(ss, function(v) {
    v <- v[nchar(v)>0]
    n <- length(v)
    setNames(1:n, toupper(v))
  })
  setNames(ss, u)
}

#' Compute distance between two titles
#'
#' @param a first title
#' @param b second title
#'
#' @return
#' @export
#'
#' @examples
#' sentence_Dist("This is the first title",
#'               "This is the second title")
sentence_Dist <- function(a, b) {
  jaccard <- length(intersect(names(a),names(b))) / min(length(a),length(b),length(union(names(a),names(b))))
  n <- intersect(names(a), names(b))
  if(length(n) <= 4) return(0)
  return(cor(a[n],b[n], method = "spearman") * jaccard)
}

#' Calculate all pairwise scores for two vectors of 
#'
#' @param x first vector of titles
#' @param y second vector of titles
#'
#' @return
#' @export
#'
#' @examples
#' calcScore(c("A title","Another title"),
#'           c("This title","That title"))
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



#' fetch doi entries from local unpaywall
#'
#' @param dois dois to get open access status from
#' @param file filename or data.frame of unpaywall data 
#'
#' @return open access status for each doi
#' @export
#'
#' @examples 
#' oadoi_fetch_local("10.1177/000271625529700159")
oadoi_fetch_local <- function(dois,file=readRDS(here::here("output","dois_unpaywall.rds"))){
  if (is(file,"mongo")){
    lapply(dois,function(doi){
      single_query <- file$find(paste0('{"doi":"',doi,'"}'))
      if (length(single_query)<2){
        tibble::tibble(doi=NULL,oa_status=NULL)
      } else {
        tibble::tibble(doi=single_query[[1]],oa_status=single_query[[2]])
      }
    }) %>% purrr::reduce(rbind)
  } else if (!is(file,"data.frame")){
    file <- readRDS(here::here("output","dois_unpaywall.rds"))
    dplyr::filter(file,doi %in% dois)
  } else {
    dplyr::filter(file,doi %in% dois)
  }
  
}


#' key of oa status
#'
#' @return list of keypairs
#' @export
#'
#' @examples
open_cols_fn <- function(){
  c("closed" = "gray48", "hybrid" = "darkorange1",
    "green" = "chartreuse4", "gold" = "gold",
    "preprint" = "red", "bronze" = "darkgoldenrod4",
    "blue" = "blue") 
}


#' create_tbl_author
#'
#' @param tbl_authorkeys tibble created in '01_zora_preprocessing.Rmd'
#' @param tbl_eprints tibble created in '01_zora_preprocessing.Rmd'
#' @param pri_author author id (with attached orcid)
#' @param sec_author author id 
#'
#' @return tbl_author
#' 
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
create_tbl_author <- function(tbl_authorkeys,tbl_eprints,pri_author,sec_author){
  tbl_authorkeys %>% 
    dplyr::filter(authorkey %in% c(pri_author, sec_author)) %>%
    dplyr::left_join(tbl_eprints,by="eprintid") %>%
    dplyr::mutate(year = date, doi = tolower(doi))
} 

#' create_zora
#'
#' @param pri_author author id (with attached orcid)
#' @param sec_author author id 
#' @param tbl_author tibble, created from \code{\link{create_tbl_author}}
#' @param tbl_subjects tibble created in '01_zora_preprocessing.Rmd'
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#' 
#' @examples
create_zora <- function(pri_author,sec_author,tbl_author,tbl_subjects){
  open_cols <-  open_cols_fn()
  dept_fac <- tbl_author %>% dplyr::left_join(tbl_subjects %>%
                                                dplyr::select(eprintid, name, parent_name),by="eprintid")
  org_unit <- suppressMessages(dept_fac %>% dplyr::select(name) %>% dplyr::group_by(name) %>%
    tally %>% dplyr::top_n(1) %>% dplyr::pull(name))
  zora <- dept_fac %>%
    dplyr::mutate(dept = name, faculty = parent_name) %>%
    dplyr::filter(authorkey == pri_author | (authorkey %in% sec_author & dept == org_unit)) %>%
    dplyr::select(-dept, -faculty, -name, -parent_name) %>% unique()
  
  # add blue OA
  zora$oa_status[zora$published_doc & zora$oa_status=="closed"] <- "blue" 
  zora$oa_status <- factor(zora$oa_status, levels = names(open_cols))
  zora
}

#' create combined data from zora, orcid and unpaywall
#'
#' @param ws tibble from \code{\link{retrieve_from_orcid}}
#' @param zora tibble from \code{\link{create_zora}}
#' @param unpaywall data.frame of reduced unpaywall database
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
create_combined_data <- function(ws,zora,unpaywall){
  open_cols <-  open_cols_fn()
  if (!is.null(ws)){
    m <- dplyr::full_join(ws, zora, 
                          by="doi", suffix=c(".orcid",".zora")) %>%
      dplyr::filter(doi != "logical(0)")
  } else {
    m <- zora
  }

  oaf <- oadoi_fetch_local(unique(na.omit(m$doi)),unpaywall)
  
  m <- m %>% dplyr::left_join(oaf %>% select(doi, oa_status), 
                       by = "doi", suffix=c(".zora", ".unpaywall"))
  if (!is.null(ws)){
    m <- m %>% dplyr::mutate(year = year.orcid, title = title.orcid)
  } 
  
  m$overall_oa <- m$oa_status.unpaywall
  m$overall_oa[m$type.orcid=="other"] <- "preprint"
  w <- is.na(m$overall_oa)
  m$overall_oa[w] <- m$oa_status.zora[w]
  w <- is.na(m$year)
  m$year[w] <- m$year.zora[w]
  w <- is.na(m$title)
  m$title[w] <- m$title.zora[w]
  w <- m$overall_oa == "closed" & m$oa_status.zora=="blue"
  m$overall_oa[w] <- "blue"
  m$overall_oa <- factor(m$overall_oa, levels = names(open_cols))
  m
}



#' create_combined_data_wrapper
#'
#' @param tbl_authorkeys tibble created in '01_zora_preprocessing.Rmd'
#' @param tbl_eprints tibble created in '01_zora_preprocessing.Rmd'
#' @param tbl_subjects tibble created in '01_zora_preprocessing.Rmd'
#' @param pri_author author id (with attached orcid)
#' @param sec_author author id 
#' @param orcid orcid
#' @param unpaywall data.frame of reduced unpaywall database
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
create_combined_data_wrapper <- function(tbl_authorkeys,tbl_eprints,tbl_subjects,pri_author,sec_author,orcid,unpaywall,progress=NULL){
  tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,pri_author,sec_author)
  if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
  dept_fac <- tbl_author %>% dplyr::left_join(tbl_subjects %>% 
                                                dplyr::select(eprintid, name, parent_name),by="eprintid")
  if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
  org_unit <- suppressMessages(dept_fac %>% dplyr::select(name) %>% dplyr::group_by(name) %>% 
    tally %>% dplyr::top_n(1) %>% dplyr::pull(name))
  if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
  ws <- tryCatch({retrieve_from_orcid(orcid) %>%
      dplyr::mutate(doi = tolower(doi))},
      error=function(e) NULL)
  if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
  # ws <- retrieve_from_orcid(orcid) %>%
  #     dplyr::mutate(doi = tolower(doi))
  zora <- create_zora(pri_author,sec_author,tbl_author,tbl_subjects)
  if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
  create_combined_data(ws,zora,unpaywall)
}



org_unit_fac <- function(pri_author,sec_author,tbl_subjects,tbl_authorkeys,tbl_eprints){
  tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,pri_author,sec_author)
  dept_fac <- tbl_author %>% left_join(tbl_subjects %>%
                                         select(eprintid, name, parent_name),by="eprintid")
  org_unit <- suppressMessages(dept_fac %>% select(name) %>% group_by(name) %>%
    tally %>% top_n(1) %>% pull(name))
  fac <- suppressMessages(dept_fac %>% select(parent_name) %>% group_by(parent_name) %>%
    tally %>% top_n(1) %>% pull(parent_name))
  return(list(org_unit=org_unit,fac=fac))
}



