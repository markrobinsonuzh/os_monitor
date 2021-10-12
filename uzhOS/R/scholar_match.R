#' get dois for google scholar titles 
#'
#' @param tbl_merge tibble with combined data from \code{\link{create_combined_data}}
#' @param df_scholar tibble from \code{\link{retrieve_from_scholar}}
#' @param with_rcrossref logical, get metadata from crossref
#' @param with_zotero get doi from url using zotero translater (\code{\link{doi_from_cid}})
#' @param ... additional parameters used in \code{\link{doi_from_cid}}
#' 
#' @return df_scholar with additional column 'doi'
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' df_orcid <- retrieve_from_orcid("0000-0002-3048-5518")
#' df_pubmed <- empty_pubmed()
#' df_zora <- empty_zora()
#' df_publons <- empty_publons()
#' con <-  odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' tbl_merge <- create_combined_data(df_orcid, df_pubmed, df_zora, df_publons, con)
#' 
#' df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
#' 
#' df_scholar <- df_scholar_matching(tbl_merge, df_scholar)
df_scholar_matching <- function(tbl_merge,df_scholar, with_rcrossref=TRUE, with_zotero=TRUE, ...){
  if(dim(df_scholar)[1]==0 || is.null(tbl_merge) || dim(tbl_merge)[1]==0){
    if(with_zotero){
      df_scholar <- search_doi_in_scholar_using_zotero(df_scholar)
    } else{
      df_scholar <- df_scholar %>% dplyr::mutate(doi=as.character(NA))
    }
    return(df_scholar)
  }
  df_scholar <-  df_scholar %>% dplyr::mutate(doi = as.character(NA))
  if ("title.orcid" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    m1 <- match(toupper(df_scholar$title[doi_is_na]), toupper(tbl_merge$title.orcid))
    m1_nona <- m1[!is.na(m1)]
    m1_cp <- m1
    # check if some entries match to the same entry of tbl_merge
    doublets <- table(m1_nona)[table(m1_nona)!=1]
    for(i in seq_along(doublets)){
      doublet_ind_m1 <- which(m1==as.integer(names(doublets[i])))
      # if year differs
      if(length(unique(df_scholar$year[doublet_ind_m1])) != 1){
        m1_tmp <- match(toupper(paste(df_scholar$title[doublet_ind_m1], df_scholar$year[doublet_ind_m1])), 
              toupper(paste(tbl_merge$title.orcid,tbl_merge$year.orcid)))
        # if still no match, just randomly choose matches...
        if(length(unique(m1_tmp)) != length(m1_tmp)){
          m1_tmp <- c(unique(m1_tmp), rep(NA, length(m1_tmp) - length(unique(m1_tmp))))
        }
        m1_cp[doublet_ind_m1] <- m1_tmp
      }
    }
    # m1 <- match(toupper(df_scholar$title[doi_is_na]), toupper(tbl_merge$title.orcid))
    if (any(!is.na(m1_cp))){
      df_scholar$doi[doi_is_na] <- tbl_merge$doi[m1_cp]
    }
  }
  if ("title.pubmed" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    m2 <- match(toupper(df_scholar$title[doi_is_na]), toupper(tbl_merge$title.pubmed))
    if (any(!is.na(m2))){
      df_scholar$doi[doi_is_na] <- tbl_merge$doi[m2]
    }
  }

  if ("title.orcid" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    if(length(doi_is_na) > 1){
      ld <- stringdist::stringdistmatrix(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.orcid), method = "lv")
      ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/stringr::str_length(df_scholar$title[doi_is_na[i]]))
      m3 <- unlist(apply(ld_rel, 2, function(x) {
        tmpind <- which(x==min(na.omit(x)) & x < 0.1)
        ifelse(length(tmpind)==0,NA,tmpind)}))
      if (any(!is.na(m3))){
        df_scholar$doi[doi_is_na] <- tbl_merge$doi[m3]
      }
    }
  }
  
  if ("title.pubmed" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    if(length(doi_is_na) > 1){
      ld <- stringdist::stringdistmatrix(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.pubmed), method = "lv")
      ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
      m4 <- unlist(apply(ld_rel, 2, function(x) {
        tmpind <- which(x==min(na.omit(x)) & x < 0.1)
        ifelse(length(tmpind)==0,NA,tmpind)}))
      if (any(!is.na(m4))){
        df_scholar$doi[doi_is_na] <- tbl_merge$doi[m4]
      }
    }
  }
  
  if ("title.orcid" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    if(length(doi_is_na) > 1){
      # bit of faffing to match titles from SCHOLAR to ORCID
      scores <- calcScore(df_scholar$title[doi_is_na], tbl_merge$title.orcid)
      top_score <- apply(scores$dist, 1, max)
      which_top_score <- apply(scores$dist, 1, which.max)
      
      scholar_to_orcid_matches <- data.frame(top_score, 
                                             scholar_title=df_scholar$title[doi_is_na], 
                                             orcid_title=scores$cols[which_top_score])
      
      cutoff <- .7999
      keep <- top_score > cutoff
      df_scholar$doi[doi_is_na[keep]] <- tbl_merge$doi[which_top_score[keep]]
    }
  }
  
  if ("title.pubmed" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    if(length(doi_is_na) > 1){
      # bit of faffing to match titles from SCHOLAR to ORCID
      scores <- calcScore(df_scholar$title[doi_is_na], tbl_merge$title.pubmed)
      top_score <- apply(scores$dist, 1, max)
      which_top_score <- apply(scores$dist, 1, which.max)
      
      scholar_to_orcid_matches <- data.frame(top_score, scholar_title=df_scholar$title[doi_is_na], 
                                             orcid_title=scores$cols[which_top_score])
      
      cutoff <- .7999
      keep <- top_score > cutoff
      df_scholar$doi[doi_is_na[keep]] <- tbl_merge$doi[which_top_score[keep]]
    }
  }
  
  # try to find DOI's using Zotero
  if (with_zotero){
    df_scholar <- search_doi_in_scholar_using_zotero(df_scholar)
  }

  if (with_rcrossref){
    doi_is_na <- which(is.na(df_scholar$doi))
    if(length(doi_is_na) > 1){
      # get info from rcrossref
      qrows <- df_scholar[doi_is_na,]
      Sys.setenv(crossref_email="retogerber93@gmail.com")
      out <- lapply(seq_along(doi_is_na),function(i){
        tryCatch({
          sq <- rcrossref::cr_works(flq=list(query.bibliographic=paste(qrows$title[i],qrows$year[i], qrows$journal[i]),
                                  query.author=qrows$author[i]),limit = 3)
          if(!is.null(sq$data)){
            sq$data <-
              sq$data %>% dplyr::mutate(score=as.numeric(score))
              
            if(dim(sq$data)[1] > 1){
              sq$data <-
                sq$data %>% dplyr::filter(.data[["score"]]/c(.data[["score"]][-1],.data[["score"]][length(.data[["score"]][-1])]) > 1.5)
            }
            sq$data %>% 
              dplyr::filter(score > 70) %>% 
              dplyr::slice(1) %>% 
              dplyr::select(doi,container.title,published.print,title)
          } else{
            tibble::tibble(doi=character(),container.title=character(),published.print=character(),title=character())
          }
        }, error=function(e) tibble::tibble(doi=character(),container.title=character(),published.print=character(),title=character()))
      })
      empty_r <- sapply(seq_along(out), function(i) nrow(out[[i]])>0)
      out_tib <- out %>% purrr::reduce(rbind)
      df_scholar$doi[doi_is_na][empty_r] <- out_tib$doi
    }
  }

  return(df_scholar)
}


