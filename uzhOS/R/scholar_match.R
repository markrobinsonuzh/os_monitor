#' get dois for google scholar titles 
#'
#' @param tbl_merge tibble with combined data from \code{\link{create_combined_data}}
#' @param df_scholar tibble from \code{\link{retrieve_from_scholar}}
#'
#' @return df_scholar with additional column 'doi'
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
# scholar_id <- "XPfrRQEAAAAJ"
# pri_author <- "robinson m 0000 0002 3048 5518"
# sec_author <- "robinson m d"
# full_author <- "robinson mark d"
# pri_author <- "robinson mark d (orcid: 0000-0002-3048-5518)"
# sec_author <- "robinson mark d"
# # sec_author <- ""
# orcid <- "0000-0002-3048-5518"
# 
# pubmed <- pubmed_search_string_from_zora_id(full_author,tbl_unique_authorkeys_fullname,2001)
# 
# # tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,pri_author)
# tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,c(pri_author,sec_author))
# 
# zora <- create_zora(c(pri_author,sec_author),tbl_author,tbl_subjects)
# 
# df_pubmed <- retrieve_from_pubmed(pubmed)
# if (!is.null(df_pubmed)){
#   tmpoadoi <- oadoi_fetch_local(na.omit(df_pubmed$doi),unpaywall)
#   df_pubmed <- left_join(df_pubmed,tmpoadoi,by="doi")
# }
# df_orcid <- tryCatch({retrieve_from_orcid(orcid) %>%
#     dplyr::mutate(doi = tolower(doi))},
#     error=function(e) NULL)
# if (!is.null(df_orcid)){
#   tmpoadoi <- oadoi_fetch_local(na.omit(df_orcid$doi),unpaywall)
#   df_orcid <- left_join(df_orcid,tmpoadoi,by="doi")
# }
# tbl_merge <- create_combined_data(df_orcid,df_pubmed,zora,NULL,unpaywall)
# df_scholar_master <- retrieve_from_scholar(scholar_id)
# df_scholar <- df_scholar_master
# 
# df_scholar <- df_scholar_matching(tbl_merge,df_scholar)
# tbl_merge <- full_join(tbl_merge,df_scholar,by="doi",suffix=c("",".scholar"))
# tbl_merge <- tbl_merge %>% mutate(across(starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))
df_scholar_matching <- function(tbl_merge,df_scholar, with_rcrossref=TRUE){
  if(dim(df_scholar)[1]==0){
    return(df_scholar %>% dplyr::mutate(doi=character()))
  }
  df_scholar <-  df_scholar %>% dplyr::mutate(doi = as.character(NA))
  if ("title.orcid" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    m1 <- match(toupper(df_scholar$title[doi_is_na]), toupper(tbl_merge$title.orcid))
    if (any(!is.na(m1))){
      df_scholar$doi[doi_is_na] <- tbl_merge$doi[m1]
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
    ld <- adist(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.orcid))
    ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
    m3 <- unlist(apply(ld_rel, 2, function(x) {
      tmpind <- which(x==min(na.omit(x)) & x < 0.1)
      ifelse(length(tmpind)==0,NA,tmpind)}))
    if (any(!is.na(m3))){
      df_scholar$doi[doi_is_na] <- tbl_merge$doi[m3]
    }
  }
  
  if ("title.pubmed" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    ld <- adist(toupper(df_scholar$title[doi_is_na]),toupper(tbl_merge$title.pubmed))
    ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$title[doi_is_na[i]]))
    m4 <- unlist(apply(ld_rel, 2, function(x) {
      tmpind <- which(x==min(na.omit(x)) & x < 0.1)
      ifelse(length(tmpind)==0,NA,tmpind)}))
    if (any(!is.na(m4))){
      df_scholar$doi[doi_is_na] <- tbl_merge$doi[m4]
    }
  }
  
  if ("title.orcid" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    
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
  
  if ("title.pubmed" %in% names(tbl_merge)){
    doi_is_na <- which(is.na(df_scholar$doi))
    
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
  
  # 
  # if ("title.orcid" %in% names(tbl_merge)){
  #   ld <- adist(toupper(df_scholar$journal[doi_is_na]),toupper(tbl_merge$journal.orcid))
  #   ld_rel <- t(sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/str_length(df_scholar$journal[doi_is_na][i])))
  #   # m5 <- rep(Inf,length(df_scholar$journal[doi_is_na]))
  #   m5 <- lapply(seq_len(dim(ld_rel)[1]), function(i) {
  #     tmpind <- which(ld_rel[i,] < 0.05)
  #     if(length(tmpind)==0) {NA
  #     }else {tmpind}})
  #   
  #   # year
  #   m7 <- lapply(seq_along(df_scholar$year[doi_is_na]),
  #                function(i) which(as.integer(df_scholar$year[doi_is_na][i]) == as.integer(tbl_merge$year)))
  #   
  #   # authors
  #   uncomplete_authors <- str_detect(df_scholar$author[doi_is_na],"\\.{3}")
  #   df_scholar$author[doi_is_na][uncomplete_authors] <-  sapply(df_scholar$pubid[doi_is_na][uncomplete_authors], function(pubid) get_complete_authors(scholar_id,pubid))
  #   
  #   s_auth <- df_scholar$author[doi_is_na] %>% 
  #     str_to_upper() %>% 
  #     str_replace_all(" ?, ?",",") %>% 
  #     str_split(",")
  #   
  #   m_auth <- tbl_merge$authors %>% 
  #     str_to_upper() %>% 
  #     str_replace_all(" ?, ?",",") %>% 
  #     str_split(",")
  #   
  #   tmp <- lapply(seq_along(m_auth), function(i) m_auth[[i]] %>% stringi::stri_extract_all_words())
  #   m_auth_family <- lapply(seq_along(m_auth), function(i) unlist(lapply(seq_along(m_auth[[i]]), function(j) tmp[[i]][[j]][which.max(str_length(tmp[[i]][[j]]))])))
  #   
  #   tmp <- lapply(seq_along(s_auth), function(i) s_auth[[i]] %>% stringi::stri_extract_all_words())
  #   s_auth_family <- lapply(seq_along(s_auth), function(i) unlist(lapply(seq_along(s_auth[[i]]), function(j) tmp[[i]][[j]][which.max(str_length(tmp[[i]][[j]]))])))
  #   
  #   
  #   m8 <- lapply(seq_along(s_auth_family),function(j){
  #     which(sapply(seq_along(s_auth_family), function(i) {
  #       tryCatch({
  #         tmpld <- adist(s_auth_family[[j]],m_auth_family[[i]])
  #         all(apply(tmpld< 3,1,function(x)sum(x)>0)) &
  #           dim(tmpld)[1]==dim(tmpld)[2]
  #       },error=function(e) FALSE)
  #     }))
  #   })
  # 
  #   mmm <- lapply(seq_along(doi_is_na), function(i) intersect(intersect(m5[[i]],m7[[i]]),m8[[i]]))
  #   mmm_b <- sapply(mmm, function(x) length(x)>0)
  #   which(mmm_b)
  #   mmm[mmm_b]
  #   df_scholar$doi[doi_is_na[mmm_b]] <- tbl_merge$doi[unlist(mmm[mmm_b])]
  # }
  # 
  if (with_rcrossref){
    doi_is_na <- which(is.na(df_scholar$doi))
    # get info from rcrossref
    qrows <- df_scholar[doi_is_na,]
    Sys.setenv(crossref_email="retogerber93@gmail.com")
    out <- lapply(seq_along(doi_is_na),function(i){
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
    })
    empty_r <- sapply(seq_along(out), function(i) nrow(out[[i]])>0)
    out_tib <- out %>% purrr::reduce(rbind)
    df_scholar$doi[doi_is_na][empty_r] <- out_tib$doi
  }

  return(df_scholar)
}


