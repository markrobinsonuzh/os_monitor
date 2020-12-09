
# con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
# author_vec <- c("robinson mark d","robinson mark d (orcid: 0000-0002-3048-5518)")
# df_zora <- create_zora(author_vec, con)
# df_orcid <- retrieve_from_orcid("0000-0002-3048-5518")
# search_string <- pubmed_search_string_from_zora_id(author_vec,con)
# df_pubmed <- retrieve_from_pubmed(search_string)
# tbl_merge <- create_combined_data(df_orcid,df_pubmed,df_zora,empty_publons(),con)
# 
# 
# df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
# df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_rcrossref=FALSE)
# 
# tbl_merge_new <- dplyr::full_join(tbl_merge,df_scholar_matched,by="doi",suffix=c("",".scholar")) %>%
#   dplyr::mutate(overall_oa = factor(dplyr::if_else(is.na(overall_oa), "unknown",as.character(overall_oa)),
#                                     levels = names(open_cols_fn()))) %>%
#   dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x))) %>%
#   dplyr::mutate(year = dplyr::if_else(is.na(year) & !is.na(year.scholar), as.integer(year.scholar), as.integer(year)))
# 
# merge_scholar_into_tbl_merge(tbl_merge, df_scholar_matched)
# connect_multiple_publications_with_scholar(tbl_merge_new)
# tmpfu <- future(seed=NULL,{
#   tmpscholar <- df_scholar_matching(tbl_merge_iso, df_scholar_iso, with_rcrossref = TRUE)
#   merge_scholar_into_tbl_merge(tbl_merge_iso, tmpscholar)
#   # dplyr::full_join(tbl_merge_iso,tmpscholar,by="doi",suffix=c("",".scholar"))
# },  globals = list('%>%'= magrittr::'%>%',
#                    df_scholar_matching=df_scholar_matching,
#                    df_scholar_iso=df_scholar,
#                    tbl_merge_iso=tbl_merge,
#                    merge_scholar_into_tbl_merge=merge_scholar_into_tbl_merge
# ))
# value(tmpfu)


#' Search for close matches between scholar and rest
#'
#' @param tbl_merge_new tbl_merge merged with df_scholar
#'
#' @return tbl_merge_new with some changed entries
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
connect_multiple_publications_with_scholar <- function(tbl_merge_new){
  # ld <- adist(toupper(tbl_merge_new$title),toupper(tbl_merge_new$title))
  # ld_y <-  as.matrix(dist(tbl_merge_new$year, diag=TRUE, upper = TRUE, method = "manhattan"))
  # ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/stringr::str_length(tbl_merge_new$title[i]))
  # m3 <- lapply(seq_len(dim(ld_rel)[1]), function(x) {
  #   which(ld_rel[x,] < 0.1 & ld_y[x, ]<=2)
  #   })
  
  # assume scholar lists titles beginning with 'Correction:' with the original publication.
  replaced_titles <- toupper(stringr::str_replace(tbl_merge_new$title,"Correction:",""))
  ld <- adist(replaced_titles,replaced_titles)
  ld_y <-  as.matrix(dist(tbl_merge_new$year, diag=TRUE, upper = TRUE, method = "manhattan"))
  ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/stringr::str_length(tbl_merge_new$title[i]))
  m3 <- lapply(seq_len(dim(ld_rel)[1]), function(x) {
    which(ld_rel[x,] < 0.1 & ld_y[x, ]<=2)
  })
    
  # copy for in place changes
  tbl_merge_update <- tbl_merge_new
  for(i in seq_along(m3)){
    # print(i)
    if(!tbl_merge_new[i,"in_scholar"]){
      tmpmatch <- m3[[i]]
      if(length(tmpmatch) != 1){
        which_is_scholar <- tbl_merge_new[tmpmatch,"in_scholar"] %>% 
          unlist() %>% 
          which()
        if(length(tmpmatch[which_is_scholar]) != 0){
          tbl_merge_update[i, stringr::str_detect(names(tbl_merge_new),"scholar")] <- 
            tbl_merge_new[tmpmatch[which_is_scholar], stringr::str_detect(names(tbl_merge_new),"scholar")]
        }
        
      }
    }
  }
  tbl_merge_update
}


#' merge tbl_merge with matched df_scholar
#'
#' @param tbl_merge tbl_merge from \code{\link{create_combined_data}}
#' @param df_scholar_matched df_scholar from \code{\link{df_scholar_matching}}
#'
#' @return tibble full join of tbl_merge and df_scholar_matched
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
merge_scholar_into_tbl_merge <- function(tbl_merge, df_scholar_matched){
  dplyr::full_join(tbl_merge,df_scholar_matched,by="doi",suffix=c("",".scholar")) %>% 
    dplyr::mutate(overall_oa = factor(dplyr::if_else(is.na(overall_oa), "unknown",as.character(overall_oa)),
                                      levels = names(open_cols_fn()))) %>% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("in_"), ~ dplyr::if_else(is.na(.x),FALSE,.x))) %>% 
    dplyr::mutate(year = dplyr::if_else(is.na(year) & !is.na(year.scholar), as.integer(year.scholar), as.integer(year))) %>% 
    connect_multiple_publications_with_scholar()
}


