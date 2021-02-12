#' remove preprints if peer review publication present
#'
#' @param tbl_merge tbl_merge
#'
#' @return
#' @export
#' @importFrom magrittr %>% 
#' @examples
#' con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' df_orcid <- retrieve_from_orcid("0000-0002-3048-5518")
#' df_publons <- retrieve_from_publons("0000-0002-3048-5518")
#' df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
#' tbl_merge <- suppressWarnings(create_combined_data(df_orcid,uzhOS::empty_pubmed(),uzhOS::empty_zora(),df_publons,con))
#' df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_zotero = FALSE,with_rcrossref=FALSE)
#' tbl_merge_comb <- uzhOS::merge_scholar_into_tbl_merge(tbl_merge, df_scholar_matched)
#' remove_duplicate_preprints(tbl_merge_comb)
remove_duplicate_preprints <- function(tbl_merge){
  if ("cid" %in% names(tbl_merge)){
    cid_ind <- !is.na(tbl_merge$cid) & tbl_merge$cid != ""
    cid_matches <- purrr::map_chr(seq_len(sum(cid_ind)), 
                                  ~ paste0(which(tbl_merge$cid[cid_ind] %in% tbl_merge$cid[cid_ind][.x]), collapse = " "))
    tbl_merge_cid_filtered <- tbl_merge[cid_ind,] %>% 
      dplyr::mutate(cid_group = cid_matches) %>% 
      tidyr::nest(dataset = -cid_group) %>% 
      dplyr::mutate(dataset = purrr::map(dataset, ~{
        if (("overall_oa" %in% names(.x)) && any(.x$overall_oa != "preprint")){
          dplyr::filter(.x, overall_oa != "preprint")
        } else {
          .x
        }
      })) %>% 
      tidyr::unnest(dataset) %>% 
      dplyr::select(-cid_group)
    tbl_merge <- rbind(tbl_merge_cid_filtered, tbl_merge[!cid_ind,])
  }
  
  not_na <- !is.na(tbl_merge$title)
  ld <- adist(toupper(tbl_merge$title[not_na]),toupper(tbl_merge$title[not_na]))
  ld_rel <- sapply(seq_len(dim(ld)[1]), function(i) ld[i,]/stringr::str_length(tbl_merge$title[not_na]))
  title_matches <- purrr::map_chr(seq_along(ld_rel[1,]), ~ paste0(which(ld_rel[.x,] < 0.1), collapse = " "))
  tbl_merge_title_filtered <- tbl_merge[not_na,] %>% 
    dplyr::mutate(title_group = title_matches) %>% 
    tidyr::nest(dataset = -title_group) %>% 
    dplyr::mutate(dataset = purrr::map(dataset, ~{
      if (("overall_oa" %in% names(.x)) && any(.x$overall_oa != "preprint")){
        dplyr::filter(.x, overall_oa != "preprint")
      } else {
        .x
      }
    })) %>% 
    tidyr::unnest(dataset) %>% 
    dplyr::select(-title_group)
  tbl_merge <- rbind(tbl_merge_title_filtered, tbl_merge[!not_na,])
  tbl_merge
}



