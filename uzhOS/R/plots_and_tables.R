#' Barplot zora data
#'
#' @param tbl_merge data.frame from \code{\link{create_combined_data}} 
#' @param cutoff_year year of cutoff for plotting
#' @param colname column name of year
#' @param title title of plot
#' @param oa_status_used column name of oa status 
#'
#' @return ggplot
#' @export
#' @import rlang
#' @import ggplot2
#' @importFrom magrittr %>% 
#'
#' @examples
oa_status_time_plot <- function(tbl_merge,cutoff_year,colname=year,title="ZORA OA Status",oa_status_used=oa_status){
  q_colname <- enquo(colname)
  q_oa_status_used <- enquo(oa_status_used)
  tmp <- tbl_merge %>% dplyr::pull(!!q_oa_status_used) 
  levels(tmp) <- c(levels(tmp),"unknown")
  tmp[is.na(tmp)] <- "unknown"
  tbl_merge <- tbl_merge %>% dplyr::mutate(!!q_oa_status_used := tmp)
  # tbl_merge_rel <- tbl_merge %>% 
  #   dplyr::group_by(!!q_colname) %>% dplyr::mutate()
  # ggplot(tbl_merge %>% dplyr::filter(!!q_colname >= cutoff_year, !!q_colname <= 2020), aes(x=!!q_colname, fill=!!q_oa_status_used)) + 
  #   geom_bar() + 
  #   theme(axis.text.x = element_text(angle = 90)) +
  #   ggtitle(title) +
  #   scale_fill_manual(values=open_cols_fn())
  
  tmptib_1 <- tbl_merge %>% 
    dplyr::filter(!!q_colname >= cutoff_year, !!q_colname <= 2020)%>% 
    group_by(!!q_colname,!!q_oa_status_used) %>% 
    summarise(Count=n()) %>%
    ungroup() %>% 
    group_by(!!q_colname) %>% 
    mutate(Proportion=Count/sum(Count)) %>% 
    tidyr::pivot_longer(cols = c(Count,Proportion))
  
  ggplot(tmptib_1, aes(x=!!q_colname,y=value, fill=!!q_oa_status_used)) + 
    geom_col() + facet_grid(rows = vars(name),scales = "free_y")+
    # facet_wrap(~name,scales = "free_y") +
    # theme(axis.text.x = element_text(angle = 90)) +
    ggtitle(title) +
    labs(y="")+
    scale_fill_manual(values=open_cols_fn())
}

# tmptib_1 <- tmptib %>% 
#   group_by(b,a) %>% 
#   summarise(Count=n()) %>%
#   ungroup() %>% 
#   group_by(b) %>% 
#   mutate(Proportion=Count/sum(Count)) %>% 
#   tidyr::pivot_longer(cols = c(Count,Proportion))
# 
# ggplot(tmptib_1, aes(x=b,y=value, fill=a)) + 
#   geom_col() + facet_wrap(~name,scales = "free_y") +
#   # theme(axis.text.x = element_text(angle = 90)) +
#   ggtitle("title") +
#   scale_fill_manual(values=open_cols_fn())
# 
# ggplot(tmptib) + stat_count(aes(a))
# ggplot(tmptib, aes(x=b, fill=a)) + 
#   geom_bar() 
  # theme(axis.text.x = element_text(angle = 90)) +
  # ggtitle(title) +
  # scale_fill_manual(values=open_cols_fn())

#' Table of closed article in Zora
#'
#' @param zora data.frame, created from \code{\link{create_zora}}
#'
#' @return \code{\link[DT]{datatable}}
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
closed_in_zora_table <- function(zora){
  z <- zora %>% 
    dplyr::filter(oa_status == "closed") %>%
    dplyr::select(doi, eprintid, type, refereed, title, oa_status, year) %>%
    dplyr::mutate(doi = ifelse(is.na(doi), "", 
                               paste0("<a href='https://www.doi.org/",
                                      doi, "' target='_blank'>", doi, "</a>")),
                  eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                             eprintid, "' target='_blank'>", eprintid, "</a>")) %>%
    dplyr::arrange(desc(year))
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')),
                escape = FALSE, rownames = FALSE)
}



#' #' Barplot combined zora and orcid data
#' #'
#' #' @param m data.frame, created from \code{\link{create_combined_data}}
#' #' @param cutoff_year year of cutoff for plotting
#' #'
#' #' @return ggplot
#' #' @export
#' #'
#' #' @examples
#' zora_orcid_plot <- function(m,cutoff_year){
#'   open_cols <- open_cols_fn()
#'   ggplot(m %>% filter(year >= cutoff_year, year <= 2020), aes(x=year, fill=overall_oa)) + 
#'     geom_bar() + 
#'     theme(axis.text.x = element_text(angle = 90)) +
#'     ggtitle("ZORA + ORCID OA Status") +
#'     scale_fill_manual(values=open_cols)
#' }

#' Table of percentage open access over time
#'
#' @param m data.frame, created from \code{\link{create_combined_data}}
#' @param cutoff_year year of cutoff for plotting
#'
#' @return \code{\link[DT]{datatable}}
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
oa_percent_time_table <- function(m,cutoff_year){
  z <- m %>% 
    dplyr::filter(year >= cutoff_year) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(oa_pct_blue = round(100*mean(overall_oa != "closed"),2),
                     oa_pct = round(100*mean(!(overall_oa %in% c("blue","closed")))),
                     closed = sum(overall_oa == "closed")) %>%
    data.frame()
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')),
                escape = FALSE, rownames = FALSE)
}



#' table of closed publications
#'
#' @param tbl_merge data.frame from \code{\link{create_combined_data}}
#' @param cutoff_year year of cutoff for plotting
#'
#' @return \code{\link[DT]{datatable}}
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
overall_closed_table <- function(tbl_merge,cutoff_year){
  z <- tbl_merge %>%
    # dplyr::filter(overall_oa == "closed", as.integer(year) >= cutoff_year) %>%
    dplyr::select(doi, eprintid, overall_oa, oa_status.zora,oa_status.unpaywall, year, dplyr::starts_with("title")) %>%
    dplyr::arrange(desc(year)) %>%
    dplyr::mutate(oa_status.unpaywall = ifelse(is.na(oa_status.unpaywall), "",
                                               paste0("<a href='https://api.unpaywall.org/v2/",
                                               doi,"?email=YOUR_EMAIL' target='_blank'>",
                                               oa_status.unpaywall, "</a>")),
                  doi = ifelse(is.na(doi), "", 
                               paste0("<a href='https://www.doi.org/",doi, "' target='_blank'>", doi, "</a>")),
                  eprintid = ifelse(is.na(eprintid), "",
                                    paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                                           eprintid, "' target='_blank'>", eprintid, "</a>")))
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')),
                escape = FALSE, rownames = FALSE)
}



# zora_without_orcid_table <- function(m,cutoff_year){
#   z <- m %>% filter(!is.na(eprintid), is.na(type.orcid), year >= cutoff_year, is_zora, !is_orcid)
#   z <- z %>% 
#     select(doi, eprintid, type.zora, refereed, title, oa_status.zora, year) %>%
#     mutate(doi = ifelse(is.na(doi), "", paste0("<a href='https://www.doi.org/",
#                                                doi, "'>", doi, "</a>"))) %>%
#     mutate(eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
#                              eprintid, "'>", eprintid, "</a>")) %>%
#     arrange(desc(year))
#   DT::datatable(z, extensions = 'Buttons',
#                 options = list(dom = 'Bfrtip',
#                                pageLength = 200,
#                                buttons = list('copy', 'csv', 'excel')), 
#                 escape = FALSE, rownames = FALSE)
# }

# orcid_without_zora_table <- function(m,cutoff_year){
#   z <- m %>% filter(is.na(eprintid), !is.na(type.orcid), 
#                     year >= cutoff_year, type.orcid != "other")
#   z <- z %>% 
#     select(doi, type.orcid, title, journal, year) %>%
#     mutate(doi = ifelse(is.na(doi), "", paste0("<a href='https://www.doi.org/",
#                                                doi, "'>", doi, "</a>"))) %>%
#     arrange(desc(year))
#   DT::datatable(unique(z), extensions = list('Buttons','Scroller','FixedHeader'),
#                 class = 'cell-border stripe',
#                 options = list(dom = 'Bfrtip',
#                                buttons = c('excel', "csv"),
#                                pageLength = 100, fixedHeader = TRUE),
#                 escape = FALSE, rownames = FALSE)
# }


# oa_status_diff_zora_unpaywall_table <- function(m,cutoff_year){
#   z <- m %>% filter(!is.na(oa_status.unpaywall), 
#                     !is.na(oa_status.zora), 
#                     oa_status.zora != oa_status.unpaywall,
#                     year >= cutoff_year) %>% 
#     select(doi, eprintid, oa_status.zora, year,
#            oa_status.unpaywall, title, journal) %>%
#     arrange(desc(year))
#   
#   z <- z %>% 
#     mutate(oa_status.unpaywall = paste0("<a href='https://api.unpaywall.org/v2/",
#                                         doi,"?email=YOUR_EMAIL'>",
#                                         oa_status.unpaywall, "</a>")) %>%
#     mutate(doi = paste0("<a href='https://www.doi.org/",
#                         doi, "'>", doi, "</a>")) %>%
#     mutate(eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
#                              eprintid, "'>", eprintid, "</a>"))
#   DT::datatable(z, extensions = 'Buttons',
#                 options = list(dom = 'Bfrtip',
#                                pageLength = 200,
#                                buttons = list('copy', 'csv', 'excel')), 
#                 escape = FALSE, rownames = FALSE)
# }


# tbl_merge <- full_join(tbl_merge,df_scholar,by="doi",suffix=c("",".scholar"))
# m <- tbl_merge



#' Upset plot
#'
#' @param tbl_merge data.frame from \code{\link{create_combined_data}} 
#'
#' @return \code{\link[UpSetR]{upset}} 
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
upset_plot <- function(tbl_merge){
  tib_plt <- tbl_merge %>% 
    dplyr::select( dplyr::starts_with("in_")) %>% 
    dplyr::mutate( dplyr::across( dplyr::starts_with("in_"),~as.integer(.x))) 
  UpSetR::upset(as.data.frame(tib_plt))
}


