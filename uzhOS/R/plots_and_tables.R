#' Barplot zora data
#'
#' @param zora data.frame, created from \code{\link{create_zora}}
#' @param cutoff_year year of cutoff for plotting
#'
#' @return ggplot
#' @export
#'
#' @examples
zora_only_plot <- function(zora,cutoff_year){
  open_cols <- open_cols_fn()
  ggplot(zora %>% dplyr::filter(date >= cutoff_year, date <= 2020), aes(x=date, fill=oa_status)) + 
    geom_bar() + 
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("ZORA OA Status") +
    scale_fill_manual(values=open_cols)
}


#' Table of closed article in Zora
#'
#' @param zora data.frame, created from \code{\link{create_zora}}
#'
#' @return \code{\link[DT]{datatable}}
#' @export
#'
#' @examples
closed_in_zora_table <- function(zora){
  z <- zora %>% filter(oa_status == "closed") %>% 
    select(doi, eprintid, type, refereed, title, oa_status, year) %>%
    mutate(doi = ifelse(is.na(doi), "", paste0("<a href='https://www.doi.org/",
                                               doi, "'>", doi, "</a>"))) %>%
    mutate(eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                             eprintid, "'>", eprintid, "</a>")) %>%
    arrange(desc(year))
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')), 
                escape = FALSE, rownames = FALSE)
}



#' Barplot combined zora and orcid data
#'
#' @param m data.frame, created from \code{\link{create_combined_data}}
#' @param cutoff_year year of cutoff for plotting
#'
#' @return ggplot
#' @export
#'
#' @examples
zora_orcid_plot <- function(m,cutoff_year){
  open_cols <- open_cols_fn()
  ggplot(m %>% filter(year >= cutoff_year, year <= 2020), aes(x=year, fill=overall_oa)) + 
    geom_bar() + 
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle("ZORA + ORCID OA Status") +
    scale_fill_manual(values=open_cols)
}

#' Table of percentage open access over time
#'
#' @param m data.frame, created from \code{\link{create_combined_data}}
#' @param cutoff_year year of cutoff for plotting
#'
#' @return
#' @export
#'
#' @examples
oa_percent_time_table <- function(m,cutoff_year){
  z <- m %>% filter(year >= cutoff_year) %>% 
    group_by(year) %>% 
    summarize(oa_pct_blue = round(100*mean(overall_oa != "closed"),2),
              oa_pct = round(100*mean(!(overall_oa %in% c("blue","closed")))),
              closed = sum(overall_oa == "closed")) %>% 
    data.frame()
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')), 
                escape = FALSE, rownames = FALSE)
}










