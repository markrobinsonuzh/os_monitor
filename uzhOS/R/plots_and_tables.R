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



overall_closed_table <- function(m,cutoff_year){
  z <- m %>% filter(overall_oa == "closed", year >= cutoff_year) %>% 
    select(doi, eprintid, overall_oa, oa_status.zora, 
           oa_status.unpaywall, year, title, journal) %>%
    arrange(desc(year))
  z <- z %>% 
    mutate(oa_status.unpaywall = ifelse(is.na(oa_status.unpaywall), "",
                                        paste0("<a href='https://api.unpaywall.org/v2/",
                                               doi,"?email=YOUR_EMAIL'>",
                                               oa_status.unpaywall, "</a>"))) %>%
    mutate(doi = ifelse(is.na(doi), "", paste0("<a href='https://www.doi.org/",
                                               doi, "'>", doi, "</a>"))) %>%
    mutate(eprintid = ifelse(is.na(eprintid), "", paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                                                         eprintid, "'>", eprintid, "</a>")))
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')), 
                escape = FALSE, rownames = FALSE)
}


zora_without_orcid_table <- function(m,cutoff_year){
  z <- m %>% filter(!is.na(eprintid), is.na(type.orcid), year >= cutoff_year)
  z <- z %>% 
    select(doi, eprintid, type.zora, refereed, title, oa_status.zora, year) %>%
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

orcid_without_zora_table <- function(m,cutoff_year){
  z <- m %>% filter(is.na(eprintid), !is.na(type.orcid), 
                    year >= cutoff_year, type.orcid != "other")
  z <- z %>% 
    select(doi, type.orcid, title, journal, year) %>%
    mutate(doi = ifelse(is.na(doi), "", paste0("<a href='https://www.doi.org/",
                                               doi, "'>", doi, "</a>"))) %>%
    arrange(desc(year))
  DT::datatable(unique(z), extensions = list('Buttons','Scroller','FixedHeader'),
                class = 'cell-border stripe',
                options = list(dom = 'Bfrtip',
                               buttons = c('excel', "csv"),
                               pageLength = 100, fixedHeader = TRUE),
                escape = FALSE, rownames = FALSE)
}


oa_status_diff_zora_unpaywall_table <- function(m,cutoff_year){
  z <- m %>% filter(!is.na(oa_status.unpaywall), 
                    !is.na(oa_status.zora), 
                    oa_status.zora != oa_status.unpaywall,
                    year >= cutoff_year) %>% 
    select(doi, eprintid, oa_status.zora, year,
           oa_status.unpaywall, title, journal) %>%
    arrange(desc(year))
  
  z <- z %>% 
    mutate(oa_status.unpaywall = paste0("<a href='https://api.unpaywall.org/v2/",
                                        doi,"?email=YOUR_EMAIL'>",
                                        oa_status.unpaywall, "</a>")) %>%
    mutate(doi = paste0("<a href='https://www.doi.org/",
                        doi, "'>", doi, "</a>")) %>%
    mutate(eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                             eprintid, "'>", eprintid, "</a>"))
  DT::datatable(z, extensions = 'Buttons',
                options = list(dom = 'Bfrtip',
                               pageLength = 200,
                               buttons = list('copy', 'csv', 'excel')), 
                escape = FALSE, rownames = FALSE)
}
