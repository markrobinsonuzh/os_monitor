#' Barplot data
#'
#' @param tbl_merge data.frame from \code{\link{create_combined_data}} 
#' @param cutoff_year year of cutoff for plotting
#' @param colname column name of year
#' @param title title of plot
#' @param oa_status_used column name of oa status 
#' @param use_plotly logical, if interactive plot with plotly is generated
#'
#' @return ggplot or plotly
#' @export
#' @import rlang
#' @import ggplot2
#' @import plotly
#' @importFrom magrittr %>% 
#'
#' @examples
#' tbl_merge <- tibble::tibble(oa_status=rep(c("gold","closed"),10),
#'                             year=rep(2017:2020,5))
#' # ggplot
#' oa_status_time_plot(tbl_merge)
#' # plotly
#' oa_status_time_plot(tbl_merge, use_plotly = TRUE)
#' 
oa_status_time_plot <- function(tbl_merge, cutoff_year=2000, colname=year, 
                                title="OA Status",
                                oa_status_used=oa_status, use_plotly=FALSE,
                                cutoff_year_upper=2021){
  q_colname <- enquo(colname)
  q_oa_status_used <- enquo(oa_status_used)
  tmp <- tbl_merge %>% dplyr::pull(!!q_oa_status_used) 
  levels(tmp) <- c(levels(tmp),"unknown")
  tmp[is.na(tmp)] <- "unknown"
  tbl_merge <- tbl_merge %>% dplyr::mutate(!!q_oa_status_used := tmp)
  
  tmptib_1 <- tbl_merge %>% 
    dplyr::filter(!!q_colname >= cutoff_year, !!q_colname <= cutoff_year_upper)%>% 
    group_by(!!q_colname,!!q_oa_status_used) %>% 
    summarise(Count=dplyr::n()) %>%
    ungroup() %>% 
    group_by(!!q_colname) %>% 
    mutate(Proportion=Count/sum(Count)) %>% 
    tidyr::pivot_longer(cols = c(Count,Proportion))
  
  # plotly
  if (use_plotly){
    plt_ls <- lapply(c("Count","Proportion"), function(facetting){
    rlang::eval_tidy(
      rlang::quo_squash(
        rlang::quo({
          tmptib_1 %>% filter(name==facetting) %>% 
            plot_ly(x = ~ !!q_colname, y = ~value, 
                    color = ~ !!q_oa_status_used, 
                    colors = open_cols_fn()[names(open_cols_fn()) %in% unique(tmptib_1 %>% pull(!!q_oa_status_used))],
                    hoverinfo="y",
                    legendgroup= ~ !!q_oa_status_used,
                    source = "C") %>%
            add_bars() %>%
            layout(barmode = "stack",
                   title = title,
                   yaxis=list(title="Counts"),
                   xaxis=list(title=""))
        })
      )
    )})
    subplot(plt_ls[[1]], style(plt_ls[[2]],showlegend=FALSE),
    nrows=2,
    shareX = TRUE)
    
    # ggplot
  } else {
    ggplot(tmptib_1, aes(x=!!q_colname,y=value, fill=!!q_oa_status_used)) + 
      geom_col() + facet_grid(rows = vars(name),scales = "free_y")+
      # facet_wrap(~name,scales = "free_y") +
      # theme(axis.text.x = element_text(angle = 90)) +
      ggtitle(title) +
      labs(y="")+
      scale_fill_manual(values=open_cols_fn())
      
  }
}


# #' Table of closed article in Zora
# #'
# #' @param zora data.frame, created from \code{\link{create_zora}}
# #'
# #' @return \code{\link[DT]{datatable}}
# #' @export
# #' @importFrom magrittr %>% 
# #'
# #' @examples
# closed_in_zora_table <- function(zora){
#   z <- zora %>% 
#     dplyr::filter(oa_status == "closed") %>%
#     dplyr::select(doi, eprintid, type, refereed, title, oa_status, year) %>%
#     dplyr::mutate(doi = ifelse(is.na(doi), "", 
#                                paste0("<a href='https://www.doi.org/",
#                                       doi, "' target='_blank'>", doi, "</a>")),
#                   eprintid = paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
#                              eprintid, "' target='_blank'>", eprintid, "</a>")) %>%
#     dplyr::arrange(desc(year))
#   DT::datatable(z, extensions = 'Buttons',
#                 options = list(dom = 'Bfrtip',
#                                pageLength = 200,
#                                buttons = list('copy', 'csv', 'excel')),
#                 escape = FALSE, rownames = FALSE)
# }


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
#' tbl_merge <- tibble::tibble(overall_oa=rep(c("gold","closed"),10),
#'                            year=rep(2017:2020,5))
#' oa_percent_time_table(tbl_merge, 2001)
oa_percent_time_table <- function(m, cutoff_year){
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
#' @param oa_status_zora logical, if zora in tbl_merge
#'
#' @return \code{\link[DT]{datatable}}
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' tbl_merge <- tibble::tibble(overall_oa=rep(c("gold","closed"),10),
#'                            year=rep(2017:2020,5),
#'                            doi=paste0("madeupdoi",1:20),
#'                            oa_status.unpaywall=overall_oa)
#' overall_closed_table(tbl_merge, FALSE)
overall_closed_table <- function(tbl_merge, oa_status_zora = TRUE){
  
  if(oa_status_zora){
    z <- tbl_merge %>%
      dplyr::select(doi, eprintid, overall_oa, oa_status.zora,oa_status.unpaywall, year, dplyr::starts_with("title"))
  } else {
    z <- tbl_merge %>%
      dplyr::select(doi,oa_status.unpaywall, title, year, dplyr::starts_with("title."))
  }
  
  z <- z %>%
    dplyr::mutate(oa_status.unpaywall = ifelse(is.na(oa_status.unpaywall), "",
                                               paste0("<a href='https://api.unpaywall.org/v2/",
                                               doi,"?email=YOUR_EMAIL' target='_blank'>",
                                               oa_status.unpaywall, "</a>")),
                  doi = ifelse(is.na(doi), "", 
                               paste0("<a href='https://www.doi.org/",doi, "' target='_blank'>", doi, "</a>")))
  if(oa_status_zora){
    z <- z %>% dplyr::mutate(eprintid = ifelse(is.na(eprintid), "",
                                    paste0("<a href='https://www.zora.uzh.ch/id/eprint/",
                                           eprintid, "' target='_blank'>", eprintid, "</a>")))
  }
  if("in_scholar" %in% names(tbl_merge)){
    z <- z %>% dplyr::mutate(cid = tbl_merge$cid,
                             cid = ifelse(is.na(cid), "",
                                          paste0("<a href='https://scholar.google.com/scholar?oi=bibs&hl=de&cluster=",
                                                 cid, "' target='_blank'>", cid, "</a>"))) %>% 
      dplyr::select(doi,oa_status.unpaywall, title, year, cid, dplyr::starts_with("title."))
  }
  colns <- stringr::str_replace_all(colnames(z),"\\."," ")
  DT::datatable(z, 
                extensions = c('ColReorder','Buttons', 'Responsive'),
                colnames = colns,
                options = list(dom = 'Blfrtip',
                               # pageLength = 200,
                               # keys=TRUE,
                               colReorder = TRUE,
                               buttons = list('copy', 'csv', 'pdf')),
                escape = FALSE, rownames = FALSE)
}


#' Upset plot
#'
#' @param tbl_merge data.frame from \code{\link{create_combined_data}} 
#'
#' @return \code{\link[UpSetR]{upset}} 
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' tbl_merge <- tibble::tibble(overall_oa=rep(c("gold","closed"),10),
#'                             in_orcid=sample(c(TRUE,FALSE),20,TRUE),
#'                             in_pubmed=sample(c(TRUE,FALSE),20,TRUE))
#' uzhOS::upset_plot(tbl_merge)
upset_plot <- function(tbl_merge){
  if(!requireNamespace("ComplexUpset", quietly = TRUE)){
    tib_plt <- tbl_merge %>%
      dplyr::select( dplyr::starts_with("in_")) %>%
      dplyr::mutate( dplyr::across( dplyr::starts_with("in_"),~as.integer(.x)))
    UpSetR::upset(as.data.frame(tib_plt))
  } else {
    tib_plt <- tbl_merge %>% 
    dplyr::select( dplyr::starts_with("in_"), "overall_oa") %>% 
    dplyr::rename_all(stringr::str_replace, pattern = "in_",replacement = "")
    colnam_used <- colnames(tib_plt)
    ComplexUpset::upset(tib_plt, colnam_used[!stringr::str_detect(colnam_used,"overall_oa")], 
                        wrap=TRUE,
                        name = "",
                        base_annotations=list(
                          'Intersection size'=ComplexUpset::intersection_size(
                            text=list(
                              size=4
                            )
                          )
                        ),
                        annotations = list(
                          'Proportion'=list(
                            aes=aes(x=intersection, fill=overall_oa),
                            geom=list(
                              geom_bar(stat='count', position='fill'),
                              scale_fill_manual(values=open_cols_fn()[unique(tib_plt$overall_oa)])
                            )
                            )),
                        themes=ComplexUpset::upset_modify_themes(
                          list(
                            'intersections_matrix'=theme(text=element_text(size=20))
                          ))
    )
  }
}





#' create simple bar of oa status summary
#'
#' @param overall_oa_status character vector of oa status
#'
#' @return ggplot
#' @export
#'
#' @examples
#' simple_oa_summary_histogram(c("closed","green","gold"))
simple_oa_summary_histogram <- function(overall_oa_status){
  tmpt <- table(overall_oa_status)
  tmpc <- cumsum(tmpt)
  tmpcn <- (tmpc-c(0,tmpc[-length(tmpc)]))/2
  postib <- tibble(x=tmpc-tmpcn,y=1,label=as.integer(tmpt))
  
  ggplot(tibble::tibble(overall_oa=overall_oa_status)) + 
    geom_bar(aes(y="someplaceholder",fill=overall_oa), position = position_stack(reverse = TRUE)) + 
    scale_fill_manual(values=open_cols_fn()) +
    theme_bw() +
    theme(line = element_blank(),rect = element_blank(),text = element_blank(), legend.position = "none") +
    geom_text(data = postib,aes(x=x,y=y,label=label))
}


