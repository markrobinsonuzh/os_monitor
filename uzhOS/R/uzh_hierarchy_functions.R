#' Summarised OA status for all departments
#'
#' @param con postgresql connection
#' @param eprintstablename table name
#' @param subjectstablename table name
#'
#' @return tibble
#' 
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' aou <- all_org_unit_fac(con)
#' 
all_org_unit_fac <- function(con, eprintstablename = "eprints", subjectstablename = "subjects"){
  # eprints joined with subjects
  fac_dep <- tbl(con, eprintstablename) %>% 
    collect()  %>% 
    inner_join(tbl(con, subjectstablename) %>% collect(),
               by="eprintid") %>% 
    group_by(name,parent_name,oa_status,published_doc,date,type) %>% 
    summarise(count=n())%>% 
    rename(year=date, fac=parent_name,dep=name) %>% 
    ungroup() %>% 
    mutate(count=as.double(count),
           published_doc = as.logical(published_doc),
           year=as.integer(year))
  # all expected faculties, departments, etc. to later have complete matrix
  all_expected <- expand.grid(
    dep=unique(fac_dep$dep),
    oa_status=factor(unique(fac_dep$oa_status)),
    year=unique(fac_dep$year),
    type=unique(fac_dep$type),
    count=0,stringsAsFactors = FALSE) %>% 
    tibble::as_tibble()
  # left join with existing data
  fac_dep <- left_join(all_expected,fac_dep, by=c("dep","oa_status","year","type"), suffix=c(".all","")) %>% 
    dplyr::mutate(count = ifelse(is.na(count),count.all,count)) %>% 
    dplyr::select(-count.all) %>% 
    dplyr::mutate(published_doc = ifelse(is.na(published_doc),FALSE,published_doc),
                  oa_status = if_else(published_doc & oa_status=="closed","blue",oa_status),
                  oa_status = factor(oa_status, levels = names(open_cols_fn()))) %>% 
    dplyr::select(-published_doc)
  # summary statistics different levels
  total_fac_dep_year_type <- suppressMessages(fac_dep %>% dplyr::group_by(fac,dep,year,type) %>% dplyr::summarise(fac_dep_year_type_sum=sum(count)))
  total_fac_dep_year <- suppressMessages(fac_dep %>% dplyr::group_by(fac,dep,year) %>% dplyr::summarise(fac_dep_year_sum=sum(count)))
  total_fac_dep <- suppressMessages(fac_dep %>% dplyr::group_by(fac,dep) %>% dplyr::summarise(fac_dep_sum=sum(count)))
  total_fac <-  suppressMessages(fac_dep %>% dplyr::group_by(fac) %>% dplyr::summarise(fac_sum=sum(count)))
  total_dep <-  suppressMessages(fac_dep %>% dplyr::group_by(dep) %>% dplyr::summarise(dep_sum=sum(count)))
  fac_dep <- suppressMessages(fac_dep %>% dplyr::inner_join(total_fac_dep) %>% 
                                dplyr::inner_join(total_fac) %>% 
                                dplyr::inner_join(total_dep) %>% 
                                dplyr::inner_join(total_fac_dep_year) %>% 
                                dplyr::inner_join(total_fac_dep_year_type))
  return(fac_dep)
}
  
  
#' unique faculties and/or departments
#'
#' @param fac_dep_filt tibble, output from \code{\link{all_org_unit_fac}}
#' @param type one of "fac","dep","fac_dep"
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
unique_fac_dep <- function(fac_dep_filt, type=c("fac","dep","fac_dep")){
  type <- match.arg(type)
  if(type == "fac"){
    return(unique(fac_dep_filt$fac))
  } else if (type == "dep"){
    return(unique(fac_dep_filt$dep))
  } else if (type == "fac_dep"){
    return(fac_dep_filt %>% dplyr::select(fac,dep) %>% unique())
  }
}




#' plot faculties or department summary of OA status
#'
#' @param fac_filt_wide_hk tibble with plotly 'highlight_key'
#' @param fac_dep_filt tibble, output from \code{\link{all_org_unit_fac}}
#' @param plot_type what plot to return , one of "anim_year","dep_year","year_val_line","year_val_bar"
#'
#' @return future of plotly
#' 
#' @export
#' @importFrom magrittr %>% 
#' @import plotly
#' @import ggplot2
#'
#' @examples
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' fac_dep_filt <- all_org_unit_fac(con)
#' vdep <- c("Department of Anthropology","Department of Biochemistry",
#'           "Department of Chemistry","Department of Molecular Mechanisms of Disease")
#' fac_filt_long <- preprocess_fac_dep(fac_dep_filt,vdep,"dep",by_year = TRUE)
#' fac_filt_wide_hk <- fac_filt_long %>%
#'   tidyr::pivot_wider(names_from=type,values_from=value) %>%
#'   group_by(dep,oa_status) %>%
#'   summarise(TotalCount=sum(TotalCount),Count=sum(Count)) %>%
#'   mutate(Proportion=Count/TotalCount,
#'          dep=factor(dep)) %>%
#'   highlight_key()
#' f <- plot_fac_dep(fac_filt_wide_hk, fac_filt_long, "year_val_bar")
#' value(f)
plot_fac_dep <- function(fac_filt_wide_hk,fac_dep_filt, 
                         plot_type=c("anim_year","dep_year","year_val_line","year_val_bar")){
  plot_type <- match.arg(plot_type)
  dep_choosen <- fac_filt_wide_hk$data() %>% dplyr::pull(dep) %>% unique()
  fac_filt_wide_aggr <- fac_filt_wide_hk$data() %>% 
    group_by(oa_status,dep) %>% summarise(Count=sum(Count), TotalCount=sum(TotalCount)) %>% 
    mutate(Proportion=Count/TotalCount) %>% 
    arrange(dep)
  oa_status_in_df <- as.character(unique(fac_filt_wide_aggr$oa_status))
  orderings_oa <- lapply(oa_status_in_df, function(oa_tmp){
    fac_filt_wide_aggr %>% dplyr::filter(oa_status==oa_tmp) %>% dplyr::arrange(Proportion) %>% dplyr::pull(dep)
  })
  names(orderings_oa) <- oa_status_in_df
  switch(plot_type,
         #######################################################################
         # animated over years
         anim_year = {
           future({
             fac_filt_wide <- fac_filt_wide_hk$data()
           base_plt <- plot_ly(fac_filt_wide,source = "bar_plot") 
           steps <- list()
           all_years <- unique(fac_filt_wide_hk$data()$year)
           all_oa_status <- as.character(unique(fac_filt_wide_hk$data()$oa_status))
           for(i in seq_along(all_years)){
               step <- list(args = list('visible', rep(FALSE, length(all_years)*5)),
                            label=all_years[i],
                            method = 'restyle')
               step$args[[2]][(((i-1)*5)+1):(((i-1)*5+4)+1)] <-  TRUE  
               steps[[i]] <-  step 
           }
           plt_ls <- lapply(c("Count","Proportion"), function(facetting){
             rlang::eval_tidy(
               rlang::quo_squash(
                 rlang::quo({
                   fac_filt_wide_wide <- fac_filt_wide %>% tidyr::pivot_wider(values_from = !!facetting,
                                                                       names_from = year, 
                                                                       id_cols=c("dep","oa_status"),
                                                                       names_prefix="P") %>% 
                     dplyr::mutate(dplyr::across(dplyr::starts_with("P2"),~ifelse(is.na(.x),0,.x)))
                   
                   tmp_base_plt <-  plot_ly(fac_filt_wide_wide,source = "bar_plot") 
                   for(i in seq_along(all_years)){
                     is_visible <- ifelse(all_years[i]==2020,rep(TRUE,5),rep(FALSE,5))
                     tmp_base_plt <- tmp_base_plt %>% 
                       add_trace(type = "bar",
                                # x = ~!!sym(facetting),
                                x = as.formula(paste0("~`P", all_years[i],"`")),
                                y = ~dep, 
                                name= ~oa_status,
                                legendgroup= ~ oa_status,
                                showlegend=FALSE,
                                hoverinfo="x+name",
                                visible=is_visible,
                                ids = ~dep,
                                color = ~ oa_status, 
                                colors = open_cols_fn()[names(open_cols_fn()) %in% unique(fac_dep_filt %>% pull(oa_status))]
                       ) 
                   }
                   tmp_base_plt%>%
                            layout(barmode = "stack",
                                   title = title,
                                   yaxis=list(title="",categoryorder = "array",categoryarray=all_oa_status),
                                   xaxis=list(title=!!facetting,range = c(0, ifelse(!!facetting=="Proportion",1,
                                                                                    max(fac_filt_wide %>% dplyr::pull(TotalCount))))),
                                   margin = list(l = 300,r = 50,b = 100,t = 100,pad = 20))
                   })))})
           subplot(plt_ls,nrows=1,titleX = TRUE,shareY = TRUE) %>%
                              layout(sliders = list(list(active = 20,
                                                         currentvalue = list(prefix = "Year: "),
                                                         steps = steps,
                                                         pad=list(t=40))),
                                     updatemenus = list(
                                       list(
                                         x = 0.3,
                                         y = 1.2,
                                         buttons=lapply(oa_status_in_df, function(order_name){
                                           list(
                                             label = order_name,
                                             method = "relayout",
                                             args = list(list(yaxis=list(categoryarray=(orderings_oa[[order_name]]),
                                                                         automargin=TRUE))))
                                         }))
                                     ))
           })
         },
         #######################################################################
         # aggreaggated over years
         dep_year = {
           future({
           prop_plt <- fac_filt_wide_aggr %>% 
             plot_ly(x = ~Proportion, 
                     y = ~dep, 
                     color = ~ oa_status, 
                     # frame = ~ year,
                     colors = open_cols_fn()[names(open_cols_fn()) %in% unique(fac_dep_filt %>% pull(oa_status))],
                     hoverinfo="x",
                     legendgroup= ~ oa_status,
                     type = "bar",
                     source = "bar_plot"
             ) %>%
             layout(barmode = "stack",
                    title = title,
                    yaxis=list(title=""),
                    xaxis=list(title="Proportion",range = c(0, 1)),
                    margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20)
                    )
           count_plt  <- fac_filt_wide_aggr %>% 
             plot_ly(x = ~Count, 
                     y = ~dep, 
                     color = ~ oa_status, 
                     # frame = ~ year,
                     colors = open_cols_fn()[names(open_cols_fn()) %in% unique(fac_dep_filt %>% pull(oa_status))],
                     hoverinfo="x",
                     legendgroup= ~ oa_status,
                     type = "bar",
                     source = "bar_plot",
                     showlegend=FALSE
             ) %>%
             layout(barmode = "stack",
                    title = title,
                    yaxis=list(title=""),
                    xaxis=list(title="Count"),
                    margin = list(l = 200, r = 50, b = 100, t = 100, pad = 20))
           
           subplot(count_plt,prop_plt,nrows=1,titleX = TRUE,shareY = TRUE)  %>% 
             layout(updatemenus = list(
               list(
                 x = 0.3,
                 y = 1.2,
                 buttons=lapply(oa_status_in_df, function(order_name){
                   list(
                     label = order_name,
                     method = "relayout",
                     args = list(list(yaxis=list(categoryarray=(orderings_oa[[order_name]]),
                                                 automargin=TRUE))))
                 }))
               ),
               annotations=list(list(text = "Sort<br>by", x=-0.1, y=1.2, xref='paper', yref='paper', showarrow=FALSE)))
           })
         },
         #######################################################################
         # lineplot over year
         year_val_line = {
           future({
           tmp <- fac_filt_wide_hk$data() %>% highlight_key(~oa_status)
           plt_ls <- lapply(c("Count","Proportion"), function(facetting){
             rlang::eval_tidy(
               rlang::quo_squash(
                 rlang::quo({
                  plot_ly(tmp,
                         x = ~year, 
                         y = ~!!sym(facetting), 
                         text = ~oa_status,
                         hoverinfo="x+y+text",
                         name= ~ oa_status,
                         legendgroup= ~ oa_status,
                         source = "bar_plot"
           )})))})
           colplts <- lapply(1:2, function(i){
             pc_ls <- lapply(dep_choosen, function(dep_c){
               plt_ls[[i]] %>% filter(dep==dep_c) %>% 
                  add_lines(
                    color = ~ oa_status, 
                    colors = open_cols_fn()[names(open_cols_fn()) %in% unique(fac_dep_filt %>% pull(oa_status))]
                  ) %>% 
                 layout(
                   barmode = "stack",
                   yaxis=list(title=""),
                   annotations=list(
                     list(
                       x = -0.07, 
                       y = 0.5, 
                       showarrow = FALSE, 
                       text = ifelse(i==1,dep_c,""), 
                       xref = "paper", 
                       yref = "paper",
                       xanchor="right",
                       align="right")),
                   margin=list(l=400)
                 )
             })
             subplot(pc_ls,nrows = length(dep_choosen),titleY = TRUE) %>% 
               layout(showlegend=FALSE)
           })
           subplot(colplts,margin=0.05) 
           })
         },
         #######################################################################
         # barplot over year
         year_val_bar = {
           future({
             # margins on the left (for labels)
             lmargin <- max(stringr::str_length(dep_choosen))*6
             # loop through Counts and Proportions
             plt_ls <- lapply(c("Count","Proportion"), function(facetting){
               # functions for substitution of 'facetting'
               rlang::eval_tidy(
                 rlang::quo_squash(
                   rlang::quo({
                     # loop over departments
                     pc_ls <- lapply(dep_choosen, function(dep_c){
                       plot_ly(fac_filt_wide_hk$data() %>% filter(dep==dep_c),
                               x = ~year, 
                               y = ~!!sym(facetting), 
                               text = ~oa_status,
                               hovertemplate = ifelse(!!facetting=="Count",
                                                      paste('year: %{x} <br> %{text}: %{y}'),
                                                      paste('year: %{x} <br> %{text}: %{y:.2f}')),
                               legendgroup= ~ oa_status,
                               showlegend=ifelse(dep_c==dep_choosen[1] && !!facetting == "Count",TRUE,FALSE),
                               source = "bar_plot",
                               type="bar",
                               height = 50+80*length(dep_choosen),
                               color = ~ oa_status, 
                               colors = open_cols_fn()[names(open_cols_fn()) %in% unique(fac_dep_filt %>% pull(oa_status))])  %>% 
                         layout(
                           barmode = "stack",
                           yaxis=list(title=""),
                           annotations=list(
                             list(
                               x = -0.1,
                               y = 0.5,
                               showarrow = FALSE,
                               text = ifelse(!!facetting=="Count",dep_c,""),
                               xref = "paper",
                               yref = "paper",
                               xanchor="right",
                               align="right")),
                           margin=list(l=lmargin,r=0,t=10,b=10,pad=4)
                         )
                     })
                     subplot(pc_ls,nrows = length(dep_choosen),titleY = TRUE,shareX = TRUE,titleX=FALSE,
                             margin=0.01-(length(dep_choosen)/50*0.01))
                   })))})
             subplot(plt_ls,margin=0.05)
           }
         )
    })

}

#' Title
#'
#' @param fac_dep_filt tibble from \code{\link{all_org_unit_fac}}
#' @param fac_chosen character vector of departments and faculties
#' @param col_to_plot which column of fac_dep_filt to plot
#' @param oa_status_filter character vector of oa status to include
#' @param by_year stratify by year
#' @param publication_filter what publications to include, default = "all".
#'
#' @return
#' @export
#'
#' @examples
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' fac_dep_filt <- all_org_unit_fac(con)
#' vdep <- c("Department of Anthropology","Department of Biochemistry",
#'           "Department of Chemistry","Department of Molecular Mechanisms of Disease")
#' fac_filt_long <- preprocess_fac_dep(fac_dep_filt,vdep,"dep",by_year = TRUE)
preprocess_fac_dep <- function(fac_dep_filt, fac_chosen, col_to_plot , 
                               oa_status_filter = c("closed","hybrid","green","gold","blue"), 
                               by_year=FALSE,
                               publication_filter="all"){
  if (by_year){
    col_to_plot <- c(col_to_plot,"year")
  }
  if (publication_filter=="all"){
    publication_filter <- unique(fac_dep_filt$type)
  }
  fac_dep_filt %>%
    dplyr::filter(dep %in% fac_chosen, type %in% publication_filter) %>%
    dplyr::group_by(!!!rlang::syms(col_to_plot),oa_status) %>%
    dplyr::summarise(Count=sum(count)) %>%
    dplyr::ungroup() %>%
    dplyr:: group_by(!!!rlang::syms(col_to_plot)) %>%
    dplyr::summarise(TotalCount=sum(Count),oa_status=oa_status, Count=Count) %>% 
    dplyr::mutate(Proportion=ifelse(TotalCount==0,0,Count/TotalCount)) %>% 
    dplyr::filter(oa_status %in% oa_status_filter) %>% 
    dplyr::arrange(!!!rlang::syms(col_to_plot)) %>% 
    tidyr::pivot_longer(cols = c("Count","Proportion"),names_to="type") %>% 
    ungroup()
}

#' @examples
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' fac_dep_filt <- all_org_unit_fac(con)
#' vdep <- c("Department of Anthropology","Department of Biochemistry",
#'           "Department of Chemistry","Department of Molecular Mechanisms of Disease")
#' # by year
#' fac_filt_long <- preprocess_fac_dep(fac_dep_filt,vdep,"dep",by_year = TRUE)
#' arranged_fac_filt_long <- arrange_fac_dep(fac_filt_long, type_arr = "Count", by_year = TRUE)
#' # not by year
#' fac_filt_long <- preprocess_fac_dep(fac_dep_filt,vdep,"dep",by_year = FALSE)
#' arranged_fac_filt_long <- arrange_fac_dep(fac_filt_long, type_arr = "Count", by_year = FALSE)
arrange_fac_dep <- function(fac_filt_long, arrange_by="closed",
                            type_arr=c("Count","Proportion"), col_to_plot="dep", by_year=FALSE){
  type_arr <- match.arg(type_arr)
  
  # order
  if(by_year){
    order_fac <- fac_filt_long %>% 
      group_by(!!sym(col_to_plot), oa_status, type) %>% 
      summarise(value=sum(value)) %>% 
      filter(oa_status==arrange_by, type==type_arr) %>% 
      dplyr::arrange(desc(value)) %>% 
      pull(!!sym(col_to_plot))
  } else {
    order_fac <- fac_filt_long %>% 
      filter(oa_status==arrange_by, type==type_arr) %>% 
      dplyr::arrange(desc(value)) %>% 
      pull(!!sym(col_to_plot))
  }

  all_fac <- fac_filt_long %>% pull(!!sym(col_to_plot)) %>% unique()
  order_fac <- c(order_fac, all_fac[!(all_fac %in% order_fac)])
  
  mat_oa <- order(match(fac_filt_long$dep,order_fac))
  fac_filt_long <- fac_filt_long[mat_oa,]
  
  
  fac_filt_long %>% mutate(!!sym(col_to_plot) := factor(!!sym(col_to_plot),order_fac)) %>% 
    dplyr::mutate(dep=as.character(dep))
}

