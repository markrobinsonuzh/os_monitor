#' Summarised OA status for all departments
#'
#' @param tbl_eprints mongodb connection
#'
#' @return tibble
#' @export
#' @import mongolite
#' @importFrom magrittr %>% 
#'
#' @examples
#' 
#' 
all_org_unit_fac <- function(con, eprintstablename = "eprints", subjectstablename = "subjects"){
  fac_dep <- tbl(con, eprintstablename) %>% 
    collect()  %>% 
    inner_join(tbl(con, subjectstablename) %>% collect(),
               by="eprintid") %>% 
    group_by(name,parent_name,oa_status,published_doc,date,type) %>% 
    summarise(count=n())%>% 
    rename(year=date, fac=parent_name,dep=name) %>% 
    ungroup() %>% 
    mutate(count=as.double(count),
           published_doc = as.logical(published_doc))
  fac_dep <- fac_dep %>% 
    dplyr::mutate(oa_status = if_else(published_doc & oa_status=="closed","blue",oa_status),
           oa_status= factor(oa_status, levels = names(open_cols_fn()))) %>% 
    dplyr::select(-published_doc)
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
  
  fac_dep_filt <- fac_dep %>% dplyr::filter(!(stringr::str_detect(fac_dep$fac,"[:digit:]{4}") | stringr::str_detect(fac_dep$dep,"[:digit:]{4}")))
  return(fac_dep_filt)
}
  
  
#' unique faculties and/or departments
#'
#' @param fac_dep_filt tibble, output from \code{\link{all_org_unit_fac}}
#' @param type one of "fac","dep","fac_dep"
#'
#' @return
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
#' @param fac_dep_filt tibble, output from \code{\link{all_org_unit_fac}}
#' @param fac_chosen Null if comparing faculties, otherwise one of 
#'   \code{\link{unique_fac_dep}}(fac_dep_filt,"dep")
#' @param oa_status_filter which OA status to include, default is all: 
#'   c("closed","hybrid","green","gold","blue")
#'
#' @return
#' @export
#' @importFrom magrittr %>% 
#' @import ggplot2
#'
#' @examples
plot_fac_dep <- function(fac_dep_filt, fac_chosen = "University of Zurich",
                         oa_status_filter = c("closed","hybrid","green","gold","blue"),
                         by_year=FALSE, arrange_by="closed",
                         publication_filter="all"){
  
  if (all(is.na(fac_chosen))){
    return(ggplot() + geom_blank())
  }
  # if (is.null(fac_chosen)){
  #   col_to_plot <- "fac"
  #   fac_chosen <- unique_fac_dep(fac_dep_filt, "fac")
  # } else {
  #   col_to_plot <- "dep"
  #   stopifnot(fac_chosen %in% unique_fac_dep(fac_dep_filt, "fac"))
  # }
  col_to_plot <- "dep"
  
  fac_filt <- preprocess_fac_dep(fac_dep_filt,fac_chosen, col_to_plot, oa_status_filter, by_year, publication_filter)
  
  fac_filt_long <- fac_filt %>% 
    tidyr::pivot_longer(cols = c("Count","Proportion"),names_to="type") %>% 
    ungroup()
  
  # order
  order_fac <- fac_filt %>% filter(oa_status==arrange_by) %>% arrange(desc(Proportion)) %>% 
    pull(!!sym(col_to_plot))
  all_fac <- fac_filt %>% pull(!!sym(col_to_plot)) %>% unique()
  order_fac <- c(order_fac, all_fac[!(all_fac %in% order_fac)])
  fac_filt_long <- fac_filt_long %>% mutate(!!sym(col_to_plot) := factor(!!sym(col_to_plot),order_fac)) 
  
  if (by_year){
    ggplot(fac_filt_long %>% filter(type=="Proportion")) +
      geom_col(aes(year,x=value,fill=oa_status),position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values=open_cols_fn()) +
      facet_wrap(~fac)
      # facet_grid(rows=vars(year)) #,cols = vars(year))
      # facet_wrap(!!sym(col_to_plot)~type,scales = "free_x") +
      # theme(axis.title.x = element_blank(), 
      #       axis.title.y = element_blank()) + 
      # labs(fill="OA status")
  } else {
    ggplot(fac_filt_long) +
      geom_col(aes(!!sym(col_to_plot),x=value,fill=oa_status),position = position_stack(reverse = TRUE))+
      scale_fill_manual(values=open_cols_fn()) +
      facet_wrap(~type,scales = "free_x") +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank()) + 
      labs(fill="OA status")
  }
}



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
    dplyr::mutate(Proportion=Count/sum(Count)) %>% 
    dplyr::filter(oa_status %in% oa_status_filter) %>% 
    dplyr::arrange(!!!rlang::syms(col_to_plot))
}


# fac_dep_filt <- all_org_unit_fac(tbl_eprints)
# dep_chosen <- unique_fac_dep(fac_dep_filt,"fac_dep")
# plot_fac_dep(fac_dep_filt, fac_chosen = "07 Faculty of Science",arrange_by = "gold")



