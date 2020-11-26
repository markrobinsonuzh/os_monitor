#' key of oa status, defining plot color
#'
#' @return list of keypairs
#' @export
#'
#' @examples
#' open_cols_fn()
open_cols_fn <- function(){
  c("closed" = "gray48", "hybrid" = "darkorange1",
    "green" = "chartreuse4", "gold" = "gold",
    "preprint" = "red", "bronze" = "darkgoldenrod4",
    "blue" = "blue", 
    "unknown"="gray90") 
}

#' @export
oa_status_order <- function(){
  c("closed", "preprint", "bronze", "blue","hybrid", "green","gold")
}


#' create_tbl_author
#' 
#' @param author_vec author name
#' @param con postgresql connection
#' @param authorstablename table name
#' @param authorkeystablename table name
#' @param eprintstablename table name
#' @param subjectstablename table name
#' @param fac_vec character vector of faculties to filter
#' @param dep_vec character vector of departments to filter
#'
#' @return tbl_author
#' 
#' @importFrom magrittr %>%
#' @import DBI
#' @export
#'
#' @examples
#' author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' create_tbl_author(author_vec, con)
#' # filter faculty
#' create_tbl_author(author_vec, con, fac_vec = "07 Faculty of Science")
#' # filter department
#' create_tbl_author(author_vec,con, dep_vec = "Evolution in Action: From Genomes to Ecosystems" )
create_tbl_author <- function(author_vec, con, authorstablename = "authors", authorkeystablename = "authorkeys", 
                              eprintstablename = "eprints", subjectstablename = "subjects", fac_vec=NULL, dep_vec=NULL){
  sql_con_cont(con)
  # author info
  tbl_author <- tbl(con, authorstablename) %>% 
    filter(authorkey_fullname %in% author_vec) %>% 
    select(eprintid,authorkey_fullname)  
  
  # eprints
  tbl_eprints <- tbl(con, eprintstablename) 
    
  # combine all
  tbl_author <-
    tbl_author %>%
    dplyr::left_join(tbl_eprints,by="eprintid") %>% 
    inner_join(tbl(con, authorkeystablename) ,
               by="authorkey_fullname") %>% 
    inner_join(tbl(con, subjectstablename) ,
               by="eprintid") %>%
    collect() %>% 
    dplyr::mutate(year = as.integer(date),
                  doi = tolower(doi)) %>% 
      dplyr::group_by(doi) %>% 
      dplyr::mutate(name=list(name), 
                    parent_name=list(parent_name),
                    parent=list(parent),
                    subjects=list(subjects)) %>% 
      unique()
  
  # filter by department if 'dep_vec' is given, first create filter expression
  if (!is.null(fac_vec) | !is.null(dep_vec)){
    if(!is.null(fac_vec) & is.null(dep_vec)){
      tmpquo_ls_fac <- lapply(fac_vec, function(fac) expr(!!fac %in% unlist(.data[["parent_name"]])))
      tmpquo_fac <- purrr::reduce(tmpquo_ls_fac,function(x,y) expr(!!x|!!y), .init = FALSE)
    }
    if(!is.null(dep_vec)){
      tmpquo_ls_dep <- lapply(dep_vec, function(dep) expr(!!dep %in% unlist(.data[["name"]])))
      tmpquo_dep <- purrr::reduce(tmpquo_ls_dep,function(x,y) expr(!!x|!!y), .init = FALSE)
    }
    if (exists("tmpquo_fac") & exists("tmpquo_dep")){
      tmpquo <- quo(!!tmpquo_fac | !!tmpquo_dep)
    } else if (exists("tmpquo_fac")){
      tmpquo <- tmpquo_fac
    } else if (exists("tmpquo_dep")){
      tmpquo <- tmpquo_dep
    }
    # second, apply filter expression
    tbl_author <- tbl_author %>% filter(!!tmpquo)
  }
  return(tbl_author)
} 

#' @importFrom magrittr %>% 
#' @export
empty_zora <- function(){
  tibble::tibble(eprintid = integer(),
                 authorkey_fullname = character(),
                 doi = character(),
                 date = character(),
                 title = character(),
                 refereed = character(),
                 institution = character(),
                 oa_status = factor(levels = names(open_cols_fn())),
                 published_doc = logical(),
                 authorkey = character(),
                 authroname = character(),
                 year = integer(),
                 in_zora = logical()) %>% 
    as_tibble_reac(name="zora")
}

#' wraper for create_tbl_author, and add "blue" oa status
#'
#' @param author_vec author name
#' @param con postgresql connection
#' @param authorstablename table name
#' @param authorkeystablename table name
#' @param eprintstablename table name
#' @param subjectstablename table name
#' 
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#' @import DBI
#' 
#' @examples
# author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
# con <- dbConnect(odbc::odbc(), "PostgreSQL")
# create_zora(author_vec,con)
create_zora <- function(author_vec, con, authorstablename = "authors", authorkeystablename = "authorkeys", 
                        eprintstablename = "eprints", subjectstablename = "subjects"){
  sql_con_cont(con)
  dept_fac <- create_tbl_author(author_vec, con, authorstablename, authorkeystablename, 
                                eprintstablename, subjectstablename, fac_vec=NULL, dep_vec=NULL)
  zora <- dept_fac %>%
    dplyr::mutate(in_zora=TRUE) %>%
    # dplyr::filter(authorkey_fullname %in% c(author_vec)) %>%
    dplyr::select(-name, -parent_name,-subjects,-parent) %>% 
    unique()
  
  if (dim(zora)[1]!=0){
    # add blue OA
    zora$oa_status[zora$published_doc & zora$oa_status=="closed"] <- "blue" 
    zora$oa_status <- factor(zora$oa_status, levels = names(open_cols_fn()))
  }
  
  return(zora)
}


#' department, faculty and full name of author
#'
#' @param author_vec author name
#' @param con postgresql connection
#' @param authorstablename table name
#' @param authorkeystablename table name
#' @param eprintstablename table name
#' @param subjectstablename table name
#' @param fac_vec character vector of faculties to filter
#' @param dep_vec character vector of departments to filter
#' 
#' @return list of elements "org_unit", "fac" and "author_name",
#' with author_name a list with elements "family" and "given"
#' @export
#' @import DBI
#' @importFrom magrittr %>% 
#'
#' @examples
#' author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' org_unit_fac(author_vec,con)
org_unit_fac <- function(author_vec, con, authorstablename = "authors", 
                         authorkeystablename = "authorkeys", eprintstablename = "eprints", 
                         subjectstablename = "subjects", fac_vec=NULL, dep_vec=NULL){
  sql_con_cont(con)
  tbl_author <- create_tbl_author(author_vec, con, authorstablename, authorkeystablename, 
                                  eprintstablename,subjectstablename, fac_vec, dep_vec)
  if (dim(tbl_author)[1] == 0){
    return(list(org_unit=NULL,fac=NULL,author_name=NULL))
  }
  filter_name <- c("name","parent_name","type")
  name_name <- c("dept", "fac", "type")
  out_ls <- lapply(seq_len(3), function(i){
    suppressMessages(tbl_author %>% 
                       dplyr::select(!!filter_name[i]) %>% 
                       dplyr::group_by(!!filter_name[i]) %>% 
                       dplyr::pull(!!filter_name[i]) %>% 
                       unlist() %>% 
                       tibble::as_tibble() %>% 
                       dplyr::group_by(value) %>% 
                       dplyr::tally() %>% 
                       dplyr::arrange(dplyr::desc(n)) %>% 
                       dplyr::rename(!!name_name[i]:=value,count=n))
  })
  names(out_ls) <- c("org_unit","fac","type")
  out_ls[["author_name"]] <- author_vec
  return(out_ls)
}

#' affiliation of aliases from author search
#'
#' @param authorname author id
#' @param con postgresql connection
#' @param authorstablename table name
#' @param authorkeystablename table name
#' @param eprintstablename table name
#' @param subjectstablename table name
#' @param fac_vec character vector of faculties to filter
#' @param dep_vec character vector of departments to filter
#' 
#' @return
#' @export
#' @import DBI
#' @importFrom magrittr %>% 
#'
#' @examples
#' authorname <- "robinson mark d"
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' pot_alias_and_affil(authorname, con)
pot_alias_and_affil <- function(authorname, con, authorstablename = "authors", 
                                authorkeystablename = "authorkeys", eprintstablename = "eprints", 
                                subjectstablename="subjects",fac_vec=NULL, dep_vec=NULL){
  sql_con_cont(con)
  tmp_author <- authorname
  pot_aliases <- tbl(con, authorkeystablename) %>% filter(authorname %in% tmp_author) %>% collect() %>% pull(authorkey_fullname)
  pot_affil <- lapply(pot_aliases, function(pot_alias){
    org_unit_fac(pot_alias, con, authorstablename, authorkeystablename, eprintstablename, subjectstablename,fac_vec,dep_vec)
  })
  names(pot_affil) <- pot_aliases
  return(list(pot_aliases=pot_aliases, pot_affil=pot_affil))
}


#' Upset selection indexes
#'
#' @param tbl_merge tibble from combining data
#' @param in_selection column names where TRUE
#' @param not_in_selection column names where FALSE
#'
#' @return vector of indices
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
upset_selection <- function(tbl_merge,in_selection,not_in_selection){
  ind_1 <- tbl_merge %>%
    dplyr::select(!!!rlang::syms(in_selection)) %>%
    purrr::reduce(.f=function(x,y){x&y}) 
  ind_2 <- tbl_merge %>%
    dplyr::select(!!!rlang::syms(not_in_selection)) %>%
    purrr::reduce(.f=function(x,y){x&!y}, .init = TRUE) 
  return(ind_1 & ind_2)
}



