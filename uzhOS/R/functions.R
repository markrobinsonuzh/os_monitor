#' Check db connection 
#'
#' @param con DBI connection object
#'
#' @export
sql_con_cont <- function(con){
  if (!(is(con,"PqConnection") | is(con,"PostgreSQL"))){
    stop("'con' is no valid connection of type 'RPostgres::`PqConnection-class` or 'OdbcConnection PostgreSQL'.", .call=FALSE)
  }
}

#' fetch doi entries from local unpaywall
#'
#' @param dois dois to get open access status from
#' @param con postgresql connection
#' @param unpaywalltablename table name
#'
#' @return open access status for each doi
#' @export
#' @importFrom tibble tibble
#' @importFrom magrittr %>% 
#' @import DBI
#'
#' @examples 
#' con <- dbConnect(odbc::odbc(), "PostgreSQL")
#' oadoi_fetch_local("10.1177/000271625529700159",con)
oadoi_fetch_local <- function(dois, con, unpaywalltablename = "unpaywall"){
  sql_con_cont(con)
  dois <- tolower(dois)
  oaf <- tbl(con, unpaywalltablename) %>% 
    filter(doi %in% dois) %>% 
    collect() %>% 
    mutate(oa_status = factor(stringr::str_trim(oa_status),levels = names(open_cols_fn())))
  if(length(oaf)==0){
    return(tibble::tibble(doi=character(),oa_status=factor(levels = names(open_cols_fn()))))
  } else {
    return(oaf)
  }
}


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

#' create combined data from zora, orcid, pubmed, publons and unpaywall
#'
#' @param df_orcid tibble from \code{\link{retrieve_from_orcid}}
#' @param df_pubmed tibble from \code{\link{retrieve_from_pubmed}}
#' @param df_zora tibble from \code{\link{create_zora}}
#' @param df_publons tibble from \code{\link{retrieve_from_publons}}
#' @param con postgresql connection
#' @param unpaywalltablename table name
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
create_combined_data <- function(df_orcid, df_pubmed, df_zora, df_publons, con, unpaywalltablename = "unpaywall"){
  ## orcid and zora
  # if df_orcid is given
  if (!(is.null(df_orcid) || dim(df_orcid)[1]==0) && !(is.null(df_zora) || dim(df_zora)[1]==0)){
    m <- dplyr::full_join(df_orcid %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          df_zora %>% 
                            dplyr::mutate(doi=tolower(doi)) %>%
                            dplyr::as_tibble(), 
                          by="doi", suffix=c(".orcid",".zora"),
                          na_matches="never")
    m$doi[m$doi=="logical(0)"] <- NA
  } else if(!(is.null(df_orcid) || dim(df_orcid)[1]==0)) {
    m <- df_orcid %>% 
      dplyr::mutate(doi=tolower(doi)) %>% 
      dplyr::as_tibble()
    if("oa_status" %in% names(m)){
      m <- m %>% dplyr::rename(oa_status.orcid=oa_status)
    }
    if("title" %in% names(m)){
      m <- m %>% dplyr::rename(title.orcid=title)
    }
    if("type" %in% names(m)){
      m <- m %>% dplyr::rename(type.orcid=type)
    }
    if("year" %in% names(m)){
      m <- m %>% dplyr::rename(year.orcid=year)
    }
  # rename some columns for consistency if df_orcid not given
  } else {
    m <- df_zora %>% 
      dplyr::mutate(doi=tolower(doi)) %>% 
      dplyr::rename(year.zora=year) %>% 
      dplyr::mutate(title.zora=title) %>% 
      dplyr::as_tibble()
  }
  
  ## pubmed
  # if df_pubmed is given, join
  if (!(is.null(df_pubmed) || dim(df_pubmed)[1]==0)){
    m <- dplyr::full_join(m, 
                          df_pubmed %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          by="doi", suffix = c("", ".pubmed"),
                          na_matches="never")
    if("title" %in% names(m) && !"title.pubmed" %in% names(m)){
      m <- m %>% dplyr::rename(title.pubmed=title)
    }
  }
  
  ## publons
  # if df_publons is present, join and rename
  if (!(is.null(df_publons) || dim(df_publons)[1]==0)){
    m <- dplyr::full_join(m, 
                          df_publons %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          by="doi", suffix = c("", ".publons"),
                          na_matches="never")
    if("title" %in% names(m) && !"title.publons" %in% names(m)){
      m <- m %>% dplyr::rename(title.publons=title)
    }
  }
  
  if("title.zora" %in% names(m)){
    m <- m %>% dplyr::mutate(title=title.zora)
  } else {
    tmptitle <- m %>% dplyr::select(!!dplyr::starts_with("title")[1]) %>% dplyr::pull()
    m <- m %>% dplyr::mutate(title=tmptitle)
  }
  
  #### OA Status ---------------------------------------------------------------
  # get oa status from unpaywall
  oaf <- oadoi_fetch_local(unique(na.omit(m$doi)), con, unpaywalltablename)
  m <- m %>% dplyr::full_join(oaf, 
                       by = "doi", suffix=c(".zora", ".unpaywall"))
  if(!("in_zora" %in% names(m))){
    m <- m %>% dplyr::rename(oa_status.unpaywall=oa_status)
  }
  
  # overall oa status from unpaywall
  # m$overall_oa <- factor(m$oa_status.unpaywall, levels = names(open_cols_fn()))
  m$overall_oa <- m$oa_status.unpaywall
  m$overall_oa <- factor(m$overall_oa, levels = names(open_cols_fn()))
  
  # preprints from orcid
  if (!is.null(df_orcid)){
    m$overall_oa[m$type.orcid=="other"] <- "preprint"
  }
  
  # other oa status from zora
  w <- is.na(m$overall_oa)
  if("oa_status.zora" %in% names(m)){
    m$overall_oa[w] <- m$oa_status.zora[w]
  }
  
  # if closed but blue in zora: blue overall
  w <- m$overall_oa == "closed" & m$oa_status.zora=="blue"
  m$overall_oa[w] <- "blue"
  
  # if not closed in zora and closed in unpaywall, set overall oa to oa from zora
  w <- m$oa_status.zora != "closed" & m$oa_status.unpaywall == "closed"
  w[is.na(w)] <- FALSE
  m$overall_oa[w] <- m$oa_status.zora[w]
  
  # set missing oa as "unknown"
  w <- is.na(m$overall_oa)
  m$overall_oa[w] <- "unknown"
  
  ####  Other    ---------------------------------------------------------------
  # set title
  w <- is.na(m$title)
  if("title.zora" %in% names(m)){
    m$title[w] <- m$title.zora[w]
  } else {
    m$title[w] <- m %>% dplyr::select(dplyr::starts_with("title.")[1]) %>% filter(w) %>% pull()
  }
  # set overall year
  if("year.zora" %in% names(m)){
    m <- m %>% dplyr::mutate(year=year.zora)
  } else {
    tmpyear <- m %>% dplyr::select(!!dplyr::starts_with("year")[1]) %>% dplyr::pull()
    m <- m %>% dplyr::mutate(year=tmpyear)
  }
  if (!is.null(df_orcid)){
    w <- is.na(m$year) & !is.na(m$year.orcid)
    m$year[w] <- m$year.orcid[w]
  }
  if (!is.null(df_pubmed)){
    w <- is.na(m$year) & !is.na(m$pubyear)
    m$year[w] <- m$pubyear[w]
  }
  
  # set NA's in 'in_..' columns to FALSE
  m <- m %>% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))
  
  return(tibble::as_tibble(m))
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



