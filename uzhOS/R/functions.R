# library(dplyr)
# library(rentrez)
# library(RefManageR)
# library(scholar)


sql_con_cont <- function(con){
  if (!(is(con,"PqConnection"))){
    stop("'con' is no valid connection of type 'RPostgres::`PqConnection-class`.", .call=FALSE)
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
#' con <- dbConnect(RPostgres::Postgres(),
#'     dbname = 'oa',
#'     host = 'db', 
#'     port = 5432,
#'     user = 'shiny',
#'     password = 'flora',
#'     options="-c search_path=oa")
#' oadoi_fetch_local("10.1177/000271625529700159",con)
oadoi_fetch_local <- function(dois, con, unpaywalltablename = "unpaywall"){
  sql_con_cont(con)
  dois <- tolower(dois)
  # if is database connection to mongodb
  oaf <- tbl(con, unpaywalltablename) %>% filter(doi %in% dois) %>% collect() %>% 
    mutate(oa_status = stringr::str_trim(oa_status))
    # if is filename, load first
  if(length(oaf)==0){
    return(tibble::tibble(doi=character(),oa_status=character()))
  } else {
    return(oaf)
  }
}


#' key of oa status
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
    "unknown"="white") 
}

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
#'
#' @return tbl_author
#' 
#' @importFrom magrittr %>%
#' @import DBI
#' @export
#'
#' @examples
# author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
# con <- dbConnect(RPostgres::Postgres(),
#     dbname = 'oa',
#     host = 'db',
#     port = 5432,
#     user = 'shiny',
#     password = 'flora',
#     options="-c search_path=oa")
# create_tbl_author(author_vec,con)
# create_tbl_author(author_vec,con, fac_vec = "07 Faculty of Science")
# create_tbl_author(author_vec,con, dep_vec = "Evolution in Action: From Genomes to Ecosystems" )
create_tbl_author <- function(author_vec, con, authorstablename = "authors", authorkeystablename = "authorkeys", 
                              eprintstablename = "eprints", subjectstablename = "subjects", fac_vec=NULL, dep_vec=NULL){
  sql_con_cont(con)
  tbl_author <- tbl(con, authorstablename) %>% 
    filter(authorkey_fullname %in% author_vec) %>% 
    select(eprintid,authorkey_fullname) %>% 
    collect()
  
  if(dim(tbl_author)[1] != 0){
    tbl_eprints <- tbl(con, eprintstablename) %>% filter(eprintid %in% !!tbl_author$eprintid) %>% collect()
  } else{
    tbl_eprints <- tbl(con, eprintstablename) %>% head(0) %>% collect()
  }
  # tbl_authorkeys <- tbl(con, authorkeystablename) %>% filter(authorkey_fullname %in% author_vec) %>% collect()
  tbl_author <-
    tbl_author %>%
    dplyr::left_join(tbl_eprints,by="eprintid") %>% 
    inner_join(tbl(con, authorkeystablename) %>% filter(authorkey_fullname %in% author_vec) %>% collect(),
               by="authorkey_fullname")
  tbl_author <-
    tbl_author %>% 
    inner_join(tbl(con, subjectstablename) %>% filter(eprintid %in% !!tbl_author$eprintid) %>% collect(),
               by="eprintid") %>%
    # dplyr::left_join(tbl_authorkeys) %>% 
    dplyr::mutate(year = date, doi = tolower(doi)) %>% 
      dplyr::group_by(doi) %>% 
      dplyr::mutate(name=list(name),parent_name=list(parent_name),parent=list(parent),subjects=list(subjects)) %>% 
      unique()
  
  # filter by department if 'dep_vec' is given
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
    tbl_author <- tbl_author %>% filter(!!tmpquo)
  }
  return(tbl_author)
} 


#' create_zora
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
# con <- dbConnect(RPostgres::Postgres(),
#     dbname = 'oa',
#     host = 'db',
#     port = 5432,
#     user = 'shiny',
#     password = 'flora',
#     options="-c search_path=oa")
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

#' create combined data from zora, orcid and unpaywall
#'
#' @param ws tibble from \code{\link{retrieve_from_orcid}}
#' @param zora tibble from \code{\link{create_zora}}
#' @param unpaywall data.frame of reduced unpaywall database
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
create_combined_data <- function(df_orcid,df_pubmed,zora,df_publons,con, unpaywalltablename= "unpaywall"){
  # if df_orcid is given
  if (!(is.null(df_orcid) || dim(df_orcid)[1]==0)){
    m <- dplyr::full_join(df_orcid %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          zora %>% 
                            dplyr::mutate(doi=tolower(doi)) %>%
                            dplyr::as_tibble(), 
                          by="doi", suffix=c(".orcid",".zora"),
                          na_matches="never") 
    m$doi[m$doi=="logical(0)"] <- NA
    # %>%
    #   dplyr::filter(doi != "logical(0)")
    
  # rename some columns for consistency if df_orcid not given
  } else {
    m <- zora %>% 
      dplyr::mutate(doi=tolower(doi)) %>% 
      dplyr::rename(year.zora=year,
             oa_status.zora = oa_status) %>% 
      dplyr::mutate(title.zora=title) %>% 
      dplyr::as_tibble()
  }
  # if df_pubmed is given, join
  if (!(is.null(df_pubmed) || dim(df_pubmed)[1]==0)){
    m <- dplyr::full_join(m, 
                          df_pubmed %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          by="doi", suffix = c("", ".pubmed"),
                          na_matches="never")
    # rename for consistency
    if("oa_status" %in% names(m)){
      m <- m %>% dplyr::rename(oa_status.pubmed=oa_status)
    }
    if("title" %in% names(m)){
      m <- m %>% dplyr::rename(title.pubmed=title)
    }
  }
  # if df_publons is present, join and rename
  if (!(is.null(df_publons) || dim(df_publons)[1]==0)){
    m <- dplyr::full_join(m, 
                          df_publons %>% 
                            dplyr::mutate(doi=tolower(doi)) %>% 
                            dplyr::as_tibble(), 
                          by="doi", suffix = c("", ".publons"),
                          na_matches="never")
    if("oa_status" %in% names(m)){
      m <- m %>% dplyr::rename(oa_status.publons=oa_status)
    }
    if("title" %in% names(m)){
      m <- m %>% dplyr::rename(title.publons=title)
    }
  }
  m <- m %>% dplyr::mutate(title=title.zora)
  
  # get oa status from unpaywall
  oaf <- oadoi_fetch_local(unique(na.omit(m$doi)), con, unpaywalltablename)
  m <- m %>% dplyr::full_join(oaf %>% dplyr::select(doi, oa_status), 
                       by = "doi", suffix=c("", ".unpaywall")) %>% 
    dplyr::rename(oa_status.unpaywall=oa_status)
  
  # set overall oa status
  m$overall_oa <- m$oa_status.unpaywall
  print(m %>% select(starts_with("oa"), overall_oa))
  m$overall_oa <- factor(m$overall_oa, levels = names(open_cols_fn()))
  
  print(dim(m))
  print(m %>% select(starts_with("oa"), overall_oa))
  # print(m$overall_oa %>% table(useNA = "ifany"))
  if (!is.null(df_orcid)){
    m$overall_oa[m$type.orcid=="other"] <- "preprint"
  }
  w <- is.na(m$overall_oa)
  m$overall_oa[w] <- m$oa_status.zora[w]
  w <- m$overall_oa == "closed" & m$oa_status.zora=="blue"
  m$overall_oa[w] <- "blue"
  w <- m$oa_status.zora != "closed" & m$oa_status.unpaywall == "closed"
  w[is.na(w)] <- FALSE
  m$overall_oa[w] <- m$oa_status.zora[w]
  w <- is.na(m$overall_oa)
  m$overall_oa[w] <- "unknown"
  # set title
  w <- is.na(m$title)
  m$title[w] <- m$title.zora[w]
  
  # set overall year
  m <- m %>% dplyr::mutate(year=year.zora)
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
#'
#' @return list of elements "org_unit", "fac" and "author_name",
#' with author_name a list with elements "family" and "given"
#' @export
#' @import DBI
#' @importFrom magrittr %>% 
#'
#' @examples
# author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
# con <- dbConnect(RPostgres::Postgres(),
#     dbname = 'oa',
#     host = 'db',
#     port = 5432,
#     user = 'shiny',
#     password = 'flora',
#     options="-c search_path=oa")
# org_unit_fac(author_vec,con)
org_unit_fac <- function(author_vec, con, authorstablename = "authors", 
                         authorkeystablename = "authorkeys", eprintstablename = "eprints", 
                         subjectstablename = "subjects", fac_vec=NULL, dep_vec=NULL){
  sql_con_cont(con)
  tbl_author <- create_tbl_author(author_vec, con, authorstablename, authorkeystablename, eprintstablename,subjectstablename, fac_vec, dep_vec)
  if (dim(tbl_author)[1] == 0){
    return(list(org_unit=NULL,fac=NULL,author_name=NULL))
  }
  org_unit <- suppressMessages(tbl_author %>% dplyr::select(name) %>% 
                                 dplyr::group_by(name) %>% 
                                 # dplyr::top_n(1) %>% 
                                 dplyr::pull(name) %>% 
                                 unlist() %>% 
                                 tibble::as_tibble() %>% 
                                 dplyr::group_by(value) %>% 
                                 dplyr::tally() %>% 
                                 dplyr::arrange(dplyr::desc(n)) %>% 
                                 dplyr::rename(dept=value,count=n)) 
  fac <- suppressMessages(tbl_author %>% dplyr::select(parent_name) %>% 
                            dplyr::group_by(parent_name) %>% 
                            dplyr::pull(parent_name) %>% 
                            unlist() %>% 
                            tibble::as_tibble() %>% 
                            dplyr::group_by(value) %>% 
                            dplyr::tally() %>% 
                            dplyr::arrange(dplyr::desc(n)) %>% 
                            dplyr::rename(fac=value,count=n))
  type <- suppressMessages(tbl_author %>% dplyr::select(type) %>% 
                             dplyr::group_by(type) %>% 
                             dplyr::pull(type) %>% 
                             unlist() %>% 
                             tibble::as_tibble() %>% 
                             dplyr::group_by(value) %>% 
                             dplyr::tally() %>% 
                             dplyr::arrange(dplyr::desc(n)) %>% 
                             dplyr::rename(type=value,count=n))
  return(list(org_unit=org_unit,fac=fac, type=type ,author_name=author_vec))
}

#' affiliation of aliases from author search
#'
#' @param authorname author id
#' @param con postgresql connection
#' @param authorstablename table name
#' @param authorkeystablename table name
#' @param eprintstablename table name
#' @param subjectstablename table name
#'
#' @return
#' @export
#' @import DBI
#' @importFrom magrittr %>% 
#'
#' @examples
# authorname <- "robinson mark d"
# con <- dbConnect(RPostgres::Postgres(),
#     dbname = 'oa',
#     host = 'db',
#     port = 5432,
#     user = 'shiny',
#     password = 'flora',
#     options="-c search_path=oa")
# pot_alias_and_affil(authorname,con)
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
  return(list(pot_aliases=pot_aliases,pot_affil=pot_affil))
}
# authorname <- author_vec[1]
# authorname <- "schaepman michael e"
# authorname <- "schaepman michael e (orcid: 0000-0002-9627-9565)"
# pot_alias_and_affil(authorname,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints)


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



