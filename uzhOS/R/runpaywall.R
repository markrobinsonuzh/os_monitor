#' Check db connection 
#'
#' @param con DBI connection object
#'
#' @export
sql_con_cont <- function(con){
  if (!(is(con,"PqConnection") | is(con,"PostgreSQL") | is(con, "DBIMockConnection"))){
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
#' con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' oadoi_fetch_local("10.1177/000271625529700159",con)
oadoi_fetch_local <- function(dois, con, unpaywalltablename = "unpaywall"){
  sql_con_cont(con)
  dois <- tolower(dois)
  oaf <- tbl(con, unpaywalltablename) %>% 
    dplyr::filter(doi %in% dois) %>% 
    dplyr::collect() %>% 
    dplyr::mutate(oa_status = factor(stringr::str_trim(oa_status),levels = names(open_cols_fn())))
  if ("version" %in% colnames(oaf)) {
    oaf <- oaf %>%
      dplyr::mutate(oa_status = dplyr::if_else(oa_status == "green" &&
                                                 !is.na(version) &&
                                                 version == "submittedVersion",
                                               "preprint",
                                               oa_status)) %>%
      dplyr::select(-version)
  }
  if(length(oaf)==0){
    return(tibble::tibble(doi=character(),oa_status=factor(levels = names(open_cols_fn()))))
  } else {
    return(oaf)
  }
}
