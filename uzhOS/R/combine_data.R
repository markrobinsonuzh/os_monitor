#' create combined data from zora, orcid, pubmed, publons and unpaywall
#'
#' @param df_orcid tibble from \code{\link{retrieve_from_orcid}}
#' @param df_pubmed tibble from \code{\link{retrieve_from_pubmed}}
#' @param df_zora tibble from \code{\link{create_zora}}
#' @param df_publons tibble from \code{\link{retrieve_from_publons}}
#' @param con postgresql connection
#' @param unpaywalltablename table name
#'
#' @return tibble
#'
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' df_orcid <- empty_orcid()
#' df_pubmed <- empty_pubmed()
#' df_zora <- empty_zora()
#' df_publons <- empty_publons()
#' con <-  odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' tbl_merge <- create_combined_data(df_orcid, df_pubmed, df_zora, df_publons, con)
create_combined_data <- function(df_orcid, df_pubmed, df_zora, df_publons, con, unpaywalltablename = "unpaywall"){
  df_names <- c("df_orcid", "df_pubmed", "df_zora", "df_publons")
  df_names_short <- stringr::str_replace(df_names,"df_","")
  df_is_empty_fn <- function(df){
    (is.null(df) || dim(df)[1]==0)
  }
  df_is_empty <- purrr::map_lgl(df_names,~df_is_empty_fn(eval(rlang::sym(.x))))

  m <- tibble::tibble(doi=character(), title=character())
  for (i in seq_along(df_is_empty)) {

      tmpdf <- eval(rlang::sym(df_names[i]))
      if("doi" %in% colnames(tmpdf)){
        tmpdf <- tmpdf %>% dplyr::mutate(doi=tolower(doi))
      }
      var_names <- c("oa_status","title","type","year","journal","date")
      for (var in var_names) {
        if(var %in% names(tmpdf)){
          tmpdf <- tmpdf %>% dplyr::rename(!!paste0(var,".",df_names_short[i]):=!!var)
        }
      }
      if(df_is_empty[i]){
        tmpdf <- tmpdf %>% dplyr::select(-!!paste0("in_",df_names_short[i]),
                                         -!!paste0("title.",df_names_short[i]))
      }
      m <- dplyr::full_join(m,
                            tmpdf %>%
                              dplyr::mutate(doi=tolower(doi)) %>%
                              dplyr::as_tibble(),
                            by="doi")
  }

  #### OA Status ---------------------------------------------------------------
  # get oa status from unpaywall
  oaf <- oadoi_fetch_local(unique(na.omit(m$doi)), con, unpaywalltablename) %>%
    dplyr::mutate(oa_status.unpaywall = oa_status)
  m <- m %>% dplyr::full_join(oaf,
                              by = "doi")

  # overall oa status from unpaywall
  m$overall_oa <- m$oa_status.unpaywall
  m$overall_oa <- factor(m$overall_oa, levels = names(open_cols_fn()))

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
  m <- m %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = na.omit(unique(dplyr::c_across(dplyr::starts_with("title"))))[1]) %>%
    dplyr::ungroup()

  # set overall year
  m <- m %>%
    dplyr::rowwise() %>%
    dplyr::mutate(year = na.omit(unique(dplyr::c_across(dplyr::starts_with("year"))))[1]) %>%
    dplyr::ungroup()

  # set NA's in 'in_..' columns to FALSE
  m <- m %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))

  return(tibble::as_tibble(m))
}







