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
 # if (!is.null(df_orcid)){
 #   m$overall_oa[m$type.orcid=="other"] <- "preprint"
 # }
  
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
  m <- m %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(title = na.omit(unique(c_across(starts_with("title"))))[1]) %>% 
    dplyr::ungroup()
  
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

