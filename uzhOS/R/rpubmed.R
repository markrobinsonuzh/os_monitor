#' Empty pubmed tibble
#' 
#' @importFrom magrittr %>% 
#' @export
empty_pubmed <- function(){
  tibble::tibble(pubyear = integer(),
                 title = character(),
                 authors = character(),
                 journal = character(),
                 doi = character(),
                 pmid = character(),
                 in_pubmed = logical()) %>% 
    as_tibble_reac(name="pubmed")
}

#' Trim null values or length 0 vectors to return single NA
#'
#' @param x value to fix
#'
#' @return the fixed values
#' @export
#'
#' @examples
#' fix_null(NULL)
#' fix_null(10)
#' fix_null(character(0))
fix_null <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  else return(x)
}


# pri_author <- "robinson m 0000 0002 3048 5518"
# sec_author <- "robinson m d"
# orcid <- "0000-0002-3048-5518"
# pubmed_search <- "(stoeckli esther[au]) or (stoeckli e[au] AND zurich[affiliation]) or Stoeckli ET[au]"
# 
# tib_co <- create_combined_data_wrapper(tbl_authorkeys,tbl_unique_authorkeys,tbl_eprints,tbl_subjects,pri_author,sec_author,orcid,pubmed_search,unpaywall,progress=NULL)

# names(tib_co)
# tib_co %>% select(starts_with("in_"))

#' Create pubmed search query
#'
#' @param author_vec author id
#' @param con db connection function call, e.g. odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' @param authorkeystablename table name
#' @param cutoff_year year, everything below will be excluded
#' @param orcid orcid
#' 
#' @return character
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' pri_author <- "robinson mark d (orcid: 0000-0002-3048-5518)"
#' con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' pubmed_search_string_from_zora_id(pri_author,con)
pubmed_search_string_from_zora_id <- function(author_vec, con, authorkeystablename = "authorkeys", cutoff_year=2001, orcid = NULL){
  scaffold <- "(%s[au] or %s[au] or %s[au]) AND (%i:%i[pdat]) AND (zurich[affiliation])"
  if (is.null(con)){
    auth_name <- tibble::tibble(authorname = "Max Muster")
  } else {
    auth_name <- tbl(con, authorkeystablename) %>% filter(authorkey_fullname %in% author_vec) %>% collect() 
  }
  if (dim(auth_name)[1]==0){
    auth_name <- tibble::tibble(authorname = "Max Muster")
  }
  full_name <- auth_name$authorname
  split_name <- strsplit(full_name," ")
  pubmed_search <- sprintf(scaffold,
                           tolower(full_name), 
                           tolower(paste(split_name[[1]][1],str_sub(split_name[[1]][2],end=1))),
                           paste(split_name[[1]][1],paste0(sapply(split_name[[1]][-1],function(elem) str_sub(elem,end=1)),collapse = "")),
                           cutoff_year,
                           as.integer(str_extract(Sys.Date(),"[:digit:]{4}")))
  if (!is.null(orcid)){
    pubmed_search <- paste(pubmed_search, "OR (orcid", orcid, "[auid])")
  }
  return(pubmed_search)
}


#' Retrieve a table of records from PubMed
#'
#' @param pmid_search search term to search PubMed
#' @param pmid_remove list of PMIDs to remove from list (that comes from search term) 
#' @param pmid_add list of PMIDs to add to list (that comes from search term)
#' @param just_ids set of PMIDs to just retrieve records for (in this case, `pmid_search`, `pmid_remove`, `pmid_add` are ignored)
#' @importFrom rentrez entrez_search entrez_summary
#'
#' @return tibble
#' @export
#'
#' @examples
#' cvm <- retrieve_from_pubmed("von mering c[au]) NOT Von Mering, Christine[Author]")
#' 
#' pmids <- c("11743205", "15761153", "23857251", "26493315", 
#'            "30002819", "30356428", "31178352", "31857895")
#' pms <- retrieve_from_entrez(just_ids = pmids)
retrieve_from_pubmed <- function(pmid_search, pmid_remove=NULL, pmid_add=NULL, just_ids=NULL) {
  if(is.null(just_ids)) {
    x <- entrez_search(db = "pubmed", term = pmid_search, retmax = 500)
    x$ids <- unique(c(base::setdiff(x$ids, pmid_remove), pmid_add))
  } else {
    x <- list(ids=just_ids)
  }
  summ <- tryCatch({
    entrez_summary(db = "pubmed", id = x$ids)
    },error=function(e) NULL)
  if (is.null(summ)){
    return(empty_pubmed())
  }
  summ <- lapply(summ, function(w) {
    tibble::tibble(pubyear = fix_null(strsplit(w$pubdate, " ")[[1]][1]), 
               title = fix_null(w$title), 
               authors = fix_null(paste(w$authors$name, collapse = ", ")),
               journal = fix_null(w$source), 
               doi = fix_null(w$articleids$value[w$articleids$idtype == "doi"]),
               pmid = fix_null(w$articleids$value[w$articleids$idtype == "pubmed"]))
  })
  summ <- do.call(rbind, summ)
  # bunch of hacks to clean stuff up
  summ$title <- sub("\\.$","",summ$title)
  summ$title <- gsub("&lt;/u&gt;","",gsub("&lt;u&gt;","",summ$title, fixed=TRUE))
  summ$title <- gsub("&lt;/i&gt;","",gsub("&lt;i&gt;","",summ$title, fixed=TRUE))
  summ$doi <- tolower(summ$doi)
  summ$doi <- gsub("&lt;","<", summ$doi)
  summ$doi <- gsub("&gt;",">", summ$doi)
  summ$in_pubmed <- TRUE
  summ$pubyear <- as.integer(summ$pubyear)
  return(summ)
}


#' parse returned httr content from pubmed
#' 
#' @param pubmed_doi_convert_get httr::response
#' 
#' @importFrom magrittr %>% 
#' @export
parse_return_html_from_ncbi <- function(pubmed_doi_convert_get){
  # parse
  xmlout <- suppressMessages(httr::content(pubmed_doi_convert_get, encoding = "UTF-8")) %>% 
    xml2::xml_children() %>% xml2::xml_attrs() 
  purrr::map(xmlout[-1],function(.x){
    tmp <- t(.x) %>% as_tibble()
    if (! ("pmid" %in% names(tmp))){
      tmp <- tmp %>% dplyr::mutate(pmid=NA)
    }
    tmp %>% dplyr::select("requested-id",pmid)
  }) %>% purrr::reduce(rbind) %>% 
    dplyr::rename(doi = "requested-id") %>% 
    dplyr::mutate(doi = tolower(doi))
}

#' get PMID from doi
#'
#' @param doi doi
#'
#' @return tibble with columns PMID, doi
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' doi <- c("10.1186/1471-2105-3-35")
#' rec_req_id_converter(c(doi,"error_doi"))
rec_req_id_converter <- function(doi){
  # doi <- unique(doi)
  id <- paste(doi,collapse = ",")
  # retrieve PMID from doi using converter api
  pubmed_doi_convert_get <- httr::GET(url=paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?tool=my_tool&email=my_emailss@example.com&ids=",id,"&idtype=doi"))
  
  # if error but more than one doi, split dataset into two and try again
  if(httr::http_error(pubmed_doi_convert_get) && length(doi)!=1){
    doi_ls <- split(doi, round(seq(0,1,length.out=length(doi))))
    return(lapply(doi_ls, function(doi_sub){
      rec_req_id_converter(doi_sub)
    }) %>% purrr::reduce(rbind))
  # if error and only one doi, return empty tibble
  } else if(httr::http_error(pubmed_doi_convert_get) && length(doi)==1){
    return(tibble::tibble(doi=character(),pmid=character()))
  # if no error return tibble
  }else {
    # read request
    return(parse_return_html_from_ncbi(pubmed_doi_convert_get))
  }
}

#' Empty pubmetric tibble
#' 
#' @export
empty_pubmetric <- function(){
  tibble::tibble(doi=character(),
                 relative_citation_ratio=numeric(), 
                 nih_percentile=numeric(),
                 citation_count=integer(),
                 in_pubmetric=logical())
}


#' retrieve articles from pubmed using dois
#'
#' @param doi doi
#'
#' @return tibble with colums doi, relative_citation_ratio, nih_percentile, citation_count, in_pubmed
#' 
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' doi <- c("10.1186/1471-2105-3-35")
#' retrieve_from_pubmed_with_doi(doi)
retrieve_from_pubmed_with_doi <- function(doi){
  pubmed_doi_convert_cont <- tryCatch(rec_req_id_converter(doi) %>% 
             dplyr::filter(!is.na(pmid)),
           error=function(e) NULL)
  if (!is.null(pubmed_doi_convert_cont) && dim(pubmed_doi_convert_cont)[1] != 0){
    # get pubmed metrics
    pubmed_metrics <- iCiteR::get_metrics(pubmed_doi_convert_cont$pmid)
    
    pubmed_metrics %>% 
      dplyr::select(doi, relative_citation_ratio, nih_percentile, citation_count) %>% 
      dplyr::mutate(in_pubmetric = ifelse(is.na(doi),NA,TRUE)) %>% 
      dplyr::filter(!is.na(doi)) %>% 
      tibble::as_tibble()
  } else {
    return(empty_pubmetric())
  }
}


#' pubmed_citation_plotly
#'
#' @param tbl_merge combined tibble
#'
#' @return shiny.tag object
#' @importFrom magrittr %>% 
#' @export
#'
#' @examples
#' mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
#' df_pubmetric <- retrieve_from_pubmed_with_doi(mr_orcs$doi)
#' tbl_merge <- dplyr::inner_join(mr_orcs,df_pubmetric)
#' df_unpaywall <- oadoi_fetch_local(tbl_merge$doi,con)
#' tbl_merge <- dplyr::inner_join(tbl_merge,df_unpaywall) %>% 
#'   dplyr::mutate(overall_oa=oa_status)
#' pubmed_citation_plotly(tbl_merge)
pubmed_citation_plotly <- function(tbl_merge){
  # preprocessing
  tbl_merge <- tbl_merge %>% 
  dplyr::mutate(overall_oa=droplevels(overall_oa),
                doi =  ifelse(is.na(doi), "", 
                              paste0("<a href='https://www.doi.org/",
                                     doi, 
                                     "' target='_blank'>", doi, "</a>"))) %>% 
    dplyr::arrange(overall_oa,year,title) %>% 
    dplyr::select(title, journal, year, overall_oa, relative_citation_ratio, 
                  nih_percentile, citation_count, doi)
  # plotly linked data
  tbl_merge_k <- highlight_key(tbl_merge)
  
  # helpter to remove names from split function
  split_unamed <- function(x,f){
    tmp <- split(x,f)
    names(tmp) <- NULL
    tmp
  }
  
  # main plot
  plt <- plot_ly(tbl_merge_k,
          name= ~ overall_oa,
          legendgroup= ~overall_oa,
          showlegend=TRUE,
          text = ~title,
          hoverinfo="text+name",
          # size = 10,
          color = ~overall_oa,
          colors = open_cols_fn()[names(open_cols_fn()) %in% unique(tbl_merge %>% pull(overall_oa))],
          source = "B"
          ) %>% 
    add_trace(type = "scatter",
              mode="markers",
              # size = 10,
              x = ~year,
              y = ~relative_citation_ratio
              ) %>% 
    layout(
      # title = "Drop down menus - Styling",
      # xaxis = list(domain = c(0.1, 1)),
      # yaxis = list(title = "y"),
      plot_bgcolor='rgb(240,240,240)', 
      annotations=list(
        list(text = "Y-Axis", x=-0.12, y=0.8, xref='paper', yref='paper', 
             showarrow=FALSE, xanchor="right"),
        list(text = "X-Axis", x=-0.12, y=0.4, xref='paper', yref='paper', 
             showarrow=FALSE, xanchor="right")
      ),
      updatemenus = list(
        # dropdown button plot type
        list(
          x = -0.12,
          y = 1,
          buttons = list(list(method = "update",
                              args = list(list(type= "scatter",
                                               marker.size=list(10))),
                              label = "Scatterplot"),
                         list(method = "update",
                              args = list(list(marker.size = list(1),
                                               type = "box")
                                         ),
                              label = "Boxplot"),
                         list(method = "restyle",
                              args = list("type", "violin"),
                              label = "Violinplot")
          )
        ),
        # dropdown button y axis
        list(
          x = -0.12,
          y = 0.75,
          buttons = lapply(c("relative_citation_ratio","year","nih_percentile","citation_count"), 
                           function(yaxis_name){
            list(method = "update",
                 args = list(list(y=split_unamed(tbl_merge[[yaxis_name]],tbl_merge$overall_oa)),
                             list(yaxis.title = yaxis_name)
                             ),
                 label = yaxis_name)
          })),
        # dropdown button y axis scale
        list(
          x = -0.12,
          y = 0.65,
          buttons = list(list(method = "relayout",
                              args = list(list(yaxis.type = "linear")),
                              label = "linear"),
                         list(method = "relayout",
                              args = list(list(yaxis.type = "log")),
                              label = "log")
          )
        ),
        # dropdown button x axis
        list(
          x = -0.12,
          y = 0.35,
          buttons = lapply(c("year","nih_percentile","relative_citation_ratio","citation_count","overall_oa"), 
                           function(xaxis_name){
            list(method = "update",
                 args = list(list(x=split_unamed(tbl_merge[[xaxis_name]],tbl_merge$overall_oa)),
                             list(xaxis.title = xaxis_name)
                             ),
                 label = xaxis_name)
          })),
        # dropdown button x axis scale
        list(
          x = -0.12,
          y = 0.25,
          buttons = list(list(method = "relayout",
                              args = list(list(xaxis.type = "linear")),
                              label = "linear"),
                         list(method = "relayout",
                              args = list(list(xaxis.type = "log")),
                              label = "log")
          )
        )
        )
      ) %>% 
    highlight("plotly_selected",
              opacityDim = 0.1, 
              selected = attrs_selected(opacity = 1))
  # merge with DT table
  crosstalk::bscols(plt, 
                    DT::datatable(tbl_merge_k, extensions = 'Buttons',
                                  colnames = c("Title","Journal","Year",
                                               "Open access status","RCR",
                                               "NIH perc","citation count","doi"),
                                  options = list(dom = 'Blfrtip',
                                                 pageLength = 10,
                                                 buttons = list('copy', 'csv', 'excel')),
                                  escape = FALSE, 
                                  rownames = FALSE),
                    widths = c(12,12))
}




if (FALSE){
  mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
  for(i in 1:10){
    df_pubmetric <- retrieve_from_pubmed_with_doi(mr_orcs$doi)
  }
  tbl_merge <- dplyr::left_join(mr_orcs,df_pubmetric)
  df_unpaywall <- oadoi_fetch_local(tbl_merge$doi,con)
  tbl_merge <- dplyr::inner_join(tbl_merge,df_unpaywall) %>% 
    dplyr::mutate(overall_oa=oa_status)
  # dplyr::mutate(oa_status=droplevels(oa_status),
  #               doi =  ifelse(is.na(doi), "", 
  #                             paste0("<a href='https://www.doi.org/",doi, "' target='_blank'>", doi, "</a>"))) %>% 
  # dplyr::arrange(oa_status,year,title) %>% 
  # dplyr::select(title, journal, year, oa_status, relative_citation_ratio, nih_percentile, citation_count, doi)
  library(plotly)
  pubmed_citation_plotly(tbl_merge)
  pubmed_citation_plotly(tmpblublutbl_merge %>% filter(in_pubmed))
  names(tbl_merge)
  tmpblublutbl_merge %>% 
    dplyr::filter(!is.na(relative_citation_ratio)) %>% 
    dplyr::select(doi,relative_citation_ratio,nih_percentile,citation_count,title, year,overall_oa, journal ) %>% 
    pubmed_citation_plotly()
}

