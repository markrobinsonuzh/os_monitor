library(dplyr)
library(rentrez)
library(RefManageR)
library(scholar)


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
  if (is.null(x) || length(x) == 0) NA
  else x
}



#' fetch doi entries from local unpaywall
#'
#' @param dois dois to get open access status from
#' @param file filename or data.frame of unpaywall data 
#'
#' @return open access status for each doi
#' @export
#'
#' @examples 
#' oadoi_fetch_local("10.1177/000271625529700159")
oadoi_fetch_local <- function(dois,file=readRDS(here::here("output","dois_unpaywall.rds"))){
  # if is database connection to mongodb
  if (is(file,"mongo")){
    lapply(dois,function(doi){
      single_query <- file$find(paste0('{"doi":"',doi,'"}'))
      if (length(single_query)<2){
        tibble::tibble(doi=NULL,oa_status=NULL)
      } else {
        tibble::tibble(doi=single_query[[1]],oa_status=single_query[[2]])
      }
    }) %>% purrr::reduce(rbind)
    
    # if is filename, load first
  } else if (!is(file,"data.frame")){
    file <- readRDS(here::here("output","dois_unpaywall.rds"))
    dplyr::filter(file,doi %in% dois)
    
    # if is data.frame
  } else {
    dplyr::filter(file,doi %in% dois)
  }
}

# authors_from_unpaywall_doi <- function(doi,file){
#     single_query <- file$find(paste0('{"doi":"',doi,'"}'))
#     single_query[[3]]
# }


#' key of oa status
#'
#' @return list of keypairs
#' @export
#'
#' @examples
open_cols_fn <- function(){
  c("closed" = "gray48", "hybrid" = "darkorange1",
    "green" = "chartreuse4", "gold" = "gold",
    "preprint" = "red", "bronze" = "darkgoldenrod4",
    "blue" = "blue",
    "NA"="white") 
}

oa_status_order <- function(){
  c("closed", "preprint", "bronze", "blue","hybrid", "green","gold")
}


#' create_tbl_author
#'
#' @param tbl_authorkeys tibble created in '01_zora_preprocessing.Rmd'
#' @param tbl_eprints tibble created in '01_zora_preprocessing.Rmd'
#' @param pri_author author id (with attached orcid)
#' @param sec_author author id 
#'
#' @return tbl_author
#' 
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
create_tbl_author <- function(tbl_authorkeys,tbl_eprints,pri_author,sec_author=""){
  if (is(tbl_authorkeys,"mongo") & is(tbl_eprints,"mongo")){
    tbl_author <- tbl_authorkeys$find(paste0('{"authorkey_fullname": {"$in": ["',pri_author,'","',sec_author,'"] }   }'))
    tbl_eprints <- tbl_eprints$find(paste0('{"eprintid": { "$in": [',paste0(tbl_author$eprintid,collapse = ','),'] } }'))
    if(!("doi" %in% names(tbl_eprints))){
      tbl_eprints <- tbl_eprints %>% mutate(doi=NA)
    }
    return(tbl_author%>%
      dplyr::left_join(tbl_eprints,by="eprintid") %>%
      dplyr::mutate(year = date, doi = tolower(doi)))
  } else {
    tbl_authorkeys %>% 
      dplyr::filter(authorkey %in% c(pri_author, sec_author)) %>%
      dplyr::left_join(tbl_eprints,by="eprintid") %>%
      dplyr::mutate(year = date, doi = tolower(doi))
  }
} 

#' create_zora
#'
#' @param pri_author author id (with attached orcid)
#' @param sec_author author id 
#' @param tbl_author tibble, created from \code{\link{create_tbl_author}}
#' @param tbl_subjects tibble created in '01_zora_preprocessing.Rmd'
#'
#' @return
#' 
#' @export
#' @importFrom magrittr %>%
#' 
#' @examples
#' pri_author <- "robinson m 0000 0002 3048 5518"
#' sec_author <- "robinson m d"
#' create_zora(pri_author,sec_author,tbl_author,tbl_subjects)
create_zora <- function(pri_author,sec_author,tbl_author,tbl_subjects){
  open_cols <-  open_cols_fn()
  if (is(tbl_subjects,"mongo")){
    tbl_subjects <- tbl_subjects$find(paste0('{"eprintid": { "$in": [',paste0(tbl_author$eprintid,collapse = ','),'] } }'))
  }
  dept_fac <- tbl_author %>% dplyr::left_join(tbl_subjects %>%
                                                dplyr::select(eprintid, name, parent_name),by="eprintid")
  # org_unit <- suppressMessages(dept_fac %>% dplyr::select(name) %>% dplyr::group_by(name) %>%
  #   tally %>% dplyr::top_n(1) %>% dplyr::pull(name))
  
  zora <- dept_fac %>%
    dplyr::mutate(dept = name, faculty = parent_name, in_zora=TRUE) %>%
    # dplyr::filter(authorkey == pri_author | (authorkey %in% sec_author & dept == org_unit)) %>%
    dplyr::filter(authorkey_fullname %in% c(pri_author,sec_author)) %>%
    dplyr::select(-dept, -faculty, -name, -parent_name) %>% unique()
  
  if (dim(zora)[1]!=0){
    # add blue OA
    zora$oa_status[zora$published_doc & zora$oa_status=="closed"] <- "blue" 
    zora$oa_status <- factor(zora$oa_status, levels = names(open_cols))
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
create_combined_data <- function(df_orcid,df_pubmed,zora,df_publons,unpaywall){
  # names(zora)[names(zora)!="doi"] <- paste0(names(zora)[names(zora)!="doi"],".zora")
  # if (!is.null(df_orcid)){
  #   names(df_orcid)[names(df_orcid)!="doi"] <- paste0(names(df_orcid)[names(df_orcid)!="doi"],".orcid")
  # }
  # if (!is.null(df_pubmed)){
  #   names(df_pubmed)[names(df_pubmed)!="doi"] <- paste0(names(df_pubmed)[names(df_pubmed)!="doi"],".pubmed")
  # }
  open_cols <-  open_cols_fn()
  if (!is.null(df_orcid)){
    m <- dplyr::full_join(df_orcid %>% dplyr::mutate(doi=tolower(doi)), 
                          zora %>% dplyr::mutate(doi=tolower(doi)), 
                          by="doi", suffix=c(".orcid",".zora"),na_matches="never") %>%
      dplyr::filter(doi != "logical(0)") #%>% 
  } else {
    m <- zora %>% mutate(doi=tolower(doi),
                         year.zora=year,
                         oa_status.zora = oa_status,
                         title.zora = title)
  }
  if (!is.null(df_pubmed)){
    m <- full_join(m, df_pubmed %>% dplyr::mutate(doi=tolower(doi)), by="doi", suffix = c("", ".pubmed"),na_matches="never")
  }
  if (!is.null(df_publons)){
    m <- full_join(m, df_publons %>% dplyr::mutate(doi=tolower(doi)), by="doi", suffix = c("", ".publons"),na_matches="never")
  }
  oaf <- oadoi_fetch_local(unique(na.omit(m$doi)),unpaywall)
  m <- m %>% dplyr::left_join(oaf %>% select(doi, oa_status), 
                       by = "doi", suffix=c("", ".unpaywall")) %>% 
    mutate(oa_status.unpaywall=oa_status)
  
  
  m$overall_oa <- m$oa_status.unpaywall
  if (!is.null(df_orcid)){
    m$overall_oa[m$type.orcid=="other"] <- "preprint"
  }
  
  w <- is.na(m$overall_oa)
  m$overall_oa[w] <- m$oa_status.zora[w]
  m <- m %>% mutate(year=year.zora)
  if (!is.null(df_orcid)){
    w <- is.na(m$year) & !is.na(m$year.orcid)
    m$year[w] <- m$year.orcid[w]
  }
  if (!is.null(df_pubmed)){
    w <- is.na(m$year) & !is.na(m$pubyear)
    m$year[w] <- m$pubyear[w]
  }

  w <- is.na(m$title)
  m$title[w] <- m$title.zora[w]
  w <- m$overall_oa == "closed" & m$oa_status.zora=="blue"
  m$overall_oa[w] <- "blue"
  m$overall_oa <- factor(m$overall_oa, levels = names(open_cols))
  
  m <- m %>% mutate(across(starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))
  m
}



#' #' create_combined_data_wrapper
#' #'
#' #' @param tbl_authorkeys tibble created in '01_zora_preprocessing.Rmd'
#' #' @param tbl_eprints tibble created in '01_zora_preprocessing.Rmd'
#' #' @param tbl_subjects tibble created in '01_zora_preprocessing.Rmd'
#' #' @param pri_author author id (with attached orcid)
#' #' @param sec_author author id 
#' #' @param orcid orcid
#' #' @param unpaywall data.frame of reduced unpaywall database
#' #'
#' #' @return
#' #' 
#' #' @export
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' create_combined_data_wrapper <- function(tbl_authorkeys,tbl_unique_authorkeys,tbl_eprints,tbl_subjects,pri_author,sec_author,orcid,pubmed,unpaywall,progress=NULL){
#'   tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,pri_author,sec_author)
#'   if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
#'   dept_fac <- tbl_author %>% dplyr::left_join(tbl_subjects %>% 
#'                                                 dplyr::select(eprintid, name, parent_name),by="eprintid")
#'   if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
#'   org_unit <- suppressMessages(dept_fac %>% dplyr::select(name) %>% dplyr::group_by(name) %>% 
#'     tally %>% dplyr::top_n(1) %>% dplyr::pull(name))
#'   if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
#'   df_orcid <- tryCatch({retrieve_from_orcid(orcid) %>%
#'       dplyr::mutate(doi = tolower(doi))},
#'       error=function(e) NULL)
#'   if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
#'   # ws <- retrieve_from_orcid(orcid) %>%
#'   #     dplyr::mutate(doi = tolower(doi))
#'   zora <- create_zora(pri_author,sec_author,tbl_author,tbl_subjects)
#'   if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
#'   df_pubmed <- retrieve_from_pubmed(pubmed)
#'   # df_pubmed <- retrieve_from_pubmed_from_zora_id(pri_author,tbl_unique_authorkeys)
#'   create_combined_data(df_orcid,df_pubmed,zora,NULL,unpaywall)
#' }




#' get family and given name of authorkey
#'
#' @param pri_author key
#' @param tbl_authorkeys data frame or mongo connection 
#'
#' @return list of two elements: family and given
#' @export
#'
#' @examples
#' pri_author <- "robinson m 0000 0002 3048 5518"
#' full_author_name(pri_author,tbl_authorkeys)
full_author_name <- function(pri_author,tbl_authorkeys){
  if (is(tbl_authorkeys,"mongo")){
    tbl_authorkeys_filt <- tbl_authorkeys$find(paste0('{"authorkey": "',pri_author,'"}'))
  } else {
    tbl_authorkeys_filt <- tbl_authorkeys %>% 
      dplyr::filter(authorkey == pri_author)
  }
  if (length(names(tbl_authorkeys_filt))!=0){
    return(tbl_authorkeys_filt %>% 
             dplyr::select(author_name_family, author_name_given) %>% unique() %>% 
             dplyr::rename(family=author_name_family,given=author_name_given) %>% 
             as.list())
  } else {
    return(list())
  }

}



#' department, faculty and full name of author
#'
#' @param pri_author key
#' @param sec_author key
#' @param tbl_subjects 
#' @param tbl_authorkeys 
#' @param tbl_eprints 
#'
#' @return list of elements "org_unit", "fac" and "author_name",
#' with author_name a list with elements "family" and "given"
#' @export
#'
#' @examples
org_unit_fac <- function(authorname,tbl_subjects,tbl_authorkeys,tbl_eprints){
  tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,authorname)
  if (is(tbl_subjects,"mongo") & is(tbl_eprints,"mongo")){
    dept_fac <- tbl_author %>% left_join(tbl_subjects$find(paste0('{"eprintid": { "$in": [',paste0(tbl_author$eprintid,collapse = ','),'] } }')) %>%
                                           select(eprintid, name, parent_name),by="eprintid")
  } else {
    dept_fac <- tbl_author %>% left_join(tbl_subjects %>%
                                           select(eprintid, name, parent_name),by="eprintid")
  }
  # author_name_pri <- full_author_name(pri_author,tbl_authorkeys)
  # author_name_sec <- full_author_name(sec_author,tbl_authorkeys)
  # author_name_pri$family <- c(author_name_pri$family,author_name_sec$family)
  # author_name_pri$given <- c(author_name_pri$given,author_name_sec$given)
  
  org_unit <- suppressMessages(dept_fac %>% select(name) %>% group_by(name) %>%
    tally %>% top_n(1) %>% pull(name))
  fac <- suppressMessages(dept_fac %>% select(parent_name) %>% group_by(parent_name) %>%
    tally %>% top_n(1) %>% pull(parent_name))
  return(list(org_unit=org_unit,fac=fac,author_name=authorname))
}


#' affiliation of aliases from author search
#'
#' @param author_string author name in the format: "familyname givenname[1] givenname2[1]"
#' @param tbl_unique_authorkeys 
#' @param tbl_subjects 
#' @param tbl_authorkeys 
#' @param tbl_eprints 
#'
#' @return
#' @export
#'
#' @examples
pot_alias_and_affil <- function(authorname,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints){
  # if data in mongodb 
  if (is(tbl_unique_authorkeys_fullname,"mongo")){
    ind_auth <- tbl_unique_authorkeys_fullname$find(paste0('{"authorname":"',authorname,'"}'))[["id"]]
    # ind_pot <- lapply(possible_alias_author(author_string), function(auth){
    #   tbl_unique_authorkeys_fullname$find(paste0('{"authorkey_processed":"',auth,'"}'))[["id"]]
    # })
    # tpmind <- ind_pot[lapply(ind_pot,length)>0]
    # if (length(tpmind)==0){
      pot_aliases <- tbl_unique_authorkeys_fullname$find(paste0('{"id": { "$in": [',paste0(ind_auth,collapse = ','),'] } }'))[["authorkey_fullname"]]
    # } else {
    #   pot_aliases <- tbl_unique_authorkeys_fullname$find(paste0('{"id": { "$in": [',paste0(c(ind_auth,tpmind[[1]]),collapse = ','),'] } }'))[["authorkey_fullname"]]
    # }
    # if data as data.frame
  } else {
    ind_auth <- which(tbl_unique_authorkeys$authorkey_processed==authorname)
    ind_pot <- lapply(possible_alias_author(authorname), function(auth){
      which(tbl_unique_authorkeys$authorkey_processed==auth)
    })
    tpmind <- ind_pot[lapply(ind_pot,length)>0]
    if (length(tpmind)==0){
      pot_aliases <- tbl_unique_authorkeys$authorkey[ind_auth]
    } else {
      pot_aliases <- c(tbl_unique_authorkeys$authorkey[ind_auth],tbl_unique_authorkeys$authorkey[tpmind[[1]]])
    }
  }
  pot_affil <- lapply(pot_aliases, function(pot_alias){
    org_unit_fac(pot_alias,tbl_subjects,tbl_authorkeys,tbl_eprints)
  })
  names(pot_affil) <- pot_aliases
  return(list(pot_aliases=pot_aliases,pot_affil=pot_affil))
}

