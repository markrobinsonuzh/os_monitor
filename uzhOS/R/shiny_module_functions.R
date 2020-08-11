#' show report module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @return d with added data.frames
#' @export
#' @import shiny
#' @importFrom magrittr %>% 
#' 
#'
#' @examples
ShowReportServer <- function(id, d, tbl_authorkeys, tbl_subjects, tbl_eprints, unpaywall) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      reactive({
        # first set old values to NULL
        d$zora <- d$df_pubmed <- d$df_orcid <- d$df_publons <- d$df_scholar <- d$m <-  NULL
        # progress bar
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        # length for progress bar
        not_null <- sapply(c(d$zora,d$pubmed,d$orcid,d$publons,d$scholar),
                           function(e) ifelse((stringr::str_trim(e) == "") | is.null(e),FALSE,TRUE))
        progress_bar_len <- sum(not_null) + 3
        # author info
        tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,d$author_vec)
        if (!is.null(progress)) progress$set( value = progress$getValue() + 1/progress_bar_len, message="create table from Zora")
        # zora data.frame
        d$zora <- create_zora(d$author_vec,tbl_author,tbl_subjects)

        if (!is.null(progress) & !is.null(d$pubmed)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Pubmed")
        # pubmed df if given
        if (stringr::str_trim(d$pubmed) != ""){
          d$df_pubmed <- tryCatch({retrieve_from_pubmed(d$pubmed)},error=function(e) NULL)
        }
        # fetch oa status from unpaywall
        if (!is.null(d$df_pubmed)){
          tmpoadoi <- oadoi_fetch_local(na.omit(d$df_pubmed$doi),unpaywall)
          d$df_pubmed <- dplyr::left_join(d$df_pubmed,tmpoadoi,by="doi")
        }
        print("df_pubmed")
        print(dim(d$df_pubmed))
        
        if (!is.null(progress) & !is.null(d$orcid)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Orcid")
        # orcid df
        if (stringr::str_trim(d$orcid) != ""){
          d$df_orcid <- tryCatch({retrieve_from_orcid(d$orcid) %>%
                  dplyr::mutate(doi = tolower(doi))},
                  error=function(e) NULL)
        }
        # fetch oa status from unpaywall
        if (!is.null(d$df_orcid)){
          tmpoadoi <- oadoi_fetch_local(na.omit(d$df_orcid$doi),unpaywall)
          d$df_orcid <- dplyr::left_join(d$df_orcid,tmpoadoi,by="doi")
        }
        print("df_orcid")
        print(dim(d$df_orcid))
        
        # publons df
        if (stringr::str_trim(d$publons) != ""){
          if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Publons")
          d$df_publons <- tryCatch({retrieve_from_publons(d$publons)},error=function(e) {print(e);return(NULL)})
          print("df_publons")
          print(dim(d$df_publons))
        }
        
        # combine table
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="Combine table")
        tbl_merge <- create_combined_data(d$df_orcid,d$df_pubmed,d$zora,d$df_publons,unpaywall)

        # google scholar
        if (stringr::str_trim(d$scholar) != ""){
          if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Scholar")
          d$df_scholar <- retrieve_from_scholar(d$scholar)
          d$df_scholar <- df_scholar_matching(tbl_merge,d$df_scholar)
          # add to combined table
          tbl_merge <- dplyr::full_join(tbl_merge,d$df_scholar,by="doi",suffix=c("",".scholar"))
        }
        # NA's in 'in_..' to FALSE
        tbl_merge <- tbl_merge %>% dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))

        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len)
        d$m <- tbl_merge
        
        return(d=d)
      })
    }
  )
}


















