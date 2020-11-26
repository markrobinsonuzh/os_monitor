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
ShowReportServer <- function(id, d, con, authorstablename = "authors", authorkeystablename = "authorkeys", eprintstablename = "eprints", subjectstablename = "subjects") {
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
                           function(e) ifelse(is.null(e) || (stringr::str_trim(e) == "") ,FALSE,TRUE))
        progress_bar_len <- sum(not_null) + 4

        
        if (!is.null(progress)) progress$set( value = progress$getValue() + 1/progress_bar_len, message="create table from Zora")
        # zora data.frame
        d$zora<- future(seed=NULL,{
          con <- dbConnect(odbc::odbc(), "PostgreSQL")
          tbl
          create_zora(isolate(d$author_vec), con)
          })
        print(dim(d$zora))
        
        if (!is.null(progress) && !(is.null(d$pubmed) || stringr::str_trim(d$pubmed) == "")) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Pubmed")
        # pubmed df if given
        if (!is.null(d$pubmed) && stringr::str_trim(d$pubmed) != ""){
          d$df_pubmed <- future(seed=NULL,{
            tbl
            retrieve_from_pubmed
            tryCatch({retrieve_from_pubmed(d$pubmed)},
                     error=function(e) tibble::tibble(pubyear=charachter(), title=character(),
                     authors= character(), journal=character(), doi=character(), 
                     pmid=character(), in_pubmed=logical())
                     )
            })
        }
        
        if (!is.null(progress) && !(is.null(d$orcid) || (stringr::str_trim(d$orcid) == ""))) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Orcid")
        # orcid df
        if (!is.null(d$orcid) && stringr::str_trim(d$orcid) != ""){
          d$df_orcid <-  future(seed=NULL,{
            tbl
            retrieve_from_orcid
            retrieve_from_orcid(isolate(d$orcid)) %>%
              dplyr::mutate(doi = tolower(doi))
          })
        }
        
        # publons df
        if (!is.null(d$publons) && stringr::str_trim(d$publons) != ""){
          if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="create table from Publons")
          d$df_publons <- future(seed = NULL,{
            tryCatch({retrieve_from_publons(d$publons)},
                     error=function(e) {return(data.frame(doi=character(),
                                                          title=character(),
                                                          date=character(),
                                                          year=character(),
                                                          in_publons=logical()))})
          })
          print("df_publons")
          print(dim(d$df_publons))
        }
        # google scholar
        if (!is.null(d$scholar) && stringr::str_trim(d$scholar) != ""){
          d$df_scholar <- future(seed=NULL,{retrieve_from_scholar(isolate(d$scholar))})
          }
        
        # combine table
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="Combine table")
        resolve(list(d$df_orcid,d$df_pubmed,d$zora,d$df_publons,d$df_scholar))
        for(df in c("df_orcid","df_pubmed","zora","df_publons","df_scholar")){
          d[[df]] <- tryCatch({value(d[[df]])},error=function(e) NULL)
        }
        print("resolved")
        tbl_merge <- create_combined_data(d$df_orcid, d$df_pubmed, d$zora ,d$df_publons, con)
        print("merged")

        # google scholar
        if (!is.null(d$scholar) && stringr::str_trim(d$scholar) != ""){
          if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="Retrieve from Google scholar")
          if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len, message="Match entries from Google scholar, this could take a few minutes.")
          print("before scholar matching")
          d$df_scholar <- future(seed = NULL,{
            str_length
            df_scholar_matching(tbl_merge,isolate(d$df_scholar))
            })
          resolve(d$df_scholar)
          d$df_scholar <- value(d$df_scholar)
          print("after matching")
          # add to combined table
          tbl_merge <- dplyr::full_join(tbl_merge,d$df_scholar,by="doi",suffix=c("",".scholar"))
          print("after merging")
        }
        # NA's in 'in_..' to FALSE
        tbl_merge <- tbl_merge %>% dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))

        
        
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/progress_bar_len)
        d$m <- tbl_merge
        
        d
      })
    }
  )
}


















