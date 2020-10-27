#' Title
#'
#' @param id 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
showReportUI <- function(id, label = "Show Report") {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_report"), label = label),
  )
}

#' @export
#' @import shiny
#' @import future
#' 
scholarModalServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        if(d$is_valid_scholar){
          showModal(modalDialog("Retrieving and matching entries from Google scholar usually takes a few minutes. 
                                The plots and tables will update automatically on successfull completion.",
                                title = "Google scholar info", size="s",easyClose = TRUE))
        }
      })
    }
  )
}


#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
createZoraServer <- function(id, d, df_zora) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        future(seed=NULL,{
          con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
          create_zora(author_vec, con)
        }, globals = list(empty_zora=empty_zora,
                          tbl=tbl,
                          create_zora=create_zora,
                          author_vec=isolate(d$author_vec))) %...>% 
          df_zora() 
      })
    }
  )
}

#' create orcid module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
createOrcidServer <- function(id, d, df_orcid) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        if (!is.null(d$orcid) && d$orcid != ""){
          future(seed=NULL,{
            con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
            retrieve_from_orcid(orcid) %>%
              dplyr::mutate(doi = tolower(doi))    
          }, globals = list(empty_orcid=empty_orcid,
                            tbl=tbl,
                            retrieve_from_orcid=retrieve_from_orcid,
                            orcid=isolate(d$orcid))) %...>% 
            df_orcid()
        } else {
          empty_orcid()
        }
      })
    }
  )
}

#' create scholar module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
createScholarServer <- function(id, d, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        if (!is.null(d$scholar) && d$scholar != ""){
          print("do scholar")
          future(seed=NULL,{
            retrieve_from_scholar(scholar)
          }, globals = list(empty_scholar=empty_scholar,
                            tbl=tbl,
                            retrieve_from_scholar=retrieve_from_scholar,
                            scholar=isolate(d$scholar))) %...>%
            df_scholar()
        }else {
          empty_scholar()
        }
      })
    }
  )
}

#' create pubmed module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
createPubmedServer <- function(id, d, df_pubmed) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        future(seed=NULL,{
          tryCatch({retrieve_from_pubmed(pubmed)},
                   error=function(e) empty_pubmed())
        }, globals = list(empty_pubmed=empty_pubmed,
                          tbl=tbl,
                          retrieve_from_pubmed=retrieve_from_pubmed,
                          pubmed=isolate(d$pubmed))) %...>%
          df_pubmed()
      })
    }
  )
}


#' create publons module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
createPublonsServer <- function(id, d, df_publons) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        future(seed=NULL,{
          tryCatch({retrieve_from_publons(publons)},
                   error=function(e) {return(empty_publons())})
          }, globals = list(empty_publons=empty_publons,
                            tbl=tbl,
                            retrieve_from_publons=retrieve_from_publons,
                            publons=isolate(d$publons))) %...>%
          df_publons()
      })
    }
  )
}





#' Title
#'
#' @param id 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
ProgressbarUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::progressBar(id = ns("pb_data_retrieval"), value = 0, total = 100, title = "", display_pct = TRUE)
    )
}


#' create publons module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
ProgressbarCreateServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # ProgressbarCreateServer("show_report")
      observeEvent({input$show_report},{
        updateProgressBar(
          session = session,
          id = "pb_data_retrieval",
          value = 0, total = 100
          # title = paste("Process", trunc(i/10))
        )
        shinyjs::show("pd_data_retrieval")
      })  
    }
  )
}

#' create publons module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
DeactivateShowReportServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # ProgressbarCreateServer("show_report")
      observeEvent({input$show_report},{
          shinyjs::disable("show_report")
      })  
    }
  )
}

#' create publons module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
ActivateShowReportServer <- function(id, d, df_zora, df_orcid, df_pubmed, df_publons, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # ProgressbarCreateServer("show_report")
      observeEvent({df_zora();df_orcid();df_pubmed();df_publons();df_scholar()},{
        nr_datasets <- purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons,df_scholar), ~ dim(req(.x()))[1] != 0) %>% sum()
        nr_datasets_total <- c(d$is_valid_zora, d$is_valid_orcid, d$is_valid_pubmed, d$is_valid_publons, d$is_valid_scholar) %>% sum()
        if(nr_datasets==nr_datasets_total){
          shinyjs::enable("show_report")
        }
      })  
    }
  )
}


#' create publons module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
ProgressbarUpdateServer <- function(id, d, df_zora, df_orcid, df_pubmed, df_publons, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # ProgressbarCreateServer("show_report")
      observeEvent({df_zora();df_orcid();df_pubmed();df_publons();df_scholar()},{
        nr_datasets <- purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons,df_scholar), ~ dim(req(.x()))[1] != 0) %>% sum()
        nr_datasets_total <- c(d$is_valid_zora, d$is_valid_orcid, d$is_valid_pubmed, d$is_valid_publons, d$is_valid_scholar) %>% sum()
        
        updateProgressBar(
          session = session,
          id = "pb_data_retrieval",
          value = nr_datasets/nr_datasets_total*100, total = 100
          # title = paste("Process", trunc(i/10))
        )
        if(nr_datasets==nr_datasets_total){
          shinyjs::hide("pd_data_retrieval")
        }
      })  
    }
  )
}


