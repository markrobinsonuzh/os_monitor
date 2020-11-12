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
scholarModalServer <- function(id, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        if(valid_input(df_scholar())){
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
createZoraServer <- function(id, df_zora) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_zora, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_zora, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_zora, "successfully_retrieved", FALSE)
        if (valid_input(df_zora())){
        future(seed=NULL,{
          con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
          create_zora(author_vec, con)
        }, globals = list(empty_zora=empty_zora,
                          tbl=tbl,
                          create_zora=create_zora,
                          author_vec=isolate(input_value(df_zora())))) %...>% 
            to_tibble_reac_template(df_zora()) %...>% 
            `retrieval_done<-`(TRUE) %...>% 
            df_zora()
        }
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
createOrcidServer <- function(id, df_orcid) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_orcid, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_orcid, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_orcid, "successfully_retrieved", FALSE)
        if (valid_input(df_orcid())){
          assign_to_reactiveVal(df_orcid, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
            retrieve_from_orcid(orcid) %>%
              dplyr::mutate(doi = tolower(doi))    
          }, globals = list(empty_orcid=empty_orcid,
                            tbl=tbl,
                            retrieve_from_orcid=retrieve_from_orcid,
                            orcid=isolate(input_value(df_orcid())))) %...>% 
            to_tibble_reac_template(df_orcid()) %...>% 
            `retrieval_done<-`(TRUE) %...>% 
            df_orcid()
        }
      })
    }
  )
}
ResultCheckServer <- function(id, df_whatever) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(retrieval_done(df_whatever()),{
        req(input$show_report)
        if (retrieval_done(df_whatever())){
          check_and_set_successfull_retrieval(df_whatever)
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
createScholarServer <- function(id, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_scholar, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_scholar, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_scholar, "successfully_retrieved", FALSE)
        if (valid_input(df_scholar())){
          assign_to_reactiveVal(df_scholar, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            retrieve_from_scholar(scholar)
          }, globals = list(empty_scholar=empty_scholar,
                            tbl=tbl,
                            retrieve_from_scholar=retrieve_from_scholar,
                            scholar=isolate(input_value(df_scholar())))) %...>% 
            to_tibble_reac_template(df_scholar()) %...>% 
            `retrieval_done<-`(TRUE) %...>% 
            df_scholar()
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
createPubmedServer <- function(id, df_pubmed) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_pubmed, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_pubmed, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_pubmed, "successfully_retrieved", FALSE)
        if (valid_input(df_pubmed())){
          assign_to_reactiveVal(df_pubmed, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            tryCatch({retrieve_from_pubmed(pubmed)},
                     error=function(e) empty_pubmed())
          }, globals = list(empty_pubmed=empty_pubmed,
                            tbl=tbl,
                            retrieve_from_pubmed=retrieve_from_pubmed,
                            pubmed=isolate(input_value(df_pubmed())))) %...>% 
            to_tibble_reac_template(df_pubmed()) %...>% 
            `retrieval_done<-`(TRUE) %...>% 
            df_pubmed()
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
createPublonsServer <- function(id, df_publons) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$show_report,{
        req(input$show_report)
        assign_to_reactiveVal(df_publons, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_publons, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_publons, "successfully_retrieved", FALSE)
        if (valid_input(df_publons())){
          future(seed=NULL,{
            tryCatch({retrieve_from_publons(publons)},
                     error=function(e) {return(empty_publons())})
            }, globals = list(empty_publons=empty_publons,
                              tbl=tbl,
                              retrieve_from_publons=retrieve_from_publons,
                              publons=isolate(input_value(df_publons())))) %...>% 
            to_tibble_reac_template(df_publons()) %...>% 
            `retrieval_done<-`(TRUE) %...>%
            df_publons()
        }
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
DeactivateShowReportServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # ProgressbarCreateServer("show_report")
      observeEvent({input$show_report},{
          shinyjs::disable("show_report")
        d$m <- d$m_filt <- d$m_filt_sub <-  NULL
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
ActivateShowReportServer <- function(id, df_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent({
        purrr::map_lgl(df_ls, ~ retrieval_done(.x()))
        },{
          print("test activate show report")
        nr_datasets <- purrr::map_lgl(df_ls, ~ retrieval_done(.x())) %>% sum()
        nr_datasets_total <- purrr::map_lgl(df_ls, ~ valid_input(.x())) %>% sum()
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
ProgressbarUpdateServer <- function(id, df_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent({purrr::map_lgl(df_ls, ~ retrieval_done(.x()))},{
        nr_datasets <- purrr::map_lgl(df_ls, ~ retrieval_done(.x())) %>% sum()
        nr_datasets_total <- purrr::map_lgl(df_ls, ~ valid_input(.x())) %>% sum()
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


