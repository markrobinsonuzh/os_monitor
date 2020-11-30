#' Action Button 'show_report'
#'
#' @param id namesapce
#' @param label label
#'
#' @import shiny
#' @export
#'
showReportUI <- function(id, label = "Show Report") {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_report"), label = label),
  )
}

#' Save 'show_report' in reactive value
#' 
#' @param id namespace
#' @param d reactiveValues
#' 
#' @import shiny
#' @export
#' 
showReportValueServer <- function(id, d) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        shiny_print_logs(paste("show_report pressed, start retrieval... (pressed",input$show_report,"times)"), sps)
        d$show_report <- input$show_report
      })
    }
  )
}

#' Google scholar modal info
#' 
#' @param id namespace
#' @param df_scholar \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_scholar}}
#' 
#' @import shiny
#' @export
#' 
scholarModalServer <- function(id, df_scholar) {
  moduleServer(
    id,
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
#' @param df_zora \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_zora}}
#' @param con database connection quosure, e.g. rlang::quo(odbc::dbConnect(odbc::odbc(), "PostgreSQL"))
#' @param sps \code{\link{shiny_print_logs}} 'hostname' (for better logs)
#'
#' @export
#' @import shiny
#' @import promises 
#' @import future
#' 
createZoraServer <- function(id, df_zora, con, sps) {
  con_quosure <- rlang::eval_tidy(rlang::enquo(con))
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_zora, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_zora, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_zora, "successfully_retrieved", FALSE)
        if (valid_input(df_zora())){
          shiny_print_logs(paste("retrieve zora",input$show_report), sps)
        future(seed=NULL,{
          # con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
          con <- rlang::eval_tidy(con_quosure)
          create_zora(author_vec, con)
        }, globals = list(con_quosure=con_quosure,
                          # empty_zora=empty_zora,
                          # tbl=dplyr::tbl,
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
#' @param df_orcid \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_orcid}}
#' @param orcid_access_token Orcid access token, see \code{\link[rorcid]{orcid_auth}}
#' @param sps \code{\link{shiny_print_logs}} 'hostname' (for better logs)
#' 
#' 
#' @export
#' @import shiny
#' @import promises 
#' @import future
#' @importFrom magrittr %>% 
#' 
createOrcidServer <- function(id, df_orcid, orcid_access_token, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_orcid, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_orcid, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_orcid, "successfully_retrieved", FALSE)
        if (valid_input(df_orcid())){
          shiny_print_logs(paste("retrieve orcid:",input_value(df_orcid())), sps)
          assign_to_reactiveVal(df_orcid, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            # con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
            dplyr::mutate(retrieve_from_orcid(orcid, orcid_access_token=orcid_access_token), doi = tolower(doi))    
          }, globals = list(# empty_orcid=empty_orcid,
                            # tbl=dplyr::tbl,
                            retrieve_from_orcid=retrieve_from_orcid,
                            orcid=isolate(input_value(df_orcid())),
                            orcid_access_token=orcid_access_token)) %...>% 
            to_tibble_reac_template(df_orcid()) %...>% 
            `retrieval_done<-`(TRUE) %...>% 
            df_orcid()
        }
      })
    }
  )
}



#' check results module
#'
#' @param id namespace
#' @param df_whatever reactiveVal of class tibble_reac
#' @param sps username, etc, 
#'
#' @import shiny
#' @export
ResultCheckServer <- function(id, df_whatever, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(retrieval_done(df_whatever()),{
        req(input$show_report)
        if (retrieval_done(df_whatever()) && !successfully_retrieved(df_whatever())){
          check_and_set_successfull_retrieval(df_whatever)
          shiny_print_logs(paste("retrieval of", name(df_whatever()), ":", 
                                 ifelse(successfully_retrieved(df_whatever()),"Success","Failure!")), 
                           sps)
        }
      })
    }
  )
}


#' create scholar module
#'
#' @param id for namespace
#' @param df_scholar \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_scholar}}
#' @param sps \code{\link{shiny_print_logs}} 'hostname' (for better logs)
#'
#' @export
#' @import shiny
#' @import promises 
#' @import future
#' 
createScholarServer <- function(id, df_scholar, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_scholar, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_scholar, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_scholar, "successfully_retrieved", FALSE)
        if (valid_input(df_scholar())){
          shiny_print_logs(paste("retrieve scholar",input_value(df_scholar())), sps)
          assign_to_reactiveVal(df_scholar, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            retrieve_from_scholar(scholar)
          }, globals = list(#empty_scholar=empty_scholar,
                            # tbl=dplyr::tbl,
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
#' @param df_pubmed \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_pubmed}}
#' @param sps \code{\link{shiny_print_logs}} 'hostname' (for better logs)
#'
#' @export
#' @import shiny
#' @import promises 
#' @import future
#' 
createPubmedServer <- function(id, df_pubmed, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        assign_to_reactiveVal(df_pubmed, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_pubmed, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_pubmed, "successfully_retrieved", FALSE)
        if (valid_input(df_pubmed())){
          shiny_print_logs(paste("retrieve pubmed",input_value(df_pubmed())), sps)
          assign_to_reactiveVal(df_pubmed, "try_to_retrieve", TRUE)
          future(seed=NULL,{
            tryCatch({retrieve_from_pubmed(pubmed)},
                     error=function(e) empty_pubmed())
          }, globals = list(#empty_pubmed=empty_pubmed,
                            # tbl=dplyr::tbl,
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
#' @param df_publons \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_publons}}
#' @param sps \code{\link{shiny_print_logs}} 'hostname' (for better logs)
#'
#' @export
#' @import shiny
#' @import promises 
#' @import future
#' 
createPublonsServer <- function(id, df_publons, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$show_report,{
        req(input$show_report)
        assign_to_reactiveVal(df_publons, "try_to_retrieve", FALSE)
        assign_to_reactiveVal(df_publons, "retrieval_done", FALSE)
        assign_to_reactiveVal(df_publons, "successfully_retrieved", FALSE)
        if (valid_input(df_publons())){
          shiny_print_logs(paste("retrieve publons",input_value(df_publons())), sps)
          future(seed=NULL,{
            tryCatch({retrieve_from_publons(publons)},
                     error=function(e) {return(empty_publons())})
            }, globals = list(empty_publons=empty_publons,
                              # tbl=dplyr::tbl,
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





#' Progressbar ui module
#'
#' @param id namespace
#'
#' @import shiny
#' @export
#'
ProgressbarUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::progressBar(id = ns("pb_data_retrieval"), value = 0, total = 100, title = "", display_pct = TRUE)
    )
}


#' progressbar server reset 
#'
#' @param id for namespace
#'
#' @export
#' @import shiny
ProgressbarCreateServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({input$show_report},{
        updateProgressBar(
          session = session,
          id = "pb_data_retrieval",
          value = 0, total = 100
        )
        shinyjs::show("pd_data_retrieval")
      })  
    }
  )
}

#' show report deactivate server module
#' 
#' reset and deactivate various values
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' 
DeactivateShowReportServer <- function(id, d) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({input$show_report},{
        shiny_print_logs("deactivate show report, set data to NULL", d$sps)
        shinyjs::disable("show_report")
        d$show_report <- input$show_report
        d$processing <- TRUE
        d$m <- d$m_filt <- d$m_filt_sub <-  NULL
      })  
    }
  )
}

#' check for completion of retrieval server
#'
#' @param id for namespace
#' @param df_ls list of reactive values containing dfs, e.g. `list(df_orcid,df_pubmed)`
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @importFrom magrittr %>% 
ActivateShowReportServer <- function(id, df_ls, d) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({d$m},{
        shiny_print_logs("check activation of show report", d$sps)
        nr_datasets <- purrr::map_lgl(df_ls, ~ retrieval_done(.x())) %>% sum()
        nr_datasets_total <- purrr::map_lgl(df_ls, ~ valid_input(.x())) %>% sum()
        if(nr_datasets==nr_datasets_total && "in_pubmetric" %in% names(d$m)){
          shinyjs::enable("show_report")
        }
      })  
    }
  )
}


#' progressbar update server
#'
#' @param id for namespace
#' @param df_ls list of reactive values containing dfs, e.g. `list(df_orcid,df_pubmed)`
#'
#' @export
#' @import shiny
#' @importFrom magrittr %>% 
ProgressbarUpdateServer <- function(id, df_ls) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({purrr::map_lgl(df_ls, ~ retrieval_done(.x()))},{
        nr_datasets <- purrr::map_lgl(df_ls, ~ retrieval_done(.x())) %>% sum()
        nr_datasets_total <- purrr::map_lgl(df_ls, ~ valid_input(.x())) %>% sum()
        updateProgressBar(
          session = session,
          id = "pb_data_retrieval",
          value = nr_datasets/nr_datasets_total*100, total = 100
        )
        if(nr_datasets==nr_datasets_total){
          shinyjs::hide("pd_data_retrieval")
        }
      })  
    }
  )
}


