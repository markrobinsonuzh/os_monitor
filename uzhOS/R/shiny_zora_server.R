#' Title
#'
#' @param con db connection function call, e.g. odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#' @param unique_authorkeys_processed tibble
#' @param all_oa_status tibble
#' @param orgtree tree
#' @param fac_dep_filt tibble
#' @param orcid_access_token Access Token for orcid, 
#'  See \code{\link[rorcid]{orcid_auth}}
#'  
#' @return server
#' 
#' @import shiny 
#' @import dplyr 
#' @import ggplot2 
#' @import stringr 
#' @import shinyjs 
#' @import plotly 
#' @import DBI 
#' @import shinyWidgets 
#' @import promises 
#' @import future 
#' @import shinydashboard 
#' @import shinydashboardPlus
#' @importFrom magrittr %>% 
#' 
#' @export
#'
shiny_zora_server <-  function(con,
                               unique_authorkeys_processed,
                               all_oa_status,
                               orgtree,
                               fac_dep_filt,
                               orcid_access_token){
  con_quosure <- rlang::eval_tidy(rlang::enquo(con))
  con <- rlang::eval_tidy(con_quosure)
  function(input, output,session) {
  # # Save extra values in state$values when we bookmark
  # onBookmark(function(state) {
  #   print("d")
  #   for(nam in names(d)){
  #     print(nam)
  #     state$values$d[[nam]] <- d[[nam]]
  #   }
  #   print("selection_ls")
  #   for(nam in names(selection_ls)){
  #     print(nam)
  #     state$values$selection_ls[[nam]] <- selection_ls[[nam]]
  #   }
  #   print("p_t")
  #   for(nam in names(p_t)){
  #     print(nam)
  #     state$values$p_t[[nam]] <- p_t[[nam]]
  #   }
  #   print("df_ls")
  #   for(i in seq_along(df_ls)){
  #     state$values$df[[all_poss_datasets[i]]] <- df_ls[[i]]()
  #   }
  # })
  # 
  # # Read values from state$values when we restore
  # onRestore(function(state) {
  #   print("Restore......................")
  #   print("d")
  #   for(nam in names(state$values$d)){
  #     print(nam)
  #     d[[nam]] <- state$values$d[[nam]]
  #   }
  #   print("selection_ls")
  #   for(nam in names(state$values$selection_ls)){
  #     print(nam)
  #     selection_ls[[nam]] <- state$values$selection_ls[[nam]]
  #   }
  #   print("p_t")
  #   for(nam in names(state$values$p_t)){
  #     print(nam)
  #     p_t[[nam]] <- state$values$p_t[[nam]]
  #   }
  #   print("df")
  #   for(i in seq_along(state$values$df)){
  #     df_ls[[i]](state$values$df[[names(state$values$df)[i]]])
  #   }
  # })
  
  # somewhat random user id
  session$userData <- list()
  session$userData$userid <- paste0("userID-",as.integer(Sys.time())%%99999)
  session$userData$col_nr <- sample(seq_len(6),1)
  sps <- reactive(session$userData)
  # observe({
  #   req(input$histogram_click, d$m_sub)
  #   print(input$histogram_click)
  #   lvls <- sort(unique(d$m_sub$year))
  #   name <- lvls[round(input$histogram_click$x)]
  #   print(name)
  #   })
  
  # have some tabs hidden at the start
  # hideTab("author_plots_tables", "Bibtex")
  # hideTab("author_plots_tables", "Upset Plot")
  ### Author ###################################################################
  
  shinyhelper::observe_helpers(session = session,
                               help_dir = system.file("extdata","helpfiles",package = "uzhOS"))
  # oa help
  oa_diagram_Server("oa_diag")
  # data
  d <- reactiveValues(sps=sps, processing = FALSE)
  # input values for selection
  selection_ls <- reactiveValues(init=TRUE, redraw=FALSE)
  
  # plots and tables
  p_t <- reactiveValues()
  updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE)
  
  df_zora <- reactiveVal(empty_zora())
  df_orcid <- reactiveVal(empty_orcid())
  df_publons <- reactiveVal(empty_publons())
  df_scholar <- reactiveVal(empty_scholar())
  df_pubmed <- reactiveVal(empty_pubmed())
  
  df_ls <- list(df_zora, df_orcid, df_pubmed, df_publons, df_scholar)
  all_poss_datasets <- c("zora","orcid","pubmed","publons","scholar")
  
  # timeout
  time_out_server("timeout", sps)
  
  # show available alias if any author_search given, otherwise hide
  observe({
    alias_selected_show_Server("alias_selected",input$author_search)
    # find and parse aliases of authors
    alias_selected_Server("alias_selected",input$author_search, con, fac_vec=d$fac_vec, dep_vec=d$dep_vec)
    if (is.null(input$author_search)){
      disable("faculty_search")
    } else{
      enable("faculty_search")
    }
  })
  
  # update department filter, save faculty
  observeEvent(input$faculty_search,{
    if("all" %in% input$faculty_search | is.null(input$faculty_search)){
      disable("department_search")
      d$fac_vec <- NULL
      updateSelectizeInput(session, "department_search", choices="all", selected = "all")
    } else {
      enable("department_search")
      d$fac_vec <- input$faculty_search
      deps <- unique_fac_dep(fac_dep_filt,type="fac_dep") %>% 
        dplyr::filter(fac %in% input$faculty_search) %>% 
        dplyr::pull(dep)
      updateSelectizeInput(session, "department_search", choices=c("all",sort(deps)), selected = "all")
    }
  })
  
  # save department
  observe({
    if(("all" %in% input$department_search | is.null(input$department_search))){
      d$dep_vec <- NULL
    } else{
      d$dep_vec <- input$department_search
    }
  })
  
  # create author entries with metadata
  observeEvent(d$dep_vec,{
    alias_selected_Server("alias_selected",input$author_search, con, fac_vec=d$fac_vec, dep_vec=d$dep_vec)
  })
  
  # Orcid and author vector into reactive value
  orcid_auth_react <- alias_selected_orcid_auth_Server("alias_selected")
  observeEvent({orcid_auth_react()},{
    req(NS("alias_selected",input$aliases_selected))
    orcid_auth_tmp <- orcid_auth_react()
    if (!is.null(orcid_auth_tmp[["orcid"]])){
      assign_to_reactiveVal(df_orcid, "input_value", orcid_auth_tmp[["orcid"]])
    } else {
      assign_to_reactiveVal(df_orcid, "input_value", "")
    }
    shiny_print_logs(paste("orcid:",orcid_auth_tmp[["orcid"]]), sps)
    if (!is.null(orcid_auth_tmp[["author_vec"]])){
      assign_to_reactiveVal(df_zora, "input_value", orcid_auth_tmp[["author_vec"]])
    } else {
      assign_to_reactiveVal(df_zora, "input_value", "")
    }
    shiny_print_logs(paste("zora:",orcid_auth_tmp[["author_vec"]]), sps)
    
    if (!is.null(input_value(df_zora())) || input_value(df_zora()) != ""){
      assign_to_reactiveVal(df_zora, "valid_input", TRUE)
    } else {
      assign_to_reactiveVal(df_zora, "valid_input", FALSE)
    }
  })
  
  # automated string operations, enable, disable report buttons
  observeEvent(input_value(df_zora()),{
    shiny_print_logs(paste("zora update input"), sps)
    updateTextInput(session,NS("input_check", "orcid"),value=input_value(df_orcid()))
    if (is.null(input_value(df_zora())) || input_value(df_zora()) == "" || d$processing){
      disable(NS("show_report", "show_report"))
      disable("report")
      disable(NS("input_check", "activate_pubmed"))
    } else{
      enable(NS("show_report", "show_report"))
      enable("report")
      enable(NS("input_check", "activate_pubmed"))
    }
  })
  
  # deactivate 'show_report' button while processing and set show_report in d (d$show_report)
  DeactivateShowReportServer("show_report", d, df_ls, tbl_merge)
  
  # remove input of dynamic UI for selection ("in_zora", etc.)
  datasetSelectionsRemoveServer("selection_standard", d, selection_ls, df_ls, length(all_poss_datasets)+1)
  
  
  # pubmed activating, model dialog and query generation
  pubmedActivateServer("input_check", df_zora, df_orcid, df_pubmed, con)
  observeEvent({input_value(df_pubmed())},{
    updateTextAreaInput(session,NS("input_check", "pubmed"),value=input_value(df_pubmed()))
  })
  
  # check for valid inputs
  orcidCheckServer("input_check", df_orcid, con)
  pubmedCheckServer("input_check", df_pubmed)
  publonsCheckServer("input_check", df_publons)
  scholarCheckServer("input_check", df_scholar)
  crossrefInputServer("input_check", d)
  zoteroInputServer("input_check", d)
  observeEvent(d$scholar_matching_with_crossref,{
    shinyFeedback::feedback(
      NS("input_check","scholar_matching_with_crossref"), 
      !d$scholar_matching_with_crossref,
      "Possibly less publications from Google scholar will be matched to other sources."
    )
  })
  
  # wait for clicking of "show_report", then retrieve all data asynchronously 
  scholarModalServer("show_report", df_scholar, d$scholar_matching_with_crossref)
  tbl_merge <- reactiveVal(NULL)
  
  createZoraServer("show_report", df_zora, con_quosure, sps)
  ResultCheckServer("show_report", df_zora, sps)
  
  createOrcidServer("show_report", df_orcid, orcid_access_token, sps)
  ResultCheckServer("show_report", df_orcid, sps)
  
  createPublonsServer("show_report", df_publons, sps)
  ResultCheckServer("show_report", df_publons, sps)
  
  createScholarServer("show_report", df_scholar, sps)
  ResultCheckServer("show_report", df_scholar, sps)
  
  createPubmedServer("show_report", df_pubmed, sps)
  ResultCheckServer("show_report", df_pubmed, sps)
  
  # df_pubmetric <- reactiveVal(empty_pubmetric())
  # merge results when they become available
  observeEvent({purrr::map(df_ls, ~ .x())},{
    shiny_print_logs("observe df_ls", sps)
    # print(purrr::map(df_ls, ~ attributes(.x())))
    if(!is.null(d$processing) && d$processing){
      # attributes table
      attr_tib <- purrr::map(c(df_zora,df_orcid,df_pubmed,df_publons, df_scholar), 
                             ~ tibble::as_tibble(attributes(.x())[c("successfully_retrieved","try_to_merge","retrieval_done", "valid_input")])) %>% 
        purrr::reduce(rbind) %>% 
        dplyr::mutate(df_name=all_poss_datasets)
      # if not at least one successfully retrieved, stop
      if (all(c(attr_tib$retrieval_done & !attr_tib$successfully_retrieved)[attr_tib$valid_input])){
        shiny_print_logs("only unsuccessfull retrievals!", sps)
        shinyjs::enable(NS("show_report","show_report"))
        d$processing <- FALSE
        shinyWidgets::show_alert(title="No publications found!", text = "The publication records of the given IDs seem to be empty.")
      }
      # attributes table without scholar
      attr_tib_noscholar <- attr_tib %>% dplyr::filter(df_name != "scholar")
      # merge if needed
      if (any(attr_tib_noscholar$successfully_retrieved & !attr_tib_noscholar$try_to_merge)){
        shiny_print_logs(paste("start to merge:", paste(list(df_zora,df_orcid,df_pubmed,df_publons)[attr_tib_noscholar$successfully_retrieved & !attr_tib_noscholar$try_to_merge][[1]]() %>% name(),collapse = ", ")), sps)
        shiny_print_logs(paste("will (or has) merge:", paste(c("df_zora","df_orcid","df_pubmed","df_publons")[attr_tib_noscholar$successfully_retrieved],collapse = ", ")), sps)
        future(seed=NULL,{
          con <- rlang::eval_tidy(con_quosure)
          create_combined_data(df_orcid, df_pubmed, df_zora, df_publons, con)
        }, globals=list(con_quosure=con_quosure,
                        create_combined_data=create_combined_data,
                        df_orcid=isolate(df_orcid()),
                        df_pubmed=isolate(df_pubmed()),
                        df_zora=isolate(df_zora()),
                        df_publons=isolate(df_publons()))) %...>%
          tbl_merge()
        assign_to_reactiveVal(c(df_zora,df_orcid,df_pubmed,df_publons)[attr_tib_noscholar$successfully_retrieved & !attr_tib_noscholar$try_to_merge][[1]], "try_to_merge", TRUE)
      }
      
    }
  })
  
  # check if all except scholar are merged (mainly used for progressbar)
  observeEvent({tbl_merge()},{
    req(tbl_merge())
    shiny_print_logs("check if df in tbl_merge", sps)
    datainmerge <- tbl_merge() %>% 
      dplyr::select(starts_with("in_")) %>% 
      names() %>% 
      stringr::str_replace("^in_","")
    for(tmpdf in df_ls[all_poss_datasets %in% datainmerge]){
      assign_to_reactiveVal(tmpdf,"successfully_merged", TRUE)
    }
  })
  
  # check if all except scholar are merged, then give signal to merge scholar as well
  observeEvent({tbl_merge(); df_scholar()},{
    if(!is.null(d$processing) && d$processing){
      shiny_print_logs("tbl_merge or df_scholar", sps)
      if(is.null(tbl_merge())){
        d$datainmerge <- character(0)
      } else {
        d$datainmerge <- tbl_merge() %>% dplyr::select(starts_with("in_")) %>% names() #%>% stringr::str_replace("in_","")
      }
      d$dataininput <- purrr::map_lgl(all_poss_datasets, function(df){
        if (valid_input(eval(sym(paste0("df_",df)))())){
          if (retrieval_done(eval(sym(paste0("df_",df)))()) && !successfully_retrieved(eval(sym(paste0("df_",df)))())){
            FALSE
          } else {
            TRUE
          }
        } else {
          FALSE
        }
      })
      # if all retrieved, but scholar not yet merged, give signal to merge scholar
      if ((length(d$datainmerge) == (sum(d$dataininput)-1)) && successfully_retrieved(df_scholar())){
        shiny_print_logs("signal scholar matching", sps)
        d$do_scholar_match <- TRUE
        # if all merged (including scholar, or scholar not present) get citation metrics
      } else if (length(d$datainmerge) == (sum(d$dataininput))){
        if(input$retrieve_pubmetric){
          shiny_print_logs("get pubmetrics", sps)
          future(seed=NULL,{
            retrieve_from_pubmed_with_doi(tbl_merge_iso[["doi"]]) %>% 
              dplyr::right_join(tbl_merge_iso, by= "doi", suffix = c(".pubmetric",""))  %>% 
              dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))
          }, globals = list(retrieve_from_pubmed_with_doi=retrieve_from_pubmed_with_doi,
                            tbl_merge_iso=isolate(tbl_merge()),
                            '%>%'= magrittr::'%>%')) %...>% 
            tbl_merge()
        }
        
        shiny_print_logs("reset attributes", sps)
        for(tmpdf in df_ls){
          assign_to_reactiveVal(tmpdf, "try_to_retrieve", FALSE)
          assign_to_reactiveVal(tmpdf, "retrieval_done", FALSE)
          assign_to_reactiveVal(tmpdf, "successfully_retrieved", FALSE)
          assign_to_reactiveVal(tmpdf, "try_to_merge", FALSE)
        }
        
        # ActivateShowReportServer("show_report", df_ls, d)
        shiny_print_logs("enable show_report", sps)
        shinyjs::enable(NS("show_report","show_report"))
        d$processing <- FALSE
        updateBox("box_author_input",action = "toggle")
      }
    }
  })
  
  # combine scholar with tbl_merge
  observeEvent({successfully_retrieved(df_scholar());d$do_scholar_match},{
    if (d$do_scholar_match && successfully_retrieved(df_scholar())){
      shiny_print_logs("merge scholar", sps)
      future(seed=NULL,{
        con <- rlang::eval_tidy(con_quosure)
        tmpscholar <- df_scholar_matching(tbl_merge_iso, df_scholar_iso, scholar_matching_with_crossref, scholar_matching_with_zotero)
        tbl_merge_new <- merge_scholar_into_tbl_merge(tbl_merge_iso, tmpscholar)
        ind_unknownoa <- which(tbl_merge_new$overall_oa == "unknown")
        oaf <- oadoi_fetch_local(na.omit(tbl_merge_new$doi[ind_unknownoa]), con)
        tbl_merge_new <- dplyr::full_join(tbl_merge_new, oaf, by = "doi", suffix=c("", ".scholar"))
        if(!("oa_status.scholar" %in% names(tbl_merge_new))){
          tbl_merge_new <- dplyr::rename(tbl_merge_new, oa_status.scholar = oa_status)
        }
        dplyr::mutate(tbl_merge_new, 
                      overall_oa=factor(
                        ifelse(as.character(overall_oa) != "unknown",
                               as.character(overall_oa),
                               ifelse(is.na(as.character(oa_status.scholar)),
                                      "unknown",
                                      as.character(oa_status.scholar))),
                        levels = names(open_cols_fn())))
      },  globals = list('%>%'= magrittr::'%>%',
                         df_scholar_matching=df_scholar_matching,
                         df_scholar_iso=isolate(df_scholar()),
                         scholar_matching_with_crossref=d$scholar_matching_with_crossref,
                         scholar_matching_with_zotero=d$scholar_matching_with_zotero,
                         tbl_merge_iso=isolate(tbl_merge()),
                         merge_scholar_into_tbl_merge=merge_scholar_into_tbl_merge,
                         oadoi_fetch_local=oadoi_fetch_local,
                         con_quosure=con_quosure,
                         open_cols_fn=open_cols_fn
      ))  %...>% 
        tbl_merge()
      d$do_scholar_match <- FALSE
    }
  })
  
  # progress bar
  ProgressbarCreateServer("show_report")
  ProgressbarUpdateServer("show_report", d)
  
  # save 'tbl_merge' in 'm' for downstream analysis
  observeEvent({tbl_merge()},{
    req(tbl_merge())
    d$m <- tbl_merge()
    d$m_sub <- tbl_merge()
    d$m_sub_sel <- tbl_merge()
    # hide upset plot if only one dataset available
    tmp_datainmerge <-  tbl_merge() %>% dplyr::select(starts_with("in_")) %>% 
      names()
    if (length(tmp_datainmerge) < 2){
      shinyjs::hide(id = "shinyjsbox_upsetplot")
    } else {
      shinyjs::show(id = "shinyjsbox_upsetplot")
    }
  # activate some stuff and preparation
    selection_ls$init <- TRUE
    selection_ls$redraw <- TRUE
    shinyjs::show(id = "shinyjsbox_author_filter")
    # shinyjs::show(id = "shinyjsbox_upsetplot")
    shinyjs::show(id = "shinyjsbox_histogram")
    shinyjs::show(id = "shinyjsbox_table")
    shinyjs::show(id = "shinyjsbox_bibtex")
    # shinyjs::show(id = "shinyjsbox_pubmetric_table")
    shinyjs::show(id = "shinyjsbox_pubmetric_plot")
    shinyjs::show(id = "shinyjsbox_oa_perc_time")
    shinyjs::show(id = "shinyjsbox_fulltext_download")
    # updateBox("box_author_input",action = "toggle")
    # update and show selections
    # update single selection for plots and tables
    d$all_selection_choices <- colnames(d$m)[grep("in_",colnames(d$m))]
    shiny_print_logs(paste("datasets in tbl_merge:", paste(d$all_selection_choices,collapse = ", ")), sps)
    
    # update year slider
    min_year_tbl_merge <- as.integer(min(na.omit(tbl_merge()$year)))
    max_year_tbl_merge <- as.integer(max(na.omit(tbl_merge()$year)))
    updateSliderInput(session,"range_year", min = min_year_tbl_merge, max = max_year_tbl_merge,
                      value = c(min_year_tbl_merge,max_year_tbl_merge),
                      step = 1)
  })
  
  # modules for updating selection UI
  datasetSelectionsServer("selection_standard", d, selection_ls)
  datasetSelectionsUpdateServer("selection_standard", d, selection_ls)
  redrawSelectionsServer("selection_standard", d, selection_ls)
  
  observeEvent({input$oa_status_filtered_table; input$range_year; d$m_sub_all_oa},{
    req(d$m_sub_all_oa)
    m_filt <- d$m_sub_all_oa %>% 
      dplyr::mutate(year=as.integer(year)) %>% 
      dplyr::filter(overall_oa %in% input$oa_status_filtered_table,
                    year >= input$range_year[1] | is.na(year),
                    year <= input$range_year[2] | is.na(year)) %>% 
      dplyr::arrange(desc(year))
    d$m_upsetplot <-  d$m %>% 
      dplyr::mutate(year=as.integer(year)) %>% 
      dplyr::filter(overall_oa %in% input$oa_status_filtered_table,
                    (year >= input$range_year[1]) | is.na(year),
                    year <= input$range_year[2] | is.na(year)) %>% 
      dplyr::arrange(desc(year))
    d$m_sub <- m_filt
    d$m_sub_sel <- m_filt
  })
  
  # update summary when subset changes
  oaSummaryServer("oa_summary", d)
  
  ## oa status upset plot -----------------------------------------------------
  p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m)},error=function(e) {print(e);ggplot() + geom_blank()})})
  output$plot_upset <- renderPlot({
    # req(d$m_sub)
    p_t$upset_plot()
  },res=100)
  
  ## selected plot  -----------------------------------------------------------
  p_t$selected_plot <- reactive({tryCatch({oa_status_time_plot(d$m_sub,
                                                               title = paste(paste0(d$in_selection,collapse = " + "), "OA Status"),
                                                               oa_status_used=overall_oa,use_plotly=TRUE)},
                                          error=function(e) {print(e);ggplot() + geom_blank()})})
  output$plot_selected <- renderPlotly({
    req(d$m_sub)
    p_t$selected_plot()
  })
  observeEvent(event_data("plotly_click", source = "C"),{
    req(d$m_sub)
    ev_dat <- event_data("plotly_click", source = "C")
    # only use existing oa status
    oa_order <- names(open_cols_fn()[names(open_cols_fn()) %in% unique(d$m_sub$overall_oa)])
    modulus_click <- ev_dat$curveNumber %% length(oa_order)
    d$plot_selected_ly_clicked <- list(year=ev_dat$x,
                                       oa_status=oa_order[modulus_click+1])
  })
  
  # popup table
  p_t$modal_selected_table <- reactive({req(d$plot_selected_ly_clicked)
    tryCatch({overall_closed_table(
      dplyr::filter(d$m_sub,
                    year==d$plot_selected_ly_clicked[["year"]],
                    overall_oa==d$plot_selected_ly_clicked[["oa_status"]]),
      oa_status_zora = FALSE)
    },error=function(e) DT::datatable(head(d$m,0)))})
  observeEvent(event_data("plotly_click", source = "C"),{
    showModal(modalDialog(DT::renderDataTable({
      p_t$modal_selected_table()
    }),title = "Selection", size="l",easyClose = TRUE))
  })
  
  ### selected closed table  ---------------------------------------------------
  # apply selection
  observeEvent(input$apply_DT_selection,{
    req(d$m_sub,d$m_sub_sel)
    d$m_sub_sel <- d$m_sub_sel[input$table_selected_closed_rows_selected,]
  })
  # reset selection
  observeEvent(input$reset_DT_selection,{
    req(d$m_sub)
    d$m_sub_sel <- d$m_sub
  })
  # render table
  p_t$selected_closed_table <- reactive({tryCatch({overall_closed_table(d$m_sub_sel, oa_status_zora = FALSE)},
                                                  error=function(e) DT::datatable(head(d$m,0)))})
  output$table_selected_closed <- DT::renderDataTable({
    req(d$m_sub_sel)
    p_t$selected_closed_table()
  })
  
  ### Pubmetric table  -------------------------------------------------------
  output$table_pubmetric <- DT::renderDataTable({
    req(d$m_sub)
    if("relative_citation_ratio" %in% names(d$m_sub)){
      d$m_sub %>% 
        dplyr::filter(!is.na(relative_citation_ratio)) %>% 
        dplyr::select(doi,relative_citation_ratio,nih_percentile,citation_count,in_pubmetric )
    }
  })
  
  output$plot_pubmetric <- renderUI({
    req(d$m_sub)
    if("relative_citation_ratio" %in% names(d$m_sub)){
      d$m_sub %>% 
        dplyr::filter(!is.na(relative_citation_ratio)) %>% 
        dplyr::select(doi,relative_citation_ratio,nih_percentile,citation_count,title, year,overall_oa, journal ) %>% 
        pubmed_citation_plotly()
    }
  })
  
  ### OA percent time table  ---------------------------------------------------
  p_t$oa_percent_time_table <- reactive({tryCatch({oa_percent_time_table(d$m,input$range_year)},error=function(e) ggplot() + geom_blank())})
  output$table_oa_percent_time <- DT::renderDataTable({
    req(d$m)
    p_t$oa_percent_time_table()
  })
  
  ### bibtex  ------------------------------------------------------------------
  
  bibtex_ls <- reactiveVal()
  BibtexConfirmationServer("bibtex", d)
  BibtexRetrieveServer("bibtex", d, bibtex_ls)
  BibtexObserveResultServer("bibtex", d, bibtex_ls)
  BibtexDownloadButtonServer("bibtex", bibtex_ls)
  
  
  ### sci-hub  -----------------------------------------------------------------
  
  sci_hub_pdf_links <- reactiveVal()
  ScihubConfirmationServer("scihub", d)
  ScihubObserveActionbuttonServer("scihub", d, sci_hub_pdf_links)
  ScihubObservePdflinksServer("scihub", d, sci_hub_pdf_links)
  ScihubRenderDTServer("scihub", d)
  
  ### Department ###############################################################
  
  d_dep <- reactiveValues()
  
  output$tree <- renderTree({
    orgtree
  })
  
  
  
  observeEvent(input$treeapply,{
    shinyjs::disable("treeapply")
    d_dep$chosen_orgs <- lapply(get_selected(input$tree), function(i) {
      if(attr(i,"stselected")){
        i
      }
    }) %>% unlist()
    if(length(d_dep$chosen_orgs) > 50){
      d_dep$chosen_orgs <- d_dep$chosen_orgs[1:50]
      showModal(modalDialog("Too many departments were selected for proper visualization. Only the first 50 are shown.",
                            title = "Number of Departments too large", size="s",easyClose = TRUE))
    }
    d_dep$publication_type_filtered <- input$publication_type_filtered
    d_dep$wide_data_for_plotly <- preprocess_fac_dep(fac_dep_filt,
                                                     col_to_plot = "dep", 
                                                     fac_chosen =  d_dep$chosen_orgs, 
                                                     publication_filter = d_dep$publication_type_filtered,
                                                     by_year = TRUE) %>% 
      dplyr::mutate(year=as.integer(year)) %>% 
      arrange_fac_dep(type_arr="Count",by_year = TRUE) %>% 
      tidyr::pivot_wider(names_from=type,values_from=value) %>% 
      highlight_key(~dep)
  })
  
  p_t$dep_plot <- reactive({tryCatch({plot_fac_dep(d_dep$wide_data_for_plotly,fac_dep_filt,plot_type = "year_val_bar")},
                                    error=function(e) ggplot() + geom_blank())})
  
  output$plot_dep_fac_year_val_bar_unsized <- renderPlotly({
    req(p_t$dep_plot())
    p_t$dep_plot()
  })
  output$plot_dep_fac_year_val_bar <- renderUI({
    plotlyOutput(outputId = "plot_dep_fac_year_val_bar_unsized")
  })
  
  observeEvent(p_t$dep_plot(),{
    shinyjs::enable("treeapply")
  })
  
  }
}
