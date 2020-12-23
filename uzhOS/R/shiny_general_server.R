#' Server function for \code{\link{shinyApp_general}}
#'
#' @param con db connection function call, e.g. odbc::dbConnect(odbc::odbc(), "PostgreSQL")
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
shiny_general_server <-  function(con, orcid_access_token){
  con_quosure <- rlang::eval_tidy(rlang::enquo(con))
  con <- rlang::eval_tidy(con_quosure)
  function(input, output,session) {
  
  # somewhat random user id
  session$userData <- list()
  session$userData$userid <- paste0("userID-",as.integer(Sys.time())%%99999)
  session$userData$col_nr <- sample(seq_len(6),1)
  sps <- reactive(session$userData)
  
  tbl_merge <- reactiveVal(NULL)
  
  shinyhelper::observe_helpers(session = session,
                               help_dir = system.file("inst","extdata","helpfiles",package = "uzhOS"))
  # oa help
  oa_diagram_Server("oa_diag")
  # data
  d <- reactiveValues(sps=sps, processing = FALSE)
  
  # input values for selection
  selection_ls <- reactiveValues(init=TRUE, redraw=FALSE)
  
  # plots and tables
  p_t <- reactiveValues()
  # updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE)
  
  df_zora <- reactiveVal(empty_zora())
  df_orcid <- reactiveVal(empty_orcid())
  df_publons <- reactiveVal(empty_publons())
  df_scholar <- reactiveVal(empty_scholar())
  df_pubmed <- reactiveVal(empty_pubmed())
  
  df_ls <- list(df_zora, df_orcid, df_pubmed, df_publons, df_scholar)
  all_poss_datasets <- c("zora","orcid","pubmed","publons","scholar")
  
  # deactivate 'show_report' button while processing and set show_report in d (d$show_report)
  DeactivateShowReportServer("show_report", d)
  
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
  observeEvent(input$scholar_matching_with_crossref,{
    shinyFeedback::feedback(
      "scholar_matching_with_crossref", 
      !input$scholar_matching_with_crossref,
      "Possibly less publications from Google scholar will be matched to other sources."
    )
  })
  
  # wait for clicking of "show_report", then retrieve all data asynchronously 
  scholarModalServer("show_report", df_scholar, input$scholar_matching_with_crossref)
  
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
    success_ls <- purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons), ~successfully_retrieved(.x()))
    merged_ls <- purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons), ~try_to_merge(.x()))
    if (any(success_ls & !merged_ls)){
      shiny_print_logs(paste("start to merge:", paste(list(df_zora,df_orcid,df_pubmed,df_publons)[success_ls & !merged_ls][[1]]() %>% name(),collapse = ", ")), sps)
      shiny_print_logs(paste("will (or has) merge:", paste(c("df_zora","df_orcid","df_pubmed","df_publons")[success_ls],collapse = ", ")), sps)
      future(seed=NULL,{
        # con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
        con <- rlang::eval_tidy(con_quosure)
        create_combined_data(df_orcid, df_pubmed, df_zora, df_publons, con)
        }, globals=list(con_quosure=con_quosure,
                        create_combined_data=create_combined_data,
                        df_orcid=isolate(df_orcid()),
                        df_pubmed=isolate(df_pubmed()),
                        df_zora=isolate(df_zora()),
                        df_publons=isolate(df_publons()))) %...>%
        tbl_merge()
      assign_to_reactiveVal(c(df_zora,df_orcid,df_pubmed,df_publons)[success_ls & !merged_ls][[1]], "try_to_merge", TRUE)
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
    req(tbl_merge())
    d$datainmerge <- tbl_merge() %>% dplyr::select(starts_with("in_")) %>% names() #%>% stringr::str_replace("in_","")
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
                          '%>%' = magrittr::'%>%')) %...>% 
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
  })
  
  
  # combine scholar with tbl_merge
  observeEvent({successfully_retrieved(df_scholar());d$do_scholar_match},{
    if (d$do_scholar_match && successfully_retrieved(df_scholar())){
      shiny_print_logs("merge scholar", sps)
      future(seed=NULL,{
        tmpscholar <- df_scholar_matching(tbl_merge_iso, df_scholar_iso, scholar_matching_with_crossref)
        merge_scholar_into_tbl_merge(tbl_merge_iso, tmpscholar)
      },  globals = list('%>%'= magrittr::'%>%',
                         df_scholar_matching=df_scholar_matching,
                         df_scholar_iso=isolate(df_scholar()),
                         scholar_matching_with_crossref=input$scholar_matching_with_crossref,
                         tbl_merge_iso=isolate(tbl_merge()),
                         merge_scholar_into_tbl_merge=merge_scholar_into_tbl_merge
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
    shinyjs::show(id = "shinyjsbox_pubmetric_table")
    shinyjs::show(id = "shinyjsbox_pubmetric_plot")
    shinyjs::show(id = "shinyjsbox_oa_perc_time")
    shinyjs::show(id = "shinyjsbox_fulltext_download")
    # updateBox("box_author_input",action = "toggle")
    # update and show selections
    # update single selection for plots and tables
    d$all_selection_choices <- colnames(d$m)[grep("in_",colnames(d$m))]
    shiny_print_logs(paste("datasets in tbl_merge:", paste(d$all_selection_choices,collapse = ", ")), sps)
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
                    (year >= input$range_year[1]) | is.na(year),
                    year <= input$range_year[2] | is.na(year))
    d$m_upsetplot <-  d$m %>% 
      dplyr::mutate(year=as.integer(year)) %>% 
      dplyr::filter(overall_oa %in% input$oa_status_filtered_table,
                    (year >= input$range_year[1]) | is.na(year),
                    year <= input$range_year[2] | is.na(year))
    d$m_sub <- m_filt
    d$m_sub_sel <- m_filt
  })
  
  # update summary when subset changes
  oaSummaryServer("oa_summary", d)
  # observeEvent({d$m_sub},{
  #   # summary of subset table
  #   overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa") %>% as.character()
  #   # overall_oa_status[is.na(overall_oa_status)] <- "unknown"
  #   # levels(overall_oa_status) <- names(open_cols_fn())
  #   output$sub_summary <- renderPrint({
  #     table(overall_oa_status,useNA = "ifany")
  #   })
  #   # total number of publications
  #   tmp_total <- ifelse(!is.numeric(length(overall_oa_status)), 0, length(overall_oa_status))
  #   # total number of open publications
  #   tmp_open <- ifelse(tmp_total != 0, ((tmp_total-sum(overall_oa_status == "closed"))/tmp_total)*100, 0)
  #   # total number of open publications without blue
  #   tmp_open_blue <- ifelse(tmp_total != 0,((tmp_total-sum(overall_oa_status %in% c("closed","blue")))/tmp_total)*100, 0)
  #   # simple summary bar of oa status
  #   output$oa_summary_histogram_simple <- renderPlot({
  #     simple_oa_summary_histogram(overall_oa_status)
  #   },height = 50)
  #   
  #   output$pgb_closed <- renderUI(
  #     boxPad(color = "teal",
  #            # to change the color of "teal"
  #            tags$style(HTML(".bg-teal {
  #          background-color:#F0F8FF!important;
  #          color:#000000!important;
  #         }")),
  #            h4("Summary filtered data"),
  #            descriptionBlock(
  #              text = verbatimTextOutput("sub_summary")
  #            ),
  #            plotOutput("oa_summary_histogram_simple",height = 50),
  #            descriptionBlock(
  #              text = paste("Percentage open:", 
  #                           signif(tmp_open,digits=3),
  #                           "%")
  #            ),
  #            shinydashboardPlus::progressBar(value = tmp_open),
  #            descriptionBlock(
  #              text = paste("Percentage open (without blue):",
  #                           signif(tmp_open_blue,digits=3),
  #                           "%")
  #            ),
  #            shinydashboardPlus::progressBar(value = tmp_open_blue)
  #     )
  #   )
  # })

  
  ## oa status upset plot -----------------------------------------------------
  p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m_upsetplot)},error=function(e) {print(e);ggplot() + geom_blank()})})
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
  
  output$plot_dep_fac_year_val_bar_unsized <- renderPlotly({
    req(d_dep$wide_data_for_plotly)
    plot_fac_dep(d_dep$wide_data_for_plotly,fac_dep_filt,plot_type = "year_val_bar")
  })
  output$plot_dep_fac_year_val_bar <- renderUI({
    plotlyOutput(outputId = "plot_dep_fac_year_val_bar_unsized")
  })
  
}
}