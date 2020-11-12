# load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(shinyjs)
  library(plotly)
  library(DBI)
  library(shinyTree)
  library(shinyWidgets)
  library(promises)
  library(future)
  plan(multiprocess)
})
on_rstudio <- TRUE
if(on_rstudio){
  setwd("/srv/shiny-server/os_monitor/shiny_app")
  maindir <- file.path(getwd(),"..")
} else {
  setwd("/srv/shiny-server/")
  maindir <- getwd()
}
# functions for backend
devtools::load_all(file.path(maindir,"uzhOS"))
# library(uzhOS)
outdir <- file.path(maindir,"output")
datadir <- file.path(maindir,"data")
# token to get acces to orcid (currently Reto's token)
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")
use_sql <- TRUE
if(use_sql){
    con <- dbConnect(odbc::odbc(), "PostgreSQL")
  unique_authorkeys_processed <- tbl(con, "authorkeys") %>% 
    pull(authorname)
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
} 
options(shinyTree.defaultParser="tree")
print("all_org_unit_fac")
# summary of faculty and department oa status
fac_dep_filt <- tryCatch(readRDS(file.path(datadir, "fac_dep_filt.rds")), 
                         error = function(e){ all_org_unit_fac(con)})

all_oa_status <- unique(fac_dep_filt$oa_status)
mat_oa <- match(all_oa_status,names(open_cols_fn()))
ord_oa <- order(mat_oa)
all_oa_status <- names(open_cols_fn())[mat_oa[ord_oa]]
print("read tree")
orgtree <- readRDS(file.path(datadir, "orgtree.rds"))
### UI #########################################################################
ui <- navbarPage("Open science monitor UZH",
       tabPanel("Author OA explorer",
       fluidPage(
          useShinyjs(),
          shinyFeedback::useShinyFeedback(),
          div(id="Sidebar",sidebarPanel(width=6,
            # author name selection
            selectizeInput("author_search","Author search",NULL,selected = NULL, multiple = FALSE, 
                           options = list(maxOptions = 1000,placeholder="select author",maxItems=10)) %>% 
              shinyhelper::helper(type="markdown",
                                  title = "Author search input help",
                                  content = 'Author_search_input'),
            # faculty selection
            selectizeInput("faculty_search","Faculty Filter",c("all",sort(unique_fac_dep(fac_dep_filt,"fac"))),selected="all",multiple=TRUE) %>% 
              disabled(),
            # department selection
            selectizeInput("department_search","Department Filter","all",selected="all",multiple=TRUE) %>% 
              disabled(),
            # ui to show the author entries with additional information
            alias_selected_UI("alias_selected"),
            # Orcid input
            textInput(NS("input_check","orcid"),label = a("Orcid",href="https://orcid.org",target="_blank"), value=""),
            # Pubmed query input
            splitLayout(cellWidths = c("75%", "25%"),
                        textAreaInput(NS("input_check", "pubmed"),label = a("Pubmed Query",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"), value="") %>% 
                          shinyjs::disabled(),
                        actionButton(NS("input_check", "activate_pubmed"),HTML("Generate <br/> Pubmed <br/> Query")) %>% 
                          shinyjs::disabled()),
            # google scholar input
            textInput(NS("input_check", "scholar"),label = a("Google Scholar id",href="https://scholar.google.ch",target="_blank"), value="") %>% 
              shinyhelper::helper(type="markdown",
                                  title = "Google scholar id help",
                                  content = 'Google_scholar_help'),
            # publons input
            textInput(NS("input_check", "publons"),label = tags$div(tags$a("Publons id",href="https://publons.com",target="_blank"),
                                                 tags$span(class="help-block","(or if linked: ORCID, ResearcherID or TRUID)")), value=""),
            # aggregate data
            # disabled(actionButton(inputId = "show_report",label = "Show report")),
            showReportUI("show_report"),
            ProgressbarUI("show_report") #%>% shinyjs::hidden()
          )),
          # disabled(downloadButton("report", "Generate report"))
      mainPanel(
        actionButton("showSidebar", "Show sidebar") %>% shinyjs::hidden(),
        actionButton("hideSidebar", "Hide sidebar") %>% shinyjs::hidden(),
        splitLayout(cellWidths = c("25%", "75%"),
        # panel for filtering
        wellPanel(
          datasetSelectionsUpdateUI("selection_standard"),
          
          # splitLayout(cellWidths = c("25%", "75%"),
          #   checkboxGroupInput(inputId = "in_selection",label = "Data sets included","",inline = TRUE) %>%
          #     shinyjs::hidden(),
            verbatimTextOutput("sub_summary"),
          # ),
          sliderInput("range_year",label = "Cutoff year",min=2001,max = 2020,value=c(2001,2020))%>% 
            shinyjs::hidden(),
          selectizeInput("oa_status_filtered_table","OA status",names(open_cols_fn()),selected = names(open_cols_fn()), multiple = TRUE, 
                         options = list(maxOptions = 10,placeholder="select oa status",maxItems=10)) %>% 
            shinyjs::hidden(),
          # checkboxGroupInput("oa_status_filtered_table","OA status",
          #                    choices = names(open_cols_fn()),
          #                    selected = names(open_cols_fn()),
          #                    inline = TRUE) %>% 
            # shinyjs::hidden(),
        ),
        # output panel (tables, plots etc.)
        tabsetPanel(id = "author_plots_tables",
                    type = "tabs",
                    tabPanel("Upset Plot", plotOutput("plot_upset"),height="600px"),
                    tabPanel("Histogram", plotlyOutput("plot_selected") %>% 
                               shinyhelper::helper(type="markdown",
                                                   title = "Histogram selection help",
                                                   content = 'Histogram_selection')),
                    tabPanel("Table", 
                             # data_table_selection_UI("DT_author_selection")),
                             flowLayout(#cellWidths = c("30%","30%","60%"),
                               actionButton(inputId = "apply_DT_selection",label = "Apply selection") %>% 
                                          shinyjs::hidden(),
                               actionButton(inputId = "reset_DT_selection",label = "Reset selection") %>% 
                                 shinyjs::hidden(),
                               actionButton("create_bibtex","Get Bibtex citation") %>% 
                                 shinyjs::hidden()
                               ) %>% 
                                 shinyhelper::helper(type="markdown",
                                                   title = "Apply and Reset selection help",
                                                   content = 'Apply_and_Reset_selection'),
                             DT::dataTableOutput("table_selected_closed")),
                    tabPanel("Bibtex",
                             downloadButton("bibtex_download", "Download Bibtex citation") %>% 
                               shinyjs::hidden(),
                             verbatimTextOutput("bibsummary")
                             ),
                    tabPanel("Pubmed citations", DT::dataTableOutput("table_pubmetric")),
                    tabPanel("Pubmed citations Plot", uiOutput("plot_pubmetric")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time"))
        )
        )
    )
      )
      ),
      tabPanel("Department OA explorer",
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     shinyTree("tree", checkbox = TRUE, search=TRUE, theme="proton", themeIcons = FALSE, themeDots = FALSE),
                     checkboxGroupInput("publication_type_filtered",
                                        "Publication types included",
                                        choices=unique(fac_dep_filt$type),
                                        selected = "article"),
                     actionButton("treeapply",label = "Apply selection")
                   ),
                   mainPanel(
                     # plotlyOutput("plot_dep_fac_anim_year",height = "800px",width = "100%"),
                     # plotlyOutput("plot_dep_fac_dep_year",height = "800px",width = "100%"),
                     # plotlyOutput("plot_dep_fac_year_val_line",height = "800px",width = "100%"),
                     # plotlyOutput("plot_dep_fac_year_val_bar",height = "800px",width = "100%")
                     uiOutput("plot_dep_fac_year_val_bar")
                 ))
               )
      )
)

### Server #####################################################################
server = function(input, output,session) {
  observe({
    req(input$histogram_click, d$m_sub)
    print(input$histogram_click)
    lvls <- sort(unique(d$m_sub$year))
    name <- lvls[round(input$histogram_click$x)]
    print(name)
    })
  
  # have some tabs hidden at the start
  hideTab("author_plots_tables", "Bibtex")
  hideTab("author_plots_tables", "Upset Plot")
  ### Author ###################################################################
  observeEvent(input$showSidebar, {
    shinyjs::show(id = "Sidebar")
    shinyjs::hide(id="showSidebar")
    shinyjs::show(id = "hideSidebar")
  })
  observeEvent({input$hideSidebar;tbl_merge()}, {
    shinyjs::hide(id = "Sidebar")
    shinyjs::show(id="showSidebar")
    shinyjs::hide(id = "hideSidebar")
  })
  
  shinyhelper::observe_helpers(session = session)
  # data
  d <- reactiveValues(pubmed="",orcid="",publons="",scholar="", 
                      is_valid_pubmed=FALSE, is_valid_orcid=FALSE, is_valid_publons=FALSE, is_valid_scholar=FALSE,is_valid_zora=FALSE)
  # input values for selection
  selection_ls <- reactiveValues(init=TRUE)
  
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
    print("here")
    orcid_auth_tmp <- orcid_auth_react()
    print(orcid_auth_tmp)
    if (!is.null(orcid_auth_tmp[["orcid"]])){
      assign_to_reactiveVal(df_orcid, "input_value", orcid_auth_tmp[["orcid"]])
    } else {
      assign_to_reactiveVal(df_orcid, "input_value", "")
    }
    if (!is.null(orcid_auth_tmp[["author_vec"]])){
      assign_to_reactiveVal(df_zora, "input_value", orcid_auth_tmp[["author_vec"]])
    } else {
      assign_to_reactiveVal(df_zora, "input_value", "")
    }
    # d$orcid <- orcid_auth_tmp[["orcid"]]
    # d$author_vec <- orcid_auth_tmp[["author_vec"]]
    
    if (!is.null(input_value(df_zora())) || input_value(df_zora()) != ""){
      assign_to_reactiveVal(df_zora, "valid_input", TRUE)
    } else {
      assign_to_reactiveVal(df_zora, "valid_input", FALSE)
    }
    # d$is_valid_zora <- TRUE
  })

  # automated string operations, enable, disable report buttons
  observeEvent(input_value(df_zora()),{
    updateTextInput(session,NS("input_check", "orcid"),value=input_value(df_orcid()))
    if (is.null(input_value(df_zora())) || input_value(df_zora()) == ""){
        disable(NS("show_report", "show_report"))
        disable("report")
        disable(NS("input_check", "activate_pubmed"))
    } else{
        enable(NS("show_report", "show_report"))
        enable("report")
        enable(NS("input_check", "activate_pubmed"))
    }
  })
  # pubmed activating, model dialog and query generation
  pubmedActivateServer("input_check", df_zora, df_orcid, df_pubmed, con)
  observeEvent({input_value(df_pubmed())},{
    updateTextAreaInput(session,NS("input_check", "pubmed"),value=input_value(df_pubmed()))
  })

  # check for valid inputs
  orcidCheckServer("input_check", df_orcid)
  pubmedCheckServer("input_check", df_pubmed)
  publonsCheckServer("input_check", df_publons)
  scholarCheckServer("input_check", df_scholar)
  
  
  # deactivate 'show_report' button while processing
  DeactivateShowReportServer("show_report")
  ActivateShowReportServer("show_report", df_ls)
  
  # wait for clicking of "show_report", then retrieve all data asynchronously 
  scholarModalServer("show_report", df_scholar)
  tbl_merge <- reactiveVal(NULL)
  
  createZoraServer("show_report", df_zora)
  ResultCheckServer("show_report", df_zora)
  
  createOrcidServer("show_report", df_orcid)
  ResultCheckServer("show_report", df_orcid)
  
  createPublonsServer("show_report", df_publons)
  ResultCheckServer("show_report", df_publons)
  
  createScholarServer("show_report", df_scholar)
  ResultCheckServer("show_report", df_scholar)
  
  createPubmedServer("show_report", df_pubmed)
  ResultCheckServer("show_report", df_pubmed)
  
  # df_pubmetric <- reactiveVal(empty_pubmetric())
  # merge results when they become available
  observeEvent({purrr::map_lgl(df_ls, ~ successfully_retrieved(.x()))},{
    if (any(purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons), ~successfully_retrieved(.x())))){
      print("do merge")
      future(seed=NULL,{
        con <- DBI::dbConnect(odbc::odbc(), "PostgreSQL")
        create_combined_data(isolate(df_orcid()),isolate(df_pubmed()),isolate(df_zora()),isolate(df_publons()),con) %>%
          dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))}) %...>%
        tbl_merge()
    }
  })
  # check if all except scholar are merged, then give signal to merge scholar as well
  observeEvent({tbl_merge()},{
    req(tbl_merge())
    d$datainmerge <- tbl_merge() %>% dplyr::select(starts_with("in_")) %>% names() %>% stringr::str_replace("in_","")
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
      d$do_scholar_match <- TRUE
      # if all merged (including scholar, or scholar not present) get citation metrics
    } else if (length(d$datainmerge) == (sum(d$dataininput))){
      future(seed=NULL,{
        retrieve_from_pubmed_with_doi(isolate(tbl_merge())[["doi"]]) %>% 
          dplyr::right_join(isolate(tbl_merge()), by= "doi", suffix = c(".pubmetric","")) 
      }, globals = list(retrieve_from_pubmed_with_doi=retrieve_from_pubmed_with_doi,
                        tbl_merge=tbl_merge)) %...>% 
        tbl_merge()
    }
  })

  # combine scholar with tbl_merge
  observeEvent({successfully_retrieved(df_scholar());d$do_scholar_match},{
    if (d$do_scholar_match && successfully_retrieved(df_scholar())){
      future(seed=NULL,{
        str_length
        tmpscholar <- df_scholar_matching(isolate(tbl_merge()),isolate(df_scholar()))
        dplyr::full_join(isolate(tbl_merge()),tmpscholar,by="doi",suffix=c("",".scholar")) %>% 
          dplyr::mutate(#year = dplyr::if_else(is.na(year) & !is.na(year.scholar), as.integer(year.scholar), as.integer(year)),                        ,
            overall_oa = factor(dplyr::if_else(is.na(overall_oa), "unknown",as.character(overall_oa)),levels = names(open_cols_fn()))) %>% 
          dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x)))
      }) %...>% 
        dplyr::mutate(year = dplyr::if_else(is.na(year) & !is.na(year.scholar), as.integer(year.scholar), as.integer(year))) %...>% 
        tbl_merge()
      d$do_scholar_match <- FALSE
    }
  })
  
  # progress bar
  ProgressbarCreateServer("show_report")
  ProgressbarUpdateServer("show_report", df_ls)
  
  # save 'tbl_merge' in 'm' for downstream analysis
  observeEvent({tbl_merge()},{
    req(tbl_merge())
    d$m <- tbl_merge()
    d$m_sub <- tbl_merge()
    d$m_sub_sel <- tbl_merge()
    # hide upset plot if only one dataset available
    if (length(d$datainmerge) < 2){
      hideTab("author_plots_tables", "Upset Plot")
    } else {
      showTab("author_plots_tables", "Upset Plot")
    }
  })
  
  # activate some stuff and preparation
  observeEvent(d$m,{
    selection_ls$init <- TRUE
    shinyjs::show(id = "in_selection")
    shinyjs::show(id = "oa_status_filtered_table")
    shinyjs::show(id = "range_year")
    shinyjs::show(id = "apply_DT_selection")
    shinyjs::show(id = "reset_DT_selection")
    shinyjs::show(id = "create_bibtex")
    # update and show selections
    # update single selection for plots and tables
    d$all_selection_choices <- colnames(d$m)[grep("in_",colnames(d$m))]
  })
  
  # modules for updating selection UI
  datasetSelectionsServer("selection_standard",d,selection_ls)
  datasetSelectionsUpdateServer("selection_standard",d,selection_ls)
  
  observeEvent({input$oa_status_filtered_table; input$range_year; d$m_sub_all_oa},{
    req(d$m_sub_all_oa)
    m_filt <- d$m_sub_all_oa %>% 
      dplyr::mutate(year=as.integer(year)) %>% 
      dplyr::filter(overall_oa %in% input$oa_status_filtered_table,
                    year >= input$range_year[1],
                    year <= input$range_year[2])
    d$m_sub <- m_filt
    d$m_sub_sel <- m_filt
  })
  
  # update summary when subset changes
  observeEvent({d$m_sub},{
    # summary of subset table
    overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa") %>% as.character()
    # overall_oa_status[is.na(overall_oa_status)] <- "unknown"
    # levels(overall_oa_status) <- names(open_cols_fn())
    output$sub_summary <- renderPrint({
      print(paste("Total:",length(overall_oa_status)))
      table(overall_oa_status,useNA = "ifany")
    })
  })
  
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
        dplyr::select(doi,relative_citation_ratio,nih_percentile,citation_count,in_pubmed )
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

  # # bibtex creation
  # bib_reac <- in_selection_bib_Server("bib",d$m)
  # observe({
  #   d$to_update <- bib_reac()
  #   output$bibtex_summary <- renderPrint({paste("Total number of entries to download:", length(d$to_update))})
  # })
  
  observeEvent(input$create_bibtex,{
    if (!is.null(d$m_sub_sel$doi) && length(d$m_sub_sel$doi) > 0){
      shinyWidgets::ask_confirmation(
        inputId = "create_bibtex_confirmation",
        title = "Confirm Bibtex retrieval",
        text = paste("You are about to retrieve the bibtex entries of ",
                     length(d$m_sub_sel$doi), "publications. This will take approximately",
                     lubridate::seconds(length(d$m_sub_sel$doi)*0.5)," and will run in the background."
        )
      )
    } else {
      show_alert(
        title = "Bibtex retrieval error",
        text = "No valid doi's selected from Table. (The Table is likely empty)",
        type = "error"
      )
    }

  })
  
  bibtex_ls <- reactiveVal()
  observeEvent(input$create_bibtex_confirmation,{
    if(input$create_bibtex_confirmation){
      shinyjs::disable("create_bibtex")
      to_update <- d$m_sub_sel$doi
      if(length(to_update) > 0) {
        GetBibEntryWithDOI_no_temp(to_update) %...>% 
        bibtex_ls()
      }
    }
  })
   observeEvent(bibtex_ls(),{
     print(bibtex_ls())
     req(bibtex_ls())
     shinyjs::enable("create_bibtex")
     output$bibsummary <-  renderText(bibtex_ls())
     shinyjs::show("bibtex_download")
     updateTabsetPanel(session, "author_plots_tables", selected = "Bibtex")
   })
  
  output$bibtex_download <- downloadHandler(
    filename = function() {paste0("BIBTEX_FOR_",d$author_vec[1], ".bib")},
    content = function(file){
        writeLines( paste(bibtex_ls(),collapse = "\n"),file)
    }
  )
      
  
  output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
          # d$pri_author <- input$author_search[1]
          # d$sec_author <- input$author_search[2]
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(pri_author = d$pri_author,
                         sec_author = d$sec_author,
                         orcid = d$orcid,
                         cutoff_year = input$range_year,
                         tbl_subjects=tbl_subjects,
                         tbl_authorkeys=tbl_authorkeys,
                         tbl_eprints=tbl_eprints,
                         unpaywall=unpaywall,
                         zora=d$zora,
                         m=d$m)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
      }
  )
  
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

# Run the application 
shinyApp(ui = ui, server = server)
