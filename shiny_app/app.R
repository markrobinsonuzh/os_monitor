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
on_rstudio <- FALSE
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
                               actionButton("create_bibtex","Get Bibtex citation")
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
                    tabPanel("Closed in Zora", DT::dataTableOutput("table_closed_in_zora")),
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
  observe({
    orcid_auth_tmp <- orcid_auth_react()
    d$orcid <- orcid_auth_tmp[["orcid"]]
    d$author_vec <- orcid_auth_tmp[["author_vec"]]
    d$is_valid_zora <- TRUE
  })

  # automated string operations, enable, disable report buttons
  observeEvent(d$author_vec,{
    updateTextInput(session,NS("input_check", "orcid"),value=d$orcid)
    if (is.null(d$author_vec)){
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
  pubmedActivateServer("input_check", d, con)
  observeEvent({d$pubmed},{
    updateTextAreaInput(session,NS("input_check", "pubmed"),value=d$pubmed)
  })

  # check for valid inputs
  orcidCheckServer("input_check", d)
  pubmedCheckServer("input_check", d)
  publonsCheckServer("input_check", d)
  scholarCheckServer("input_check", d)
  
  # wait for clicking of "show_report", then retrieve all data asynchronously 
  scholarModalServer("show_report",d)
  tbl_merge <- reactiveVal(NULL)
  df_zora <- reactiveVal(empty_zora())
  createZoraServer("show_report", d, df_zora)
  df_orcid <- reactiveVal(empty_orcid())
  createOrcidServer("show_report", d, df_orcid)
  df_publons <- reactiveVal(empty_publons())
  createPublonsServer("show_report", d, df_publons)
  df_scholar <- reactiveVal(empty_scholar())
  createScholarServer("show_report", d, df_scholar)
  df_pubmed <- reactiveVal(empty_pubmed())
  createPubmedServer("show_report", d, df_pubmed)
  # merge results when they become available
  observeEvent({df_zora();df_orcid();df_pubmed();df_publons()},{
    if (any(purrr::map_lgl(c(df_zora,df_orcid,df_pubmed,df_publons), ~ dim(req(.x()))[1] != 0))){
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
    all_poss_datasets <- c("author_vec","orcid","pubmed","publons","scholar")
    datainmerge <- tbl_merge() %>% dplyr::select(starts_with("in_")) %>% names() %>% stringr::str_replace("in_","")
    datainmerge[datainmerge=="zora"] <- "author_vec"
    dataininput <- c(d$is_valid_zora, d$is_valid_orcid, d$is_valid_pubmed, d$is_valid_publons, d$is_valid_scholar)
    if ((length(datainmerge) == (sum(dataininput)-1)) && ("scholar" %in% all_poss_datasets[dataininput])){
      d$do_scholar_match <- TRUE
      print("signal scholar match")
    }
  })

  # combine scholar with tbl_merge
  observeEvent({df_scholar();d$do_scholar_match},{
    print("check scholar")
    if (d$do_scholar_match && (dim(df_scholar())[1] != 0)){
      print("merge scholar")
      future(seed=NULL,{
        str_length
        tmpscholar <- df_scholar_matching(isolate(tbl_merge()),isolate(df_scholar()))
        dplyr::full_join(isolate(tbl_merge()),tmpscholar,by="doi",suffix=c("",".scholar"))
      }) %...>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("in_"),~ifelse(is.na(.x),FALSE,.x))) %...>%
        tbl_merge()
      d$do_scholar_match <- FALSE
    }
  })
  
  # deactivate 'show_report' button while processing
  ActivateShowReportServer("show_report", d, df_zora, df_orcid, df_pubmed, df_publons, df_scholar)
  DeactivateShowReportServer("show_report")
  
  # progress bar
  ProgressbarCreateServer("show_report")
  ProgressbarUpdateServer("show_report", d, df_zora, df_orcid, df_pubmed, df_publons, df_scholar)
  
  # save 'tbl_merge' in 'm' for downstream analysis
  observe({
    req(tbl_merge())
    d$m <- tbl_merge()
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
    overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa")
    levels(overall_oa_status) <- c(levels(overall_oa_status),"unknown")
    overall_oa_status[is.na(overall_oa_status)] <- "unknown"
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
  # output$plot_selected <- renderPlot({
  output$plot_selected <- renderPlotly({
      req(d$m_sub)
    p_t$selected_plot()
  })
  observeEvent(event_data("plotly_click"),{
    req(d$m_sub)
    print(event_data("plotly_click"))
    # only use existing oa status
    oa_order <- names(open_cols_fn()[names(open_cols_fn()) %in% unique(d$m_sub$overall_oa)])
    modulus_click <- event_data("plotly_click")$curveNumber %% length(oa_order)
    d$plot_selected_ly_clicked <- list(year=event_data("plotly_click")$x,
                                       oa_status=oa_order[modulus_click+1])
  })

  # popup table
  p_t$modal_selected_table <- reactive({req(d$plot_selected_ly_clicked)
    tryCatch({overall_closed_table(
      dplyr::filter(d$m_sub,
                    year==d$plot_selected_ly_clicked[["year"]],
                    overall_oa==d$plot_selected_ly_clicked[["oa_status"]]))
      },error=function(e) DT::datatable(head(d$m,0)))})
  observeEvent(event_data("plotly_click"),{
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
  p_t$selected_closed_table <- reactive({tryCatch({overall_closed_table(d$m_sub_sel)},
                                                 error=function(e) DT::datatable(head(d$m,0)))})
  # observe({data_table_selection_processing_Server("DT_author_selection",d)})
  # p_t$selected_closed_table <- data_table_selection_table_Server("DT_author_selection",d$m, d$m_sub_sel)
  output$table_selected_closed <- DT::renderDataTable({
    req(d$m_sub_sel)
    p_t$selected_closed_table()
  })


  ### Zora closed table  -------------------------------------------------------
  p_t$closed_in_zora_table <- reactive({closed_in_zora_table(d$zora)})
  output$table_closed_in_zora <- DT::renderDataTable({
    req(d$zora)
      p_t$closed_in_zora_table()
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
    shinyWidgets::ask_confirmation(
      inputId = "create_bibtex_confirmation",
      title = "Confirm Bibtex retrieval",
      text = paste("You are about to retrieve the bibtex entries of ",
                   length(d$m_sub_sel$doi), "publications. This will take approximately",
                   lubridate::seconds(length(d$m_sub_sel$doi)*5)," and will run in the background."
                   )
    )
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
     req(bibtex_ls())
     shinyjs::enable("create_bibtex")
     output$bibsummary <-  renderText(bibtex_ls())
     shinyjs::show("bibtex_download")
     updateTabsetPanel(session, "author_plots_tables", selected = "Bibtex")
   })
  
  output$bibtex_download <- downloadHandler(
    filename = paste0("BIBTEX_FOR_ORCID_",d$orcid, ".bib"),
    content = function(file){
        writeLines( paste(value(bibtex_ls()),collapse = "\n"),file)
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
