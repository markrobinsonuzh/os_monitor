# load required packages
suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    # library(rcrossref)
    library(ggplot2)
    library(stringr)
    library(shinyjs)
    # library(mongolite)
  # library(shinyBS)
  library(plotly)
  library(DBI)
  library(shinyTree)
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
          sidebarPanel(width=6,
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
            textInput("orcid",label = a("Orcid",href="https://orcid.org",target="_blank"), value=""),
            # Pubmed query input
            splitLayout(cellWidths = c("75%", "25%"),
                        textAreaInput("pubmed",label = a("Pubmed Query",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"), value="") %>% 
                          shinyjs::disabled(),
                        actionButton("activate_pubmed",HTML("Generate <br/> Pubmed <br/> Query")) %>% 
                          shinyjs::disabled()),
            # google scholar input
            textInput("scholar",label = a("Google Scholar id",href="https://scholar.google.ch",target="_blank"), value="") %>% 
              shinyhelper::helper(type="markdown",
                                  title = "Google scholar id help",
                                  content = 'Google_scholar_help'),
            # publons input
            textInput("publons",label = tags$div(tags$a("Publons id",href="https://publons.com",target="_blank"),
                                                 tags$span(class="help-block","(or if linked: ORCID, ResearcherID or TRUID)")), value=""),
            # aggregate data
            disabled(actionButton(inputId = "show_report",label = "Show report"))
          ),
          # disabled(downloadButton("report", "Generate report"))
      mainPanel(
        # panel for filtering
        wellPanel(
          splitLayout(cellWidths = c("25%", "75%"),
            checkboxGroupInput(inputId = "in_selection",label = "Data sets included","",inline = TRUE) %>% 
              shinyjs::hidden(),
            verbatimTextOutput("sub_summary")
          ),
          sliderInput("range_year",label = "Cutoff year",min=2001,max = 2020,value=c(2001,2020))%>% 
            shinyjs::hidden(),
          checkboxGroupInput("oa_status_filtered_table","OA status",
                             choices = names(open_cols_fn()),
                             selected = names(open_cols_fn()),
                             inline = TRUE) %>% 
            shinyjs::hidden(),
        ),
        # output panel (tables, plots etc.)
        tabsetPanel(type = "tabs",
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
                               downloadButton("bibtex", "Bibtex citation") %>% 
                                 shinyjs::hidden()
                               ) %>% 
                                 shinyhelper::helper(type="markdown",
                                                   title = "Apply and Reset selection help",
                                                   content = 'Apply_and_Reset_selection'),
                             DT::dataTableOutput("table_selected_closed")),
                    tabPanel("Closed in Zora", DT::dataTableOutput("table_closed_in_zora")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time"))
        )
    )
      )
      ),
      tabPanel("Department OA explorer",
               fluidPage(
                 sidebarLayout(
                   sidebarPanel(
                     shinyTree("tree", checkbox = TRUE, search=TRUE, theme="proton", themeIcons = FALSE, themeDots = FALSE),
                     # selectInput("fac_choice","Faculty",
                     #             choices = c("all",sort(unique_fac_dep(fac_dep_filt,"fac")))),
                     # selectInput("dep_choice","Department",choices = NULL),
                     checkboxGroupInput("oa_status_filtered","OA status",
                                        choices = unique(fac_dep_filt$oa_status),
                                        selected = unique(fac_dep_filt$oa_status),
                                        inline = TRUE),
                     selectInput("oa_status_filtered_sorting","Sort by",
                                        choices = unique(fac_dep_filt$oa_status),
                                        selected = "closed"),
                     checkboxGroupInput("publication_type_filtered",
                                        "Publication types included",
                                        choices=unique(fac_dep_filt$type),
                                        selected = "article"),
                     actionButton("treeapply",label = "Apply selection")
                   ),
                   mainPanel(
                     # plotOutput("plot_dep_fac",height = "800px",width = "100%")
                     plotlyOutput("plot_dep_fac_anim_year",height = "800px",width = "100%"),
                     plotlyOutput("plot_dep_fac_dep_year",height = "800px",width = "100%"),
                     plotlyOutput("plot_dep_fac_year_val_line",height = "800px",width = "100%"),
                     plotlyOutput("plot_dep_fac_year_val_bar",height = "800px",width = "100%")
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

  shinyhelper::observe_helpers(session = session)
  # data
  d <- reactiveValues(pubmed="",orcid="",publons="",scholar="")
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
  })

  # automated string operations, enable, disable report buttons
  observeEvent(d$author_vec,{
    updateTextInput(session,"orcid",value=d$orcid)
    if (is.null(d$author_vec)){
        disable("show_report")
        disable("report")
        disable("activate_pubmed")
    } else{
        enable("show_report")
        enable("report")
        enable("activate_pubmed")
    }
  })
  observeEvent(input$activate_pubmed,{
    showModal(modalDialog("This is an automatically generated query and is 
                          unlikely to find all correct entries. For more details see:",
                          a("NCBI pubmedhelp",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"),
                          title = "Pubmed query info", size="s",easyClose = TRUE))
    
    d$pubmed <- tryCatch({
      pubmed_search_string_from_zora_id(d$author_vec[1],
                                        con, 
                                        cutoff_year= c(2000),
                                        orcid = unlist(ifelse(is.null(d$orcid),list(NULL),d$orcid)))
    },error=function(e)"")
    updateTextAreaInput(session,"pubmed",value=d$pubmed)
    enable("pubmed")
  })
  # for manual input from user
  observeEvent(input$orcid,{
      d$orcid <- input$orcid
  })
  observeEvent(input$pubmed,{
    d$pubmed <- input$pubmed
  })
  observeEvent(input$scholar,{
    d$scholar <- input$scholar
  })
  observeEvent(input$publons,{
    d$publons <- input$publons
  })
  
  # create combined table from given user inputs
  show_report_reac <- ShowReportServer("show_report", d, con)
  observeEvent(input$show_report,{
    d <- show_report_reac()
    shinyjs::show(id = "in_selection")
    shinyjs::show(id = "oa_status_filtered_table")
    shinyjs::show(id = "range_year")
    shinyjs::show(id = "apply_DT_selection")
    shinyjs::show(id = "reset_DT_selection")
    shinyjs::show(id = "bibtex")
    # update and show selections
    # update single selection for plots and tables
    updateCheckboxGroupInput(session,"in_selection",
                             choices =  c(colnames(d$m)[grep("in_",colnames(d$m))],"inverse"),
                             selected = c(colnames(d$m)[grep("in_",colnames(d$m))]))

  })
  
  # create subset of combined table based on selection of tables
  observe({
    req(input$in_selection)
    d$in_selection <- gsub("in_","",input$in_selection[input$in_selection != "inverse"]) %>% stringr::str_to_title()
    in_selection_quo <- quos(input$in_selection[input$in_selection != "inverse"])
    if (length(input$in_selection[input$in_selection != "inverse"]) != 0){
      m_filt <- d$m %>%
        dplyr::filter((year >= input$range_year[1]) & (year <= input$range_year[2])) %>%
        dplyr::filter(overall_oa %in% input$oa_status_filtered_table)
      ind <- m_filt %>%
        dplyr::select(!!!in_selection_quo) %>%
        purrr::reduce(.f=function(x,y){x|y},.init = FALSE)
      if ("inverse" %in% input$in_selection){
        ind <- !ind
      }
      d$m_sub <- m_filt[ind,]
      d$m_sub_sel <- m_filt[ind,]
    }
  
    # summary of subset table
    overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa")
    levels(overall_oa_status) <- c(levels(overall_oa_status),"unknown")
    overall_oa_status[is.na(overall_oa_status)] <- "unknown"
    output$sub_summary <- renderPrint({
      print(paste("Total:",length(overall_oa_status)))
      table(overall_oa_status,useNA = "ifany")
      })
  })
  
  ### oa status upset plot -----------------------------------------------------
  p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m_sub)},error=function(e) {print(e);ggplot() + geom_blank()})})
  output$plot_upset <- renderPlot({
    req(d$m_sub)
    p_t$upset_plot()
  },res=100)
  

  
  
  ### selected plot  -----------------------------------------------------------
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
  
  output$bibtex <- downloadHandler(
    filename = paste0("BIBTEX_FOR_ORCID_",d$orcid, ".bib"),
    content = function(file){
      to_update <- d$m_sub_sel$doi
      if(length(to_update) > 0) {
        bibtex_from_doi <- GetBibEntryWithDOI_no_temp(to_update)
        writeLines( paste(bibtex_from_doi,collapse = "\n"),file)
        # df_pubmed[df_pubmed$doi %in% to_update,] %>% select(-authors,-pmid)
        # print("start")
        # write missing entries to bibtex file
        # bibtex_from_doi <- GetBibEntryWithDOI(d$to_update)
        # print("all found")
        # toBiblatex(bibtex_from_doi)
        # writeLines(toBiblatex(bibtex_from_doi),file)
        # print("all written")
      }
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
  
  # observeEvent(input$fac_choice,{
  #   if(input$fac_choice != "all"){
  #     dep_tmp <- unique_fac_dep(fac_dep_filt,"fac_dep") %>% 
  #       dplyr::filter(fac == input$fac_choice) %>% 
  #       dplyr::arrange(dep) %>% 
  #       dplyr::pull(dep)
  #     updateSelectInput(session,inputId = "dep_choice",choices = dep_tmp)
  #   }
  # })
  d_dep <- reactiveValues()
  
  # get_json <- reactive({
  #   future({
  #     treeToJSON(orgtree, pretty = TRUE)
  #   })
  # })
  
  # output$tree <- renderTree({
  #   get_json()
  # })
  # tree_future <- reactive({
  #   future({
  #     dt
  #   })
  # })
  output$tree <- renderTree({
    # req(get_json())
    # get_json()
    orgtree
  })
  
  # output$plot_dep_fac <- renderPlot({
  #   req(input$tree)
  #   chosen_orgs_bool <- lapply(input$tree$Get("state"), function(i) i[3][[1]]) %>% unlist()
  #   chosen_orgs <- names(chosen_orgs_bool)[chosen_orgs_bool][-1]
  #   if (length(input$oa_status_filtered) == 0 || length(input$publication_type_filtered) == 0){
  #     ggplot() + geom_blank()
  #   } else {
  #     plot_fac_dep(fac_dep_filt, fac_chosen = chosen_orgs, oa_status_filter = input$oa_status_filtered, 
  #                  arrange_by = input$oa_status_filtered_sorting, publication_filter = input$publication_type_filtered)
  #   }
  # },res=100)
  
  observeEvent(input$treeapply,{
    chosen_orgs_bool <- lapply(input$tree$Get("state"), function(i) i[3][[1]]) %>% unlist()#, file="/dev/null")
    d_dep$chosen_orgs <- names(chosen_orgs_bool)[chosen_orgs_bool][-1]
    d_dep$oa_status_filtered <- input$oa_status_filtered
    d_dep$publication_type_filtered <- input$publication_type_filtered
    d_dep$oa_status_filtered_sorting <-  input$oa_status_filtered_sorting
  })
  
  wide_hk <- reactive({
    req(d_dep$chosen_orgs)
    preprocess_fac_dep(fac_dep_filt,
                              col_to_plot = "dep", 
                              fac_chosen =  d_dep$chosen_orgs, 
                              oa_status_filter = d_dep$oa_status_filtered, 
                              publication_filter = d_dep$publication_type_filtered,
                              by_year = TRUE) %>% 
      arrange_fac_dep(arrange_by = d_dep$oa_status_filtered_sorting, type_arr="Count",by_year = TRUE) %>% 
      tidyr::pivot_wider(names_from=type,values_from=value) %>% 
      highlight_key(~dep)
  })
  
  # output$plot_dep_fac <- renderPlotly({
  #   dep_plot_reac()
  # })
  # observe({
  #   output$plot_dep_fac <- dep_plot_reac()
  # })
  output$plot_dep_fac_anim_year <- renderPlotly({
      plot_fac_dep(wide_hk(),fac_dep_filt,plot_type = "anim_year")
  })
  output$plot_dep_fac_dep_year <- renderPlotly({
    plot_fac_dep(wide_hk(),fac_dep_filt,plot_type = "dep_year")
  })
  output$plot_dep_fac_year_val_line <- renderPlotly({
    plot_fac_dep(wide_hk(),fac_dep_filt,plot_type = "year_val_line")
  })
  output$plot_dep_fac_year_val_bar <- renderPlotly({
    plot_fac_dep(wide_hk(),fac_dep_filt,plot_type = "year_val_bar")
  })
  
  
  
  # output$plot_dep_fac <- renderTags({
  #   dep_plot_reac()
  # })
  # output$plot_dep_fac <- renderPlotly({
  #   chosen_orgs <- tmp <- fac_dep_filt %>% group_by(dep,oa_status) %>% tally() %>% group_by(dep) %>% 
  #     tally() %>% filter(n==5) %>% pull(dep) %>%  sample(1)
  #   fac_filt_long <- preprocess_fac_dep(fac_dep_filt,
  #                             col_to_plot = "dep", 
  #                             fac_chosen = chosen_orgs,
  #                             by_year = TRUE) %>% 
  #     dplyr::filter(type=="Count") 
  #   plot_fac_dep(fac_filt_long, use_plotly = TRUE)
  # })
  # observeEvent({input$oa_status_filtered;input$oa_status_filtered_sorting;input$publication_type_filtered;input$tree},{
  #     chosen_orgs_bool <- lapply(input$tree$Get("state"), function(i) i[3][[1]]) %>% unlist()
  #     chosen_orgs <- names(chosen_orgs_bool)[chosen_orgs_bool][-1]
  #     tmp <- preprocess_fac_dep(fac_dep_filt,
  #                               col_to_plot = "dep", 
  #                               fac_chosen = chosen_orgs, 
  #                               oa_status_filter = input$oa_status_filtered, 
  #                               publication_filter = input$publication_type_filtered,
  #                               by_year = TRUE) %>% 
  #       dplyr::filter(type=="Count") %>% 
  #       arrange_fac_dep(arrange_by = input$oa_status_filtered_sorting, type_arr="Count",by_year = TRUE)
  #       # dplyr::mutate(dep=as.character(dep))
  #     print(tmp)
  #     tmpx <- lapply(all_oa_status,
  #                  function(oas) {
  #                    lapply(tmp$dep %>% unique, 
  #                           function(dep) {
  #                             tmpval <- tmp$value[tmp$dep==dep & tmp$oa_status==oas]
  #                             if(length(tmpval)==0 || !(tmp$oa_status %in% input$oa_status_filtered)){
  #                               tmpval <- 0
  #                             }
  #                             tmpval
  #                           })
  #                  }
  #   )
  #   tmpy <- lapply(all_oa_status,
  #                  function(oas) as.list(unique(tmp$dep))
  #   )
  #   if(dim(tmp)[1] != 0){
  #     plotlyProxy("plot_dep_fac", session) %>%
  #       plotlyProxyInvoke("restyle", 
  #                         list(
  #                           y=tmpy,
  #                           x=tmpx
  #                         )
  #       )
  #   }
  # 
  # })

}

# Run the application 
shinyApp(ui = ui, server = server)
