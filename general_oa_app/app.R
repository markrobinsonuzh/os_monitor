# load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(purrr)
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
"0000-0002-3048-5518"
"XPfrRQEAAAAJ"
on_rstudio <- TRUE
if(on_rstudio){
  setwd("/srv/shiny-server/os_monitor/shiny_app")
  maindir <- file.path(getwd(),"..")
} else {
  setwd("/srv/shiny-server/")
  maindir <- getwd()
}
# functions for backend
# devtools::install("/srv/shiny-server/os_monitor/uzhOS",build = TRUE)
# library(uzhOS)
devtools::load_all(file.path(maindir,"uzhOS"))
outdir <- file.path(maindir,"output")
datadir <- file.path(maindir,"data")
# token to get acces to orcid (currently Reto's token)
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")
use_sql <- TRUE
if(use_sql){
  con <- dbConnect(odbc::odbc(), "PostgreSQL")
} 
options(shinyTree.defaultParser="tree")

### UI #########################################################################
ui <- navbarPage("Open science monitor",
                 tabPanel("Author OA explorer",
                          fluidPage(
                            useShinyjs(),
                            shinyFeedback::useShinyFeedback(),
                            sidebarPanel(width=6,
                                         # Orcid input
                                         textInput("orcid",label = a("Orcid",href="https://orcid.org",target="_blank"), value="0000-0002-3048-5518"),
                                         # Pubmed query input
                                         textAreaInput("pubmed",label = a("Pubmed Query",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"), value=""),
                                         # google scholar input
                                         textInput("scholar",label = a("Google Scholar id",href="https://scholar.google.ch",target="_blank"), value="") %>% 
                                           shinyhelper::helper(type="markdown",
                                                               title = "Google scholar id help",
                                                               content = 'Google_scholar_help'),
                                         # publons input
                                         textInput("publons",label = tags$div(tags$a("ResearchID (Publons)",href="https://publons.com",target="_blank"),
                                                                              tags$span(class="help-block","(or if linked: ORCID)")), value=""),
                                         # aggregate data
                                         disabled(actionButton(inputId = "show_report",label = "Show report"))
                            ),
                            # disabled(downloadButton("report", "Generate report"))
                            conditionalPanel("input.show_report > 0",
                            mainPanel(
                              # panel for filtering
                              wellPanel(
                                uiOutput("selection_rules"),
                                # splitLayout(cellWidths = c("25%", "75%"),
                                            # checkboxGroupInput(inputId = "in_selection",label = "Data sets included","",inline = TRUE) %>% 
                                              # shinyjs::hidden(),
                                            verbatimTextOutput("sub_summary"),
                                # ),
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
                                          tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time"))
                              )
                            )
                          )
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
  # input values for selection
  selection_ls <- reactiveValues(init=TRUE)
  
  # for manual input from user
  observeEvent(input$orcid,{
    if(input$orcid != ""){
      shinyFeedback::feedbackWarning(
        "orcid", 
        tryCatch(rorcid::as.orcid(x = input$orcid),error=function(e) "") == "",
        "Please select a valid Orcid!"
      ) 
      d$orcid <- input$orcid
    }
  })
  observeEvent(input$pubmed,{
    d$pubmed <- input$pubmed
  })
  observeEvent(input$scholar,{
    if(input$scholar != ""){
      shinyFeedback::feedbackWarning(
        "scholar", 
        tryCatch(scholar::get_profile(input$scholar),error=function(e) "") == "",
        "Please select a valid Google scholar id!"
      )
      d$scholar <- input$scholar
    }
  })
  observeEvent(input$publons,{
    if(input$publons != ""){
      shinyFeedback::feedbackWarning(
        "publons", 
        !in_publons(input$publons),
        "Please select a valid ResearcherID!"
      )
      d$publons <- input$publons
    }
  })
  
  observe({
    d$orcid <- input$orcid
    if (d$orcid=="" && d$pubmed=="" && d$scholar=="" && d$publons==""){
      disable("show_report")
    } else{
      enable("show_report")
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

  
  # create combined table from given user inputs
  show_report_reac <- ShowReportServer("show_report", d, con)
  observeEvent(input$show_report,{
    selection_ls$init <- TRUE
    d <- show_report_reac()
    shinyjs::show(id = "in_selection")
    shinyjs::show(id = "oa_status_filtered_table")
    shinyjs::show(id = "range_year")
    shinyjs::show(id = "apply_DT_selection")
    shinyjs::show(id = "reset_DT_selection")
    shinyjs::show(id = "bibtex")
    
    d$all_selection_choices <- colnames(d$m)[grep("in_",colnames(d$m))]
    # d$subset_selection_choices <- 
    # output$selection_rules <- renderUI({
    #   splitLayout(cellWidths = 100,
    #   selectizeInput(inputId = "blab1", label = NULL, choices = c("In","Not In"), selected = "In"),
    #   selectizeInput(inputId = "blab2", label = NULL, choices = d$all_selection_choices, selected = d$all_selection_choices[1]),
    #   selectizeInput(inputId = "blab3", label = NULL, choices = c("And","Or"), options = list(maxItems=1),selected = NULL, multiple = TRUE)
    #   )
    # })
    
    # update and show selections
    # update single selection for plots and tables
    # updateCheckboxGroupInput(session,"in_selection",
    #                          choices =  c(colnames(d$m)[grep("in_",colnames(d$m))],"inverse"),
    #                          selected = c(colnames(d$m)[grep("in_",colnames(d$m))]))
    
  })
  
  observeEvent({
    map(seq_len(length(d$all_selection_choices)*3), function(i){input[[paste0("blab",i)]]})
    },{
    req(input$show_report)
    req(d$all_selection_choices)
    if(selection_ls$init){
      for(i in seq_len(length(d$all_selection_choices)*3-1)){
        if(i%%3 == 1){
          selection_ls[[paste0("blab",i)]] <- "In"
        } else if(i%%3 == 2){
          selection_ls[[paste0("blab",i)]] <- stringr::str_replace_all(d$all_selection_choices[(i+1)/3],"in_","")
          
        } else {
          selection_ls[[paste0("blab",i)]] <- "Or"
        }
      }
      selection_ls$init <- FALSE
    } else{
      for(i in seq_len(length(d$all_selection_choices)*3-1)){
        selection_ls[[paste0("blab",i)]] <- input[[paste0("blab",i)]]
      }
    }
    selection_list <- map(seq_len(length(d$all_selection_choices)*3-1), ~ selection_ls[[paste0("blab",.x)]])
    nr_not_null <- map(selection_list, ~ !is.null(.x)) %>% reduce(sum)
    if(nr_not_null < 2){
      total_not_null_needed <- 2
    }else if(nr_not_null%%3 == 0){
      total_not_null_needed <- nr_not_null + 2
    } else if (nr_not_null%%3 == 1){
      total_not_null_needed <- nr_not_null + 1
    } else if (nr_not_null%%3 == 2 && !((nr_not_null+1)/3 == length(d$all_selection_choices))){
      total_not_null_needed <- nr_not_null + 1
    } else {
      total_not_null_needed <- nr_not_null
    }
    filter_all_selection_choices <- function(i, all_selection_choices){
      id_to_remove <- seq(2,i,by=3)
      id_to_remove <- id_to_remove[-length(id_to_remove)]
      ind_to_remove <- map(id_to_remove, ~ which(paste0("in_",selection_ls[[paste0("blab",.x)]]) == all_selection_choices)) %>% unlist()
      if(length(ind_to_remove) > 0){
        ret_sele <- all_selection_choices[-ind_to_remove]
      } else {
        ret_sele <- all_selection_choices
      }
      return(ret_sele %>% stringr::str_replace_all("in_","") %>% unlist())
    }
    
    list_all_choices <- map(seq_len(ceiling(total_not_null_needed/3)), function(i){filter_all_selection_choices((i*3)-1,d$all_selection_choices)})
    output$selection_rules <- renderUI({
      # splitLayout(cellWidths = rep(100,total_not_null_needed),
                  map(seq_len(total_not_null_needed), function(i){
                    if(i %% 3 == 1){
                      div(style="display: inline-block;vertical-align:top; width: 100px;",
                          selectizeInput(inputId = paste0("blab",i), label = NULL, 
                                         choices = c("In","Not In"), options = list(maxItems=1),
                                         selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
                    } else if(i %% 3 == 2){
                      div(style="display: inline-block;vertical-align:top; width: 150px;",
                          selectizeInput(inputId = paste0("blab",i), label = NULL, 
                                         choices = list_all_choices[[(i+1)/3]], options = list(maxItems=1), 
                                         selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
                    } else {
                      div(style="display: vertical-align:top; width: 70px;",
                          selectizeInput(inputId = paste0("blab",i), label = NULL, 
                                         choices = c("And","Or"), options = list(maxItems=1), 
                                         selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
                    }
                  }) 
      # )
    })
    
  })
  
  observeEvent({
    map(seq_len(length(d$all_selection_choices)*3), function(i){input[[paste0("blab",i)]]})
  },{
    req(input$show_report)
    req(d$all_selection_choices)
    tmpfilt <- map(seq_len(length(d$all_selection_choices)*3-1), function(i){
      if(i%%3 == 1){
        ifelse(input[[paste0("blab",i)]] == "Not In" && !is.null(input[[paste0("blab",i)]]), "!", "")
      } else if(i%%3 == 2){
        ifelse(!is.null(input[[paste0("blab",i)]]),paste0("in_",input[[paste0("blab",i)]]),"")
        
      } else {
        ifelse(input[[paste0("blab",i)]] == "And" && !is.null(input[[paste0("blab",i)]]), "&", "|")
      }
    }) 
    print(tmpfilt)
    tmpfilt <- tmpfilt %>% unlist() 
    for (i in seq(2,length(tmpfilt),by=3)){
      if(tmpfilt[i] == ""){
        tmpfilt[(i-1):(i+1)] <- ""
      }
    }
    tmpfilt <- tmpfilt %>% paste(collapse = "")
    if(stringr::str_detect(tmpfilt,"\\|$|&$")){
      tmpfilt <- stringr::str_replace(tmpfilt,"\\|$|&$","")
    }
    print(tmpfilt)
    
    
    if(!(tmpfilt=="")){
      m_filt <- subset(d$m,eval(parse(text=tmpfilt)))
      print(m_filt)
      d$m_sub <- m_filt
      d$m_sub_sel <- m_filt
      
      # summary of subset table
      overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa")
      levels(overall_oa_status) <- c(levels(overall_oa_status),"unknown")
      overall_oa_status[is.na(overall_oa_status)] <- "unknown"
      output$sub_summary <- renderPrint({
        print(paste("Total:",length(overall_oa_status)))
        table(overall_oa_status,useNA = "ifany")
      })
    }


    
    
  })
  
  # # create subset of combined table based on selection of tables
  # observe({
  #   req(input$in_selection)
  #   d$in_selection <- gsub("in_","",input$in_selection[input$in_selection != "inverse"]) %>% stringr::str_to_title()
  #   in_selection_quo <- quos(input$in_selection[input$in_selection != "inverse"])
  #   if (length(input$in_selection[input$in_selection != "inverse"]) != 0){
  #     m_filt <- d$m %>%
  #       dplyr::filter((year >= input$range_year[1]) & (year <= input$range_year[2])) %>%
  #       dplyr::filter(overall_oa %in% input$oa_status_filtered_table)
  #     ind <- m_filt %>%
  #       dplyr::select(!!!in_selection_quo) %>%
  #       purrr::reduce(.f=function(x,y){x|y},.init = FALSE)
  #     if ("inverse" %in% input$in_selection){
  #       ind <- !ind
  #     }
  #     d$m_sub <- m_filt[ind,]
  #     d$m_sub_sel <- m_filt[ind,]
  #   }
  #   
  #   # summary of subset table
  #   overall_oa_status <- dplyr::pull(d$m_sub,"overall_oa")
  #   levels(overall_oa_status) <- c(levels(overall_oa_status),"unknown")
  #   overall_oa_status[is.na(overall_oa_status)] <- "unknown"
  #   output$sub_summary <- renderPrint({
  #     print(paste("Total:",length(overall_oa_status)))
  #     table(overall_oa_status,useNA = "ifany")
  #   })
  # })
  
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
                    overall_oa==d$plot_selected_ly_clicked[["oa_status"]]),
      !(is.null(d$zora) || dim(d$zora)[1]==0))
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
  p_t$selected_closed_table <- reactive({tryCatch({overall_closed_table(d$m_sub_sel,!(is.null(d$zora) || dim(d$zora)[1]==0))},
                                                  error=function(e) DT::datatable(head(d$m,0)))})
  # observe({data_table_selection_processing_Server("DT_author_selection",d)})
  # p_t$selected_closed_table <- data_table_selection_table_Server("DT_author_selection",d$m, d$m_sub_sel)
  output$table_selected_closed <- DT::renderDataTable({
    req(d$m_sub_sel)
    p_t$selected_closed_table()
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
  
  get_json <- reactive({
    treeToJSON(orgtree, pretty = TRUE)
  })
  
  output$tree <- renderTree({
    get_json()
  })
  
  output$plot_dep_fac <- renderPlot({
    req(input$tree)
    chosen_orgs_bool <- lapply(input$tree$Get("state"), function(i) i[3][[1]]) %>% unlist()
    chosen_orgs <- names(chosen_orgs_bool)[chosen_orgs_bool][-1]
    if (length(input$oa_status_filtered) == 0 || length(input$publication_type_filtered) == 0){
      ggplot() + geom_blank()
    } else {
      plot_fac_dep(fac_dep_filt, fac_chosen = chosen_orgs, oa_status_filter = input$oa_status_filtered, 
                   arrange_by = input$oa_status_filtered_sorting, publication_filter = input$publication_type_filtered)
    }
  },res=100)
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
