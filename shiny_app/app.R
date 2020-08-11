suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(rcrossref)
    library(ggplot2)
    library(stringr)
    library(shinyjs)
    library(mongolite)
})
tryCatch({setwd("/srv/shiny-server/os_monitor/shiny_app")},
         error=function(e) setwd("~/ownCloud/Projects/open_access/os_monitor/shiny_app/"))
maindir <- getwd()
devtools::load_all(file.path(maindir,"..","uzhOS"))
outdir <- file.path(maindir,"..","output")
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")

unpaywall <- tryCatch({mongo(collection="unpaywall", db="oa", url="mongodb://192.168.16.2/20")},
                      error=function(e) readRDS(file.path(outdir,"dois_unpaywall_subset.rds")))
tbl_eprints <- tryCatch({mongo(collection="eprints", db="oa", url="mongodb://192.168.16.2/20")},
                        error=function(e) readRDS(file.path(outdir, "tbl_eprints.rds")))
tbl_authorkeys <- tryCatch({mongo(collection="authorkeys", db="oa", url="mongodb://192.168.16.2/20")},
                           error=function(e) readRDS(file.path(outdir, "tbl_authorkeys.rds")))
tbl_subjects <- tryCatch({mongo(collection="subjects", db="oa", url="mongodb://192.168.16.2/20")},
                         error=function(e) readRDS(file.path(outdir, "tbl_subjects.rds")))

tbl_unique_authorkeys_fullname <- tryCatch({mongo(collection="unique_authorkeys_fullname", db="oa", url="mongodb://192.168.16.2/20")},
                                  error=function(e) readRDS(file.path(outdir, "tbl_unique_authorkeys_fullname.rds")))

if (is(tbl_unique_authorkeys_fullname,"mongo")){
  unique_authorkeys_processed <- unique(tbl_unique_authorkeys_fullname$find('{}', fields='{"_id":0,"authorkey":0,"id":0,"authorkey_fullname":0}') %>% dplyr::pull(authorname))
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
} else {
  unique_authorkeys_processed <- unique(tbl_unique_authorkeys_fullname$authorkey_processed)
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
}

# summary of faculty and department oa status
fac_dep_filt <- all_org_unit_fac(tbl_eprints)


# Define UI for application that draws a histogram
ui <- navbarPage("Open science monitor UZH",
                 tabPanel("Author OA explorer",
                 fluidPage(
                    useShinyjs(),
                    sidebarLayout(
                        sidebarPanel(
                            selectizeInput("author_search","Author search",NULL,selected = NULL, multiple = FALSE, 
                                           options = list(maxOptions = 50,placeholder="select author",maxItems=10)) %>% 
                              shinyhelper::helper(type="inline",
                                                  title = "Author search input help",
                                                  content = 'This is your full name as saved in ZORA. 
                                                  The format is "(familiy name) (given name)". 
                                                  It is possible that of your given name only the initials are saved in ZORA.
                                                  Multiple selections are possible.'),
                            alias_selected_UI("alias_selected"),
                            textInput("orcid",label = a("Orcid",href="https://orcid.org",target="_blank"), value=""),
                            textInput("pubmed",label = a("Pubmed Query",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"), value=""),
                            textInput("scholar",label = a("Google Scholar id",href="https://scholar.google.ch",target="_blank"), value=""),
                            textInput("publons",label = tags$div(tags$a("Publons id",href="https://publons.com",target="_blank"),
                                                                 tags$span(class="help-block","(or if linked: ORCID, ResearcherID or TRUID)")), value=""),
                            disabled(sliderInput("cutoff_year",label = "Cutoff year",min=2001,max = 2020,value=2001)),
                
                            disabled(actionButton(inputId = "show_report",label = "Show report"))
                            # downloadButton("report", "Generate report")
                            ),
                        disabled(downloadButton("report", "Generate report"))
                        
                        
                    ),
                    mainPanel(
                        wellPanel(
                          titlePanel("Bibtex export"),
                          in_selection_UI("bib"),
                          verbatimTextOutput("bibtex_summary"),
                          disabled(downloadButton("bibtex", "Generate bibtex citation file"))
                          ),
                        # in_selection_UI("plots_in_selection"),
                        splitLayout(cellWidths = c("25%", "75%"),
                          checkboxGroupInput(inputId = "in_selection",label = "Data sets included","",inline = TRUE) %>% 
                            shinyjs::hidden(),
                          verbatimTextOutput("sub_summary")
                        ),
                        
                        # Output: Tabset w/ plot, summary, and table ----
                        tabsetPanel(type = "tabs",
                                    tabPanel("Upset", plotOutput("plot_upset"),height="600px"),
                                    tabPanel("Selected", plotOutput("plot_selected")),
                                    tabPanel("Selected closed", DT::dataTableOutput("table_selected_closed")),
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
                               selectInput("fac_choice","Faculty",
                                           choices = c("all",sort(unique_fac_dep(fac_dep_filt,"fac")))),
                               # selectInput("dep_choice","Department",choices = NULL),
                               checkboxGroupInput("oa_status_filtered","OA status",
                                                  choices = c("closed","hybrid","green","gold","blue"),
                                                  selected = c("closed","hybrid","green","gold","blue"),
                                                  inline = TRUE)
                             ),
                             mainPanel(
                               plotOutput("plot_dep_fac",height = "800px",width = "100%")
                             )
                           )
                         )
                )
)

################################################################################
# Define server logic required to draw a histogram
server = function(input, output,session) {
  
  ###########################################################################
  ### Author
  shinyhelper::observe_helpers(session = session)
    d <- reactiveValues()
    p_t <- reactiveValues()
    
    updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE)
    
    # show available alias if any author_search given, otherwise hide
    observeEvent(input$author_search,{
      alias_selected_show_Server("alias_selected",input$author_search)
    })
    # find and parse aliases of authors
    observeEvent(input$author_search,{
      alias_selected_Server("alias_selected",input$author_search,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints)
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
        d$pubmed <- tryCatch({
          pubmed_search_string_from_zora_id(d$author_vec[1],
                                            tbl_unique_authorkeys_fullname, 
                                            input$cutoff_year, 
                                            orcid = unlist(ifelse(is.null(d$orcid),list(NULL),d$orcid)))
          },error=function(e)"")
        updateTextInput(session,"pubmed",value=d$pubmed)
        if (is.null(d$author_vec)){
            disable("show_report")
            disable("report")
            disable("cutoff_year")
        } else{
            enable("show_report")
            enable("report")
            enable("cutoff_year")
        }
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
    show_report_reac <- ShowReportServer("show_report",d, tbl_authorkeys, tbl_subjects, tbl_eprints, unpaywall)
    observeEvent(input$show_report,{
      d <- show_report_reac()
      enable("bibtex")
      for(tmp_id in c("in_selection")){
        shinyjs::show(id = tmp_id)
      }
      # update and show selections
      in_selection_Server("bib",d$m)
      # in_selection_Server("plots_in_selection",d$m)
      # update single selection for plots and tables
      updateCheckboxGroupInput(session,"in_selection",
                               choices =  c(names(d$m)[grep("in_",names(d$m))],"inverse"),
                               selected = c(names(d$m)[grep("in_",names(d$m))]))
    })
    
    # create subset of combined table based on selection of tables
    observeEvent(input$in_selection,{
      in_selection_quo <- quos(input$in_selection[input$in_selection != "inverse"])
      if (length(input$in_selection[input$in_selection != "inverse"]) != 0){
        ind <- d$m %>%
          dplyr::select(!!!in_selection_quo) %>%
          purrr::reduce(.f=function(x,y){x|y})
        if ("inverse" %in% input$in_selection){
          ind <- !ind
        }
        d$m_sub <- d$m[ind,]
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
    
    ### oa status upset plot
    p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m_sub)},error=function(e) {ggplot() + geom_blank()})})
    output$plot_upset <- renderPlot({
      p_t$upset_plot()
    },res=100)
    ### selected plot
    p_t$selected_plot <- reactive({tryCatch({oa_status_time_plot(d$m_sub,input$cutoff_year,oa_status_used=overall_oa)},
                                            error=function(e) {print(e);ggplot() + geom_blank()})})
    output$plot_selected <- renderPlot({
      p_t$selected_plot()
    })
    ### selected closed table
    p_t$selected_closed_table <- reactive({tryCatch({overall_closed_table(d$m_sub,input$cutoff_year)},
                                                   error=function(e) DT::datatable())})
    output$table_selected_closed <- DT::renderDataTable({
      p_t$selected_closed_table()
    })
    ### Zora closed table
    p_t$closed_in_zora_table <- reactive({closed_in_zora_table(d$zora)})
    output$table_closed_in_zora <- DT::renderDataTable({
        p_t$closed_in_zora_table()
    })
    ### OA percent time table
    p_t$oa_percent_time_table <- reactive({tryCatch({oa_percent_time_table(d$m,input$cutoff_year)},error=function(e) ggplot() + geom_blank())})
    output$table_oa_percent_time <- DT::renderDataTable({
        p_t$oa_percent_time_table()
    })

    # bibtex creation
    bib_reac <- in_selection_bib_Server("bib",d$m)
    observe({
      d$to_update <- bib_reac()
      output$bibtex_summary <- renderPrint({paste("Total number of entries to download:", length(d$to_update))})
    })
    
    output$bibtex <- downloadHandler(
      filename = paste0("BIBTEX_FOR_ORCID_",d$orcid, ".bib"),
      content = function(file){
        if(length(d$to_update) > 0) {
          bibtex_from_doi <- GetBibEntryWithDOI_no_temp(d$to_update)
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
                           cutoff_year = input$cutoff_year,
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
    
    ###########################################################################
    ### Department
    
    # observeEvent(input$fac_choice,{
    #   if(input$fac_choice != "all"){
    #     dep_tmp <- unique_fac_dep(fac_dep_filt,"fac_dep") %>% 
    #       dplyr::filter(fac == input$fac_choice) %>% 
    #       dplyr::arrange(dep) %>% 
    #       dplyr::pull(dep)
    #     updateSelectInput(session,inputId = "dep_choice",choices = dep_tmp)
    #   }
    # })
    
    output$plot_dep_fac <- renderPlot({
      if(input$fac_choice == "all"){
        fac_choice <- NULL
      } else {
        fac_choice <- input$fac_choice
      }
      print(input$oa_status_filtered)
      if (length(input$oa_status_filtered) == 0){
        ggplot() + geom_blank()
      } else {
        plot_fac_dep(fac_dep_filt, fac_chosen = fac_choice, oa_status_filter = input$oa_status_filtered)
      }
    },res=100)
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
