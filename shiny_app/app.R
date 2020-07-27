suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(rcrossref)
    library(roadoi)
    library(ggplot2)
    library(stringr)
    library(shinyjs)
    library(rentrez)
    library(RefManageR)
    library(scholar)
    library(rorcid)
    library(mongolite)
})
tryCatch({setwd("/srv/shiny-server/os_monitor/shiny_app")},
         error=function(e) setwd("~/ownCloud/Projects/open_access/os_monitor/shiny_app/"))
maindir <- getwd()
devtools::load_all(file.path(maindir,"..","uzhOS"))
outdir <- file.path(maindir,"..","output")
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")


cat("load unpaywall data\n")

unpaywall <- tryCatch({mongo(collection="unpaywall", db="oa", url="mongodb://192.168.16.2/20")},
                      error=function(e) readRDS(file.path(outdir,"dois_unpaywall_subset.rds")))
cat("load other data\n")
tbl_eprints <- tryCatch({mongo(collection="eprints", db="oa", url="mongodb://192.168.16.2/20")},
                        error=function(e) readRDS(file.path(outdir, "tbl_eprints.rds")))
tbl_authorkeys <- tryCatch({mongo(collection="authorkeys", db="oa", url="mongodb://192.168.16.2/20")},
                           error=function(e) readRDS(file.path(outdir, "tbl_authorkeys.rds")))
tbl_subjects <- tryCatch({mongo(collection="subjects", db="oa", url="mongodb://192.168.16.2/20")},
                         error=function(e) readRDS(file.path(outdir, "tbl_subjects.rds")))
tbl_unique_authorkeys <- tryCatch({mongo(collection="unique_authorkeys", db="oa", url="mongodb://192.168.16.2/20")},
                                  error=function(e) readRDS(file.path(outdir, "tbl_unique_authorkeys.rds")))
# tbl_unique_authorkeys_processed <- readRDS(file.path(outdir, "tbl_unique_authorkeys_processed.rds"))

if (is(tbl_unique_authorkeys,"mongo")){
  unique_authorkeys_processed <- tbl_unique_authorkeys$find('{}', fields='{"_id":0,"authorkey":0,"id":0,"author_name_family_given":0}') %>% dplyr::pull(authorkey_processed)
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
} else {
  unique_authorkeys_processed <- tbl_unique_authorkeys$authorkey_processed
  names(unique_authorkeys_processed) <- stringr::str_to_title(tbl_unique_authorkeys$authorkey_processed)
}



# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Open science monitor UZH"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # textInput("pri_author",label = "pri_author",value = "robinson m 0000 0002 3048 5518"),
            # textInput("sec_author",label = "sec_author", value = "robinson m d"),
            selectizeInput("author_search","Author search",NULL,selected = NULL, multiple = FALSE, 
                           options = list(maxOptions = 50,placeholder="select author",maxItems=1)),
            verbatimTextOutput("fac_inst"),
            checkboxGroupInput("aliases_selected","",""),
            textInput("orcid",label = "Orcid", value=""),
            textInput("pubmed",label = "Pubmed Query", value=""),
            textInput("scholar",label = "Google Scholar id", value=""),
            textInput("publons",label = "Publons id (or if linked: ORCID, ResearcherID or TRUID)", value=""),
            disabled(sliderInput("cutoff_year",label = "cutoff_year",min=2001,max = 2020,value=2001)),

            disabled(actionButton(inputId = "show_report",label = "Show report"))
            # downloadButton("report", "Generate report")
            ),
        disabled(downloadButton("report", "Generate report"))
        

    ),
    mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Zora", plotOutput("plot_zora_only")),
                    tabPanel("Closed in Zora", DT::dataTableOutput("table_closed_in_zora")),
                    tabPanel("Orcid", plotOutput("plot_orcid_only")),
                    tabPanel("Pubmed", plotOutput("plot_pubmed_only")),
                    tabPanel("Zora & Orcid", plotOutput("plot_zora_orcid")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time")),
                    tabPanel("Overall closed", DT::dataTableOutput("table_overall_closed")),
                    tabPanel("Zora", DT::dataTableOutput("table_zora_without_orcid")),
                    tabPanel("Orcid", DT::dataTableOutput("table_orcid_without_zora")),
                    tabPanel("Upset", plotOutput("plot_upset"))
        )

    )
)

# Define server logic required to draw a histogram
server = function(input, output,session) {
    d <- reactiveValues(pri_author=NULL,sec_author=NULL)
    p_t <- reactiveValues()
    
    updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE,selected = sample(unique_authorkeys_processed,1))
    
    observeEvent(input$author_search,{
        pot_aliases_ls <- pot_alias_and_affil(input$author_search[1],tbl_unique_authorkeys,tbl_subjects,tbl_authorkeys,tbl_eprints)
        pot_aliases_ls_text <- lapply(pot_aliases_ls[[1]],function(alias){
            HTML(paste(alias,"<br>",
                       paste(pot_aliases_ls[[2]][[alias]][[1]],collapse = " - "),"<br>",
                       paste(pot_aliases_ls[[2]][[alias]][[2]],collapse = " - ")))
        })
        d$pot_aliases_ls <- pot_aliases_ls
        if(length(pot_aliases_ls$pot_affil)==0){
          pot_aliases_ls$pot_affil <- NULL
          pot_aliases_ls_text <- NULL
        }
        updateCheckboxGroupInput(session,"aliases_selected",label = "Found authors, please select (max 2)", 
                                 choiceNames = pot_aliases_ls_text, choiceValues = pot_aliases_ls[[1]])
    })
    
    observeEvent(input$aliases_selected,{
        orcid_ind <- str_which(input$aliases_selected,"([:digit:]{3,4} ?)+[:alpha:]?($| )")
        if(length(orcid_ind)>=1){
            d$pri_author <- input$aliases_selected[orcid_ind]
            d$orcid <- str_extract_all(input$aliases_selected[orcid_ind],"([:digit:]{3,4} ?)+[:alpha:]?($| )") %>% str_trim() %>% str_replace_all("[:space:]+","-")
            d$sec_author <- ifelse(length(input$aliases_selected)>1,input$aliases_selected[seq_along(input$aliases_selected)[-orcid_ind]],"")
        } else{
            d$pri_author <- input$aliases_selected[1]
            d$sec_author <- ifelse(length(input$aliases_selected)>1, input$aliases_selected[2],"")
            d$orcid <- NULL
        }
        updateTextInput(session,"orcid",value=d$orcid)
        d$pubmed <- tryCatch({pubmed_search_string_from_zora_id(d$pri_author,tbl_unique_authorkeys, input$cutoff_year)},error=function(e)"")
        updateTextInput(session,"pubmed",value=d$pubmed)
        d$publons <- tryCatch({get_ids_from_publons(d$orcid)[["publonsid"]]},error=function(e) "")
        d$publons <- ifelse(length(d$publons)==0,"",d$publons)
        updateTextInput(session,"publons",value=d$publons)
        if (is.null(d$pri_author)){
            disable("show_report")
            disable("report")
            disable("cutoff_year")
        } else{
            enable("show_report")
            enable("report")
            enable("cutoff_year")
        }
    })
    
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
    
    observeEvent(input$show_report,{
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,d$pri_author,d$sec_author)
        if (!is.null(progress)) progress$set( value = progress$getValue() + 1/5, message="create table from Zora")
        d$zora <- create_zora(d$pri_author,d$sec_author,tbl_author,tbl_subjects)
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5, message="create table from Pubmed")
        if (str_trim(d$pubmed) != ""){
          d$df_pubmed <- retrieve_from_pubmed(d$pubmed)
        }
        if (!is.null(d$df_pubmed)){
          tmpoadoi <- oadoi_fetch_local(na.omit(d$df_pubmed$doi),unpaywall)
          d$df_pubmed <- left_join(d$df_pubmed,tmpoadoi,by="doi")
        }
        print("df_pubmed")
        print(dim(d$df_pubmed))
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5, message="create table from Orcid")
        d$df_orcid <- tryCatch({retrieve_from_orcid(d$orcid) %>%
                dplyr::mutate(doi = tolower(doi))},
                error=function(e) NULL)
        # print(d$df_orcid)
        if (!is.null(d$df_orcid)){
          tmpoadoi <- oadoi_fetch_local(na.omit(d$df_orcid$doi),unpaywall)
          d$df_orcid <- left_join(d$df_orcid,tmpoadoi,by="doi")
        }
        print("df_orcid")
        print(dim(d$df_orcid))
        if (str_trim(d$publons) != ""){
          d$df_publons <- retrieve_from_publons(d$publons)
          print("df_publons")
          print(dim(d$df_publons))
        }
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5, message="Combine table")
        # tbl_merge <- create_combined_data_wrapper(tbl_authorkeys,tbl_unique_authorkeys,tbl_eprints,tbl_subjects,d$pri_author,d$sec_author,d$orcid,unpaywall,progress)
        tbl_merge <- create_combined_data(d$df_orcid,d$df_pubmed,d$zora,d$df_publons,unpaywall)
        
        if (str_trim(d$scholar) != ""){
          d$df_scholar <- retrieve_from_scholar(d$scholar)
          d$df_scholar <- df_scholar_matching(tbl_merge,d$df_scholar)
          print(dim(tbl_merge))
          tbl_merge <- full_join(tbl_merge,d$df_scholar,by="doi",suffix=c("",".scholar"))
          print(dim(tbl_merge))
          print(names(tbl_merge))
        }
        d$m <- tbl_merge
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5)
    })
    
    ### Zora only plot
    p_t$zora_only_plot <- reactive({tryCatch({oa_status_time_plot(d$zora,input$cutoff_year)},error=function(e) ggplot() + geom_blank())})
    output$plot_zora_only <- renderPlot({
        p_t$zora_only_plot()
    })

    ### Zora closed table
    p_t$closed_in_zora_table <- reactive({closed_in_zora_table(d$zora)})
    output$table_closed_in_zora <- DT::renderDataTable({
        p_t$closed_in_zora_table()
    })

    ### Orcid only plot
    p_t$orcid_only_plot <- reactive({tryCatch({oa_status_time_plot(d$df_orcid,input$cutoff_year,year,title="Orcid OA Status")},error=function(e) ggplot() + geom_blank())})
    output$plot_orcid_only <- renderPlot({
        p_t$orcid_only_plot()
    })
    
    ### Pubmed only plot
    p_t$pubmed_only_plot <- reactive({tryCatch({oa_status_time_plot(d$df_pubmed,input$cutoff_year,pubyear,title="Pubmed OA Status")},error=function(e) ggplot() + geom_blank())})
    output$plot_pubmed_only <- renderPlot({
      p_t$pubmed_only_plot()
    })
    
    ### Zora and Orcid plot
    p_t$zora_orcid_plot <- reactive({tryCatch({oa_status_time_plot(d$m %>% filter(in_zora | in_orcid),input$cutoff_year,title = "Zora + Orcid OA Status",oa_status_used=overall_oa)},error=function(e) ggplot() + geom_blank())})
    output$plot_zora_orcid <- renderPlot({
        p_t$zora_orcid_plot()
    })

    ### OA percent time table
    p_t$oa_percent_time_table <- reactive({tryCatch({oa_percent_time_table(d$m,input$cutoff_year)},error=function(e) ggplot() + geom_blank())})
    output$table_oa_percent_time <- DT::renderDataTable({
        p_t$oa_percent_time_table()
    })

    ### overall closed table
    p_t$overall_closed_table <- reactive({tryCatch({overall_closed_table(d$m,input$cutoff_year)},error=function(e) DT::datatable())})
    output$table_overall_closed <- DT::renderDataTable({
        p_t$overall_closed_table()
    })

    ### in zora but not in orcid table
    p_t$zora_without_orcid_table <- reactive({tryCatch({zora_without_orcid_table(d$m,input$cutoff_year)},error=function(e) DT::datatable())})
    output$table_zora_without_orcid <- DT::renderDataTable({
        p_t$zora_without_orcid_table()
    })

    ### in orcid but not in zora table
    p_t$orcid_without_zora_table <- reactive({tryCatch({orcid_without_zora_table(d$m,input$cutoff_year)},error=function(e) DT::datatable())})
    output$table_orcid_without_zora <- DT::renderDataTable({
        p_t$orcid_without_zora_table()
    })

    ### oa status difference between zora and unpaywall
    p_t$oa_status_diff_zora_unpaywall_table <- reactive({tryCatch({oa_status_diff_zora_unpaywall_table(d$m,input$cutoff_year)},error=function(e) DT::datatable())})
    output$table_orcid_without_zora <- DT::renderDataTable({
        p_t$oa_status_diff_zora_unpaywall_table()
    })
    
    ### oa status difference between zora and unpaywall
    p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m)},error=function(e) ggplot() + geom_blank())})
    output$plot_upset <- renderPlot({
      p_t$upset_plot()
    })
        
    
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
}

# Run the application 
shinyApp(ui = ui, server = server)
