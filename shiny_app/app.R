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

tbl_unique_authorkeys_fullname <- tryCatch({mongo(collection="unique_authorkeys_fullname", db="oa", url="mongodb://192.168.16.2/20")},
                                  error=function(e) readRDS(file.path(outdir, "tbl_unique_authorkeys_fullname.rds")))
# tbl_unique_authorkeys_processed <- readRDS(file.path(outdir, "tbl_unique_authorkeys_processed.rds"))

# if (is(tbl_unique_authorkeys,"mongo")){
#   unique_authorkeys_processed <- tbl_unique_authorkeys$find('{}', fields='{"_id":0,"authorkey":0,"id":0,"author_name_family_given":0}') %>% dplyr::pull(authorkey_processed)
#   names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
# } else {
#   unique_authorkeys_processed <- tbl_unique_authorkeys$authorkey_processed
#   names(unique_authorkeys_processed) <- stringr::str_to_title(tbl_unique_authorkeys$authorkey_processed)
# }
if (is(tbl_unique_authorkeys_fullname,"mongo")){
  unique_authorkeys_processed <- unique(tbl_unique_authorkeys_fullname$find('{}', fields='{"_id":0,"authorkey":0,"id":0,"authorkey_fullname":0}') %>% dplyr::pull(authorname))
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
} else {
  unique_authorkeys_processed <- unique(tbl_unique_authorkeys_fullname$authorkey_processed)
  names(unique_authorkeys_processed) <- stringr::str_to_title(unique_authorkeys_processed)
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
                           options = list(maxOptions = 50,placeholder="select author",maxItems=3)) %>% 
              shinyhelper::helper(type="inline",
                                  title = "Author search input help",
                                  content = 'This is your full name as saved in ZORA. 
                                  The format is "(familiy name) (given name)". 
                                  It is possible that of your given name only the initials are saved in ZORA.
                                  Multiple selections are possible.'),
            checkboxGroupInput("aliases_selected","","") %>% 
              shinyjs::hidden() %>% 
              shinyhelper::helper(type="inline",
                                  title = "Author selection help",
                                  content = 'All entries for the given name(s) from above. 
                                  Please choose all registries that match. Most often 
                                  this is just one, but if you have a orcid linked
                                  to ZORA this will probably be two.'),
            textInput("orcid",label = HTML('<a href="https://orcid.org">Orcid</a>'), value=""),
            textInput("pubmed",label = HTML('<a href= https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author">Pubmed Query</a>'), value=""),
            textInput("scholar",label = HTML('<a href="https://scholar.google.ch">Google Scholar id</a>'), value=""),
            textInput("publons",label = HTML('<a href="https://publons.com">Publons id</a> (or if linked: ORCID, ResearcherID or TRUID)'), value=""),
            disabled(sliderInput("cutoff_year",label = "Cutoff year",min=2001,max = 2020,value=2001)),

            disabled(actionButton(inputId = "show_report",label = "Show report"))
            # downloadButton("report", "Generate report")
            ),
        disabled(downloadButton("report", "Generate report"))
        
        
    ),
    mainPanel(
        
        checkboxGroupInput(inputId = "in_selection",label = "Data sets included","",inline = TRUE) %>% 
          shinyjs::hidden(),

        wellPanel(
          titlePanel("Bibtex export"),
          
          splitLayout(
            checkboxGroupInput(inputId = "bib_in_selection",label = "In","",inline = TRUE) %>% 
              shinyjs::hidden(),
            checkboxGroupInput(inputId = "bib_not_in_selection",label = "Not in","",inline = TRUE) %>% 
              shinyjs::hidden()
            ),
          disabled(downloadButton("bibtex", "Generate bibtex citation file"))
          ),
        
        
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Upset", plotOutput("plot_upset")),
                    tabPanel("Zora", plotOutput("plot_zora_only")),
                    tabPanel("Closed in Zora", DT::dataTableOutput("table_closed_in_zora")),
                    tabPanel("Orcid", plotOutput("plot_orcid_only")),
                    tabPanel("Pubmed", plotOutput("plot_pubmed_only")),
                    tabPanel("Zora & Orcid", plotOutput("plot_zora_orcid")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time")),
                    tabPanel("Overall closed", DT::dataTableOutput("table_overall_closed")),
                    tabPanel("Zora", DT::dataTableOutput("table_zora_without_orcid")),
                    tabPanel("Orcid", DT::dataTableOutput("table_orcid_without_zora"))
        )

    )
)

# Define server logic required to draw a histogram
server = function(input, output,session) {
  shinyhelper::observe_helpers(session = session)
    d <- reactiveValues(pri_author=NULL,sec_author=NULL)
    p_t <- reactiveValues()
    
    updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE)
    
    observe({
      if (any(!is.null(input$author_search))){
        shinyjs::show(id="aliases_selected")
      } else {
        shinyjs::hide(id="aliases_selected")
      }
    })
    
    observeEvent(input$author_search,{

        pot_aliases_ls_ls <- lapply(input$author_search, function(e) {
          pot_alias_and_affil(e,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints)
          })
        pot_aliases <- unlist(lapply(pot_aliases_ls_ls, function(p) p[[1]]))
        pot_aliases_ls <- lapply(seq_along(pot_aliases_ls_ls), 
               function(i) lapply(seq_along(pot_aliases_ls_ls[[i]][["pot_affil"]]), 
                                  function(j) pot_aliases_ls_ls[[i]][["pot_affil"]][[j]]))
        pot_aliases_ls <- list()
        k <- 1
        for(i in seq_along(pot_aliases_ls_ls)){
          for(j in seq_along(pot_aliases_ls_ls[[i]][["pot_affil"]])){
            pot_aliases_ls[[k]] <- pot_aliases_ls_ls[[i]][["pot_affil"]][[j]]
            k <- k+1
          }
        }
        pot_aliases_ls_text <- lapply(seq_along(pot_aliases_ls),function(i){
            HTML(paste(pot_aliases_ls[[i]][["author_name"]],"<br>",
                       paste(pot_aliases_ls[[i]][["org_unit"]],collapse = " - "),"<br>",
                       paste(pot_aliases_ls[[i]][["fac"]],collapse = " - ")))
        })
        d$pot_aliases_ls <- pot_aliases_ls
        if(length(pot_aliases_ls)==0){
          pot_aliases_ls <- NULL
          pot_aliases_ls_text <- NULL
        }
        updateCheckboxGroupInput(session,"aliases_selected",label = "Found authors, please select (max 2)", 
                                 choiceNames = pot_aliases_ls_text, 
                                 choiceValues = sapply(seq_along(pot_aliases_ls),
                                                       function(i)pot_aliases_ls[[i]][["author_name"]]))
    })
    
    observeEvent(input$aliases_selected,{
        orcid_ind <- str_which(input$aliases_selected,"([:alnum:]{4}-){3}[:alnum:]{4}")
        if(length(orcid_ind)>=1){
            d$pri_author <- input$aliases_selected[orcid_ind]
            d$orcid <- str_extract_all(input$aliases_selected[orcid_ind],"([:alnum:]{4}-){3}[:alnum:]{4}")
            d$sec_author <- ifelse(length(input$aliases_selected)>1,input$aliases_selected[seq_along(input$aliases_selected)[-orcid_ind]],"")
        } else{
            d$pri_author <- input$aliases_selected[1]
            d$sec_author <- ifelse(length(input$aliases_selected)>1, input$aliases_selected[2],"")
            d$orcid <- NULL
        }
        updateTextInput(session,"orcid",value=d$orcid)
        d$pubmed <- tryCatch({pubmed_search_string_from_zora_id(d$pri_author,tbl_unique_authorkeys_fullname, input$cutoff_year)},error=function(e)"")
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
          d$df_publons <- tryCatch({retrieve_from_publons(d$publons)},error=function(e) {print(e);return(NULL)})
          print("df_publons")
          print(dim(d$df_publons))
        }
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5, message="Combine table")
        # tbl_merge <- create_combined_data_wrapper(tbl_authorkeys,tbl_unique_authorkeys,tbl_eprints,tbl_subjects,d$pri_author,d$sec_author,d$orcid,unpaywall,progress)
        tbl_merge <- create_combined_data(d$df_orcid,d$df_pubmed,d$zora,d$df_publons,unpaywall)
        
        if (str_trim(d$scholar) != ""){
          d$df_scholar <- retrieve_from_scholar(d$scholar)
          d$df_scholar <- df_scholar_matching(tbl_merge,d$df_scholar)
          # print(dim(tbl_merge))
          tbl_merge <- full_join(tbl_merge,d$df_scholar,by="doi",suffix=c("",".scholar"))
          # print(dim(tbl_merge))
          # print(names(tbl_merge))
        }
        if (!is.null(progress)) progress$set(value = progress$getValue() + 1/5)
        enable("bibtex")
        for(tmp_id in c("in_selection","bib_in_selection","bib_not_in_selection")){
          shinyjs::show(id = tmp_id)
        }
        updateCheckboxGroupInput(session,"bib_in_selection",
                                 choices =  names(tbl_merge)[grep("in_",names(tbl_merge))],
                                 selected = names(tbl_merge)[grep("in_",names(tbl_merge))][1])
        updateCheckboxGroupInput(session,"bib_not_in_selection",
                                 choices =  names(tbl_merge)[grep("in_",names(tbl_merge))],
                                 selected = names(tbl_merge)[grep("in_",names(tbl_merge))][1])
        updateCheckboxGroupInput(session,"in_selection",
                                 choices =  c(names(tbl_merge)[grep("in_",names(tbl_merge))],"inverse"),
                                 selected = c(names(tbl_merge)[grep("in_",names(tbl_merge))]))
        

        d$m <- tbl_merge
    })
    
    observeEvent(input$in_selection,{
      in_selection_quo <- quos(input$in_selection[input$in_selection != "inverse"])
      ind <- d$m %>%
        dplyr::select(!!!in_selection_quo) %>%
        purrr::reduce(.f=function(x,y){x|y})
      if ("inverse" %in% input$in_selection){
        ind <- !ind
      }
      d$m_sub <- d$m[ind,]
    })
    
    ### oa status upset plot
    # p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m_sub)},error=function(e) ggplot() + geom_blank())})
    p_t$upset_plot <- reactive({tryCatch({upset_plot(d$m_sub)},error=function(e) {print(e);ggplot() + geom_blank()})})
    output$plot_upset <- renderPlot({
      p_t$upset_plot()
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
    
    output$bibtex <- downloadHandler(
      filename = paste0("BIBTEX_FOR_ORCID_",d$orcid, ".bib"),
      content = function(file){
        bib_in_selection_quo <- quo(input$bib_in_selection[1])
        bib_not_in_selection_quo <- quos(input$bib_not_in_selection)
        
        ind <- d$m %>%
          dplyr::select(!!bib_in_selection_quo, !!!bib_not_in_selection_quo) %>%
          purrr::reduce(.f=function(x,y){x&!y})
        print(ind)
        ind <- !ind
        to_update <- d$m[ind,"doi"]
        print(to_update)
        if(length(to_update) > 0) {
          # df_pubmed[df_pubmed$doi %in% to_update,] %>% select(-authors,-pmid)

          # write missing entries to bibtex file
          bibtex_from_doi <- GetBibEntryWithDOI(to_update)
          # toBiblatex(bibtex_from_doi)
          writeLines(toBiblatex(bibtex_from_doi),file)
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
}

# Run the application 
shinyApp(ui = ui, server = server)
