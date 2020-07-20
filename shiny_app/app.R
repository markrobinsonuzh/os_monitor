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
})
tryCatch(library(mongolite),error=function(e) NA)
tryCatch({setwd("/srv/shiny-server/os_monitor/shiny_app")},
         error=function(e) setwd("~/ownCloud/Projects/open_access/os_monitor/shiny_app/"))

devtools::load_all(here::here("uzhOS"))
outdir <- here::here("output")
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")


cat("load unpaywall data\n")

unpaywall <- tryCatch({mongo(collection="unpaywall", db="oa", url="mongodb://192.168.16.2/20")},
                      error=function(e) readRDS(file.path(outdir,"dois_unpaywall_subset.rds")))
cat("load other data\n")
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
unique_authorkeys <- unique(tbl_authorkeys$authorkey)

unique_authorkeys_which <-
    stringr::str_which(unique_authorkeys,"([:digit:]{3,4} ?)+[:alpha:]?($| )")
# unique_authorkeys_which_inv <- seq_along(unique_authorkeys)[-unique_authorkeys_which]

unique_authorkeys_sub <- tibble(id=unique_authorkeys_which,
       authorkeys_sub_no_digits = stringr::str_replace_all(unique_authorkeys[unique_authorkeys_which],"([:digit:]{3,4} ?)+[:alpha:]?($| )","") %>%
    stringr::str_trim() ,
    authorkeys_sub_only_digits = stringr::str_extract_all(unique_authorkeys[unique_authorkeys_which],"([:digit:]{3,4} ?)+[:alpha:]?($| )") %>%
    stringr::str_trim())

unique_authorkeys_no_digits <- tibble(author=unique_authorkeys,id=seq_along(unique_authorkeys))
unique_authorkeys_no_digits$author[unique_authorkeys_which] <- unique_authorkeys_sub$authorkeys_sub_no_digits

unique_authorkeys_processed <- unique(unique_authorkeys_no_digits$author)

# lensauth <- BiocParallel::bplapply(BPPARAM = BiocParallel::MulticoreParam(workers = 14),
#                        unique_authorkeys_processed,function(elem){
#     c(ind_auth=elem,len=length(unique_authorkeys[which(unique_authorkeys_no_digits==elem)]))
# }) %>% purrr::reduce(rbind)

# lensauth
# pri_author <- c("robinson m 0000 0002 3048 5518")
# sec_author <- c("robinson m d")
# orcid <- "0000-0002-3048-5518"
# m <- create_combined_data_wrapper(tbl_authorkeys,tbl_eprints,tbl_subjects,pri_author,sec_author,orcid,unpaywall)
# 
# oaf <- oadoi_fetch_local(unique(na.omit(m$doi)),unpaywall)

# author_string <- "robinson m d"
# author_string <- "schmid m r t"

# author_string <- "schmid b"

# potnam <- pot_alias_and_affil(author_string,unique_authorkeys_no_digits,tbl_subjects,tbl_authorkeys,tbl_eprints)
# org_unit_fac(potnam[[1]][4],"",tbl_subjects,tbl_authorkeys,tbl_eprints)

# ind_auth <- which(unique_authorkeys_no_digits$author==author_string)
# ind_pot <- lapply(possible_alias_author(author_string), function(auth){
#     which(unique_authorkeys_no_digits$author==auth)
# })
# tpmind <- ind_pot[lapply(ind_pot,length)>0]
# if (length(tpmind)==0){
#     pot_aliases <- unique_authorkeys[ind_auth]
# } else {
#     pot_aliases <- c(unique_authorkeys[ind_auth],unique_authorkeys[tpmind[[1]]])
# }
# pot_affil <- lapply(pot_aliases, function(pot_alias){
#     org_unit_fac(pot_aliases,"",tbl_subjects,tbl_authorkeys,tbl_eprints)
#     })


# tmpentr <- unique_authorkeys[tpmind[[1]]]
# org_unit_fac(tmpentr[1],"",tbl_subjects,tbl_authorkeys,tbl_eprints)





# ind_auth <- pmatch("robinson m",unique_authorkeys_no_digits$author,duplicates.ok = TRUE)

# tmpentr <- unique_authorkeys[ind_auth]

# tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,"robinson m 0000 0002 3048 5518","robinson m d")
# org_unit_fac(tmpentr[1],"",tbl_subjects,tbl_authorkeys,tbl_eprints)



# dept_fac <- tbl_author %>% left_join(tbl_subjects %>%
#                                          select(eprintid, name, parent_name))
#  dept_fac %>% select(name) %>% group_by(name) %>%
#     tally %>% top_n(1) %>% pull(name)
# dept_fac %>% select(parent_name) %>% group_by(parent_name) %>%
#     tally %>% top_n(1) %>% pull(parent_name)
# 
# eprintids <- sapply(tmpentr, function(i) tbl_authorkeys$eprintid[which(i==tbl_authorkeys$authorkey)])
# tbl_subjects %>% filter(eprintid %in% eprintids[[1]]) %>% select(name,parent_name) %>% unique()
# tbl_subjects %>% filter(eprintid %in% eprintids[[2]]) %>% select(name,parent_name) %>% unique()
# tbl_subjects %>% filter(eprintid %in% eprintids[[3]]) %>% select(name,parent_name) %>% unique()

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
                    tabPanel("Zora & Orcid", plotOutput("plot_zora_orcid")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time")),
                    tabPanel("Overall closed", DT::dataTableOutput("table_overall_closed")),
                    tabPanel("Zora", DT::dataTableOutput("table_zora_without_orcid")),
                    tabPanel("Orcid", DT::dataTableOutput("table_orcid_without_zora"))
                    
                    
                    
                    # tabPanel("Summary", verbatimTextOutput("summary")),
                    # tabPanel("Table", tableOutput("table"))
        )

    )
)

# Define server logic required to draw a histogram
server = function(input, output,session) {
    d <- reactiveValues(pri_author=NULL,sec_author=NULL)
    p_t <- reactiveValues()
    
    updateSelectizeInput(session, 'author_search', choices = unique_authorkeys_processed, server = TRUE,selected = sample(unique_authorkeys_processed,1))
    
    observeEvent(input$author_search,{
        pot_aliases_ls <- pot_alias_and_affil(input$author_search[1],unique_authorkeys,unique_authorkeys_no_digits,tbl_subjects,tbl_authorkeys,tbl_eprints)
        pot_aliases_ls_text <- lapply(pot_aliases_ls[[1]],function(alias){
            HTML(paste(alias,"<br>",
                       paste(pot_aliases_ls[[2]][[alias]][[1]],collapse = " - "),"<br>",
                       paste(pot_aliases_ls[[2]][[alias]][[2]],collapse = " - ")))
        })
        d$pot_aliases_ls <- pot_aliases_ls
        updateCheckboxGroupInput(session,"aliases_selected",label = "Found authors, please select (max 2)", 
                                 choiceNames = pot_aliases_ls_text, choiceValues = pot_aliases_ls[[1]])
    })
    
    observe({
        orcid_ind <- str_which(input$aliases_selected,"([:digit:]{3,4} ?)+[:alpha:]?($| )")
        if(length(orcid_ind)>=1){
            d$pri_author <- input$aliases_selected[orcid_ind]
            d$orcid <- str_extract_all(input$aliases_selected[orcid_ind],"([:digit:]{3,4} ?)+[:alpha:]?($| )") %>% str_replace_all("[:space:]+","-")
            d$sec_author <- ifelse(length(input$aliases_selected)>1,input$aliases_selected[seq_along(input$aliases_selected)[-orcid_ind]],"")
        } else{
            d$pri_author <- input$aliases_selected[1]
            d$sec_author <- ifelse(length(input$aliases_selected)>1, input$aliases_selected[2],"")
            d$orcid <- NULL
        }
        updateTextInput(session,"orcid",value=d$orcid)
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

    
    observeEvent(input$show_report,{
        progress <- shiny::Progress$new()
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
            tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,d$pri_author,d$sec_author)
            if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
            zora <- create_zora(d$pri_author,d$sec_author,tbl_author,tbl_subjects)
            if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
            m <- create_combined_data_wrapper(tbl_authorkeys,tbl_eprints,tbl_subjects,d$pri_author,d$sec_author,d$orcid,unpaywall,progress)
            if (!is.null(progress)) progress$set(value = progress$getValue() + 1/8)
            d$m <- m
            d$zora <- zora
    })
    
    ### Zora only plot
    p_t$zora_only_plot <- reactive({tryCatch({zora_only_plot(d$zora,input$cutoff_year)},error=function(e) ggplot() + geom_blank())})
    output$plot_zora_only <- renderPlot({
        p_t$zora_only_plot()
    })

    ### Zora closed table
    p_t$closed_in_zora_table <- reactive({closed_in_zora_table(d$zora)})
    output$table_closed_in_zora <- DT::renderDataTable({
        p_t$closed_in_zora_table()
    })

    ### Zora and Orcid plot
    p_t$zora_orcid_plot <- reactive({tryCatch({zora_orcid_plot(d$m,input$cutoff_year)},error=function(e) ggplot() + geom_blank())})
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
