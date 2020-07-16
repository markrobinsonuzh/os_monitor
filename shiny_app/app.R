#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(rcrossref)
library(roadoi)
library(ggplot2)
devtools::load_all("../uzhOS")
# devtools::load_all("uzhOS")
outdir <- here::here("output")
cat("load unpaywall data\n")
unpaywall <- readRDS(file.path(outdir,"dois_unpaywall_subset.rds"))
cat("load other data\n")
tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))
tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))
tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
unique_authorkeys <- unique(tbl_authorkeys$authorkey)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Open science monitor UZH"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # textInput("pri_author",label = "pri_author",value = "robinson m 0000 0002 3048 5518"),
            # textInput("sec_author",label = "sec_author", value = "robinson m d"),
            selectizeInput("author_search","Author search",NULL,selected = NULL, multiple = FALSE,
                           options = list(maxOptions = 50,placeholder="select author",maxItems=2)),
            textInput("orcid",label = "Orcid", value="0000-0002-3048-5518"),
            sliderInput("cutoff_year",label = "cutoff_year",min=2001,max = 2020,value=2001),

            actionButton(inputId = "show_report",label = "Show report")
            # downloadButton("report", "Generate report")
            ),
        downloadButton("report", "Generate report")
        

    ),
    mainPanel(

        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Zora", plotOutput("plot_zora_only")),
                    tabPanel("Closed in Zora", DT::dataTableOutput("table_closed_in_zora")),
                    tabPanel("Zora & Orcid", plotOutput("plot_zora_orcid")),
                    tabPanel("Percent closed", DT::dataTableOutput("table_oa_percent_time"))
                    
                    
                    # tabPanel("Summary", verbatimTextOutput("summary")),
                    # tabPanel("Table", tableOutput("table"))
        )

    )
)

# Define server logic required to draw a histogram
server = function(input, output,session) {
    updateSelectizeInput(session, 'author_search', choices = unique_authorkeys, server = TRUE)
    d <- reactiveValues()
    # d$open_cols <- c("closed" = "gray48", "hybrid" = "darkorange1",
    #                       "green" = "chartreuse4", "gold" = "gold",
    #                       "preprint" = "red", "bronze" = "darkgoldenrod4",
    #                       "blue" = "blue")
    observeEvent(input$show_report,{
        tbl_author <- create_tbl_author(tbl_authorkeys,tbl_eprints,input$author_search[1],input$author_search[2])
        zora <- create_zora(input$author_search[1],input$author_search[2],tbl_author,tbl_subjects)
        m <- create_combined_data_wrapper(tbl_authorkeys,tbl_eprints,tbl_subjects,input$author_search[1],input$author_search[2],input$orcid,unpaywall)
        d$m <- m
        d$zora <- zora
    })
    
    ### Zora only plot
    plot_zora_only_reactive <- eventReactive(input$show_report,{
        zora_only_plot(d$zora,input$cutoff_year)
    })
    output$plot_zora_only <- renderPlot({
        plot_zora_only_reactive()
    })

    ### Zora closed table
    table_closed_in_zora_reactive <- eventReactive(input$show_report,{
        closed_in_zora_table(d$zora)
    })
    output$table_closed_in_zora <- DT::renderDataTable({
        table_closed_in_zora_reactive()
    })
    
    ### Zora and Orcid plot
    plot_zora_orcid_reactive <- eventReactive(input$show_report,{
        zora_orcid_plot(d$m,input$cutoff_year)
    })
    output$plot_zora_orcid <- renderPlot({
        plot_zora_orcid_reactive()
    })
    
    ### OA percent time table
    table_oa_percent_time_reactive <- eventReactive(input$show_report,{
        oa_percent_time_table(d$m,input$cutoff_year)
    })
    output$table_oa_percent_time <- DT::renderDataTable({
        table_oa_percent_time_reactive()
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            d$pri_author <- input$author_search[1]
            d$sec_author <- input$author_search[2]
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(pri_author = d$pri_author,
                           sec_author = d$sec_author,
                           orcid = input$orcid,
                           cutoff_year = input$cutoff_year,
                           tbl_subjects=tbl_subjects,
                           tbl_authorkeys=tbl_authorkeys,
                           tbl_eprints=tbl_eprints,
                           unpaywall=unpaywall)
            
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
