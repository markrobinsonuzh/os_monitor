#' Title
#'
#' @param request 
#'
#' @return
#' @export
#'
#' @examples
shiny_general_ui <- function(request) {
  dashboardPage(
    preloader = list(
      waiter = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
      duration = 1
    ),
    title =  "Open access monitor",
    dashboardHeader(leftUi = tagList(
      h4("Open access monitor"))),
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu(id="menu",
                                 menuItem("Author", tabName = "Author", icon = icon("user"))
                     )
    ),
    dashboardBody(
      useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      tabItems(
        # First tab content
        tabItem(tabName = "Author",
                fluidRow(
                  column(width = 4,
                         box(title = "Author information", width = NULL, collapsible = TRUE, id = "box_author_input",
                             # Orcid input
                             textInput(NS("input_check","orcid"),
                                       label = a("Orcid",href="https://orcid.org",target="_blank"), value=""),
                             # Pubmed query input
                             splitLayout(cellWidths = c("75%", "25%"),
                                         textAreaInput(NS("input_check", "pubmed"),label = a("Pubmed Query",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"), value="") %>% 
                                           shinyjs::disabled(),
                                         actionButton(NS("input_check", "activate_pubmed"),HTML("Generate <br/> Pubmed <br/> Query"))),
                             # google scholar input
                             textInput(NS("input_check", "scholar"),label = a("Google Scholar id",href="https://scholar.google.ch",target="_blank"), value="") %>% 
                               shinyhelper::helper(type="markdown",
                                                   title = "Google scholar id help",
                                                   content = 'Google_scholar_help'),
                             # publons input
                             textInput(NS("input_check", "publons"),label = tags$div(tags$a("Publons id",href="https://publons.com",target="_blank"),
                                                                                     tags$span(class="help-block","(or if linked: ORCID, ResearcherID or TRUID)")), value=""),
                             # if pubmetrics
                             shinyWidgets::prettySwitch(inputId = "retrieve_pubmetric",label = "Retrieve Pubmed citation metrics", 
                                                        fill = TRUE, status = "primary"),                             
                             # aggregate data
                             showReportUI("show_report"),
                             ProgressbarUI("show_report")
                         ),
                         div(id="shinyjsbox_author_filter",
                             box(width = NULL, title = "Filter options", collapsible = TRUE, id = "box_author_filter",
                                 boxPad(
                                   uiOutput("pgb_closed"),
                                   br(),
                                   boxPad(color = "teal",
                                          # to change the color of "teal"
                                          tags$style(HTML(".bg-teal {
                               background-color:#FFFAFA!important;
                               color:#000000!important;
                              }")),
                                          h4("Filter"),
                                          fluidRow(
                                            column(width = 8,
                                                   h5("Dataset selection"),
                                                   datasetSelectionsUpdateUI("selection_standard")
                                            ),
                                            column(width = 4,
                                                   sliderInput("range_year",label = "Cutoff year",min=2001,max = 2020,value=c(2001,2020)),
                                                   selectizeInput("oa_status_filtered_table","OA status",names(open_cols_fn()),selected = names(open_cols_fn()), multiple = TRUE, 
                                                                  options = list(maxOptions = 10,placeholder="select oa status",maxItems=10)))
                                          )
                                   ))
                             )) %>% shinyjs::hidden(),
                         div(id="shinyjsbox_upsetplot",
                             box(width = NULL, title = "Upsetplot (alternative to Venn Diagram)", collapsible = TRUE, id = "box_upsetplot",
                                 plotOutput("plot_upset")
                             )) %>% shinyjs::hidden()
                  ),
                  column(
                    width = 8,
                    div(id="shinyjsbox_histogram", 
                        tabBox(width = NULL, title = "Histogram of publications", id = "box_histogram",
                               tabPanel("Time", value = "box_histogram_panel_time",
                                        plotlyOutput("plot_selected") %>% 
                                          shinyhelper::helper(type="markdown",
                                                              title = "Histogram selection help",
                                                              content = 'Histogram_selection')),
                               tabPanel("Citations", value = "box_histogram_panel_citations",
                                        uiOutput("plot_pubmetric")
                               )
                               #          )
                               # box(width = NULL, title = "Histogram of publications", collapsible = TRUE, id = "box_histogram",
                               #   plotlyOutput("plot_selected") %>% 
                               #     shinyhelper::helper(type="markdown",
                               #                         title = "Histogram selection help",
                               #                         content = 'Histogram_selection')
                        )) %>% shinyjs::hidden(),
                    div(id="shinyjsbox_table", 
                        box(width = NULL, title = "Table of publications", collapsible = TRUE,  id = "box_table",
                            sidebar = boxSidebar(startOpen = TRUE, background = "#F0F8FF" , width = "15%", icon = shiny::icon("angle-double-left"),
                                                 id = "publ_table_box_sidebar",
                                                 column(width = 12,
                                                        p("Click on publications in list and press ", style = "color:black"),
                                                        actionButton(inputId = "apply_DT_selection",label = HTML("Apply <br/> selection")),
                                                        p("to filter.", style = "color:black"),
                                                        br(),
                                                        br(),
                                                        p("To retrieve all publications press", style = "color:black"),
                                                        actionButton(inputId = "reset_DT_selection",label = HTML("Reset <br/> selection")),
                                                        br(),
                                                        br(),
                                                        p("Create Bibtex citation list of publications in list by pressing", style = "color:black"),
                                                        actionButton("create_bibtex",HTML("Bibtex <br/> citation"))
                                                 ) %>%
                                                   shinyhelper::helper(type="markdown",
                                                                       title = "Apply and Reset selection help",
                                                                       content = 'Apply_and_Reset_selection')
                            ),
                            DT::dataTableOutput("table_selected_closed")
                        )) %>% shinyjs::hidden(),
                    div(id="shinyjsbox_bibtex", 
                        box(width = NULL, collapsible = TRUE,  id = "box_bibtex",
                            downloadButton("bibtex_download", "Download Bibtex citation") %>% 
                              shinyjs::hidden(),
                            verbatimTextOutput("bibsummary")
                        )) %>% shinyjs::hidden(),
                    div(id="shinyjsbox_pubmetric_table", 
                        box(width = NULL, collapsible = TRUE,  id = "box_pubmetric_table",
                            DT::dataTableOutput("table_pubmetric")
                        )) %>% shinyjs::hidden()
                  )
                )
        )
      )
    )
  )
}