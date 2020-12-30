#' UI function for \code{\link{shinyApp_general}}
#'
#' @param request shiny internal stuff
#'
#' @return ui
#' 
#' @import shiny 
#' @import stringr 
#' @import shinyjs 
#' @import shinyWidgets 
#' @import shinydashboard 
#' @import shinydashboardPlus
#' @importFrom magrittr %>% 
#' 
#' @export
#' @example 
#'  shiny::shinyApp(ui=shiny_general_ui,server = function(input, output) { })
shiny_general_ui <- function(request) {
  dashboardPage(
    preloader = list(
      waiter = list(html = tagList(waiter::spin_1(), "Loading ..."), color = "#3c8dbc"),
      duration = 1
    ),
    title =  "Open access monitor",
    dashboardHeader(
      leftUi = tagList(h4("Open access monitor")),
      dropdownMenu(type="tasks",
                   icon=icon("info fa-1g"),
                   badgeStatus=NULL,
                   headerText="Additional information",
                   tags$li(a(tags$i(class="fab fa-github text-primary"), 
                             "Source code",
                             href= "https://github.com/markrobinsonuzh/os_monitor",target="_blank")),
                   tags$li(a(tags$i(class="fa fa-bug text-primary"), 
                             "Bug report",
                             href= "https://github.com/markrobinsonuzh/os_monitor/issues",target="_blank"))
      )
      ),
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu(id="menu",
                                 menuItem("Author", tabName = "Author", icon = icon("user")),
                                 menuItem("About", tabName = "About", icon = icon("info"))
                     )
    ),
    dashboardBody(
      time_out_ui("timeout"),
      useShinyjs(),
      shinyFeedback::useShinyFeedback(),
      tabItems(
        # First tab content
        tabItem(tabName = "Author",
                fluidRow(
                  column(width = 4,
                         box(title = "Author information", width = NULL, collapsible = TRUE, id = "box_author_input",
                             # info
                             # boxPad(descriptionBlock(numberColor="aqua", "Enter one or more of the following IDs:")),
                             p("Enter one or more of the following IDs:"),
                             # Orcid input
                             inputOrcidUI("input_check"),
                             # google scholar input
                             inputScholarUI("input_check"),
                             # scholar match with crossref
                             shinyWidgets::prettySwitch(inputId = "scholar_matching_with_crossref",
                                                        label = "Use Crossref for increased accuracy in matching publications.", 
                                                        value = FALSE,
                                                        fill = TRUE, status = "primary"),              
                             # publons input
                             inputPublonsUI("input_check"),
                             # Pubmed query input
                             inputPubmedUI("input_check"),
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
                                   oa_diagram_UI("oa_diag"),
                                   oaSummaryUI("oa_summary"),
                                   # uiOutput("pgb_closed"),
                                   br(),
                                   boxPad(color = "teal",
                                          # to change the color of "teal"
                                          tags$style(HTML(".bg-teal {
                                           background-color:#FFFAFA!important;
                                           color:#000000!important;
                                          }")),
                                          fluidRow(
                                            column(width = 10,
                                                   h4("Filter")),
                                            column(width = 2, 
                                                   shinyWidgets::dropdown(
                                                     style = "stretch", status= "primary", 
                                                     tooltip = shinyWidgets::tooltipOptions(title = "Help"),
                                                     icon=icon("question"),
                                                     div(style="width:500px",
                                                       h5("Dataset selection"),
                                                       p("A logical filter for the datasets can be specfied.
                                                         For example: 'Not in orcid And in scholar' means to only show
                                                         publications that are in Google scholar and at the same time not in ORCID."),
                                                       h5("Cutoff year"),
                                                       p("Only publications within the year limits are shown."),
                                                       h5("OA status"),
                                                       p("Only the publications with the selected OA status are shown.")
                                                     )
                                                   ))
                                          ),
                                          fluidRow(
                                            column(width = 8,
                                                   h5("Dataset selection"),
                                                   datasetSelectionsUpdateUI("selection_standard")
                                            ),
                                            column(width = 4,
                                                   sliderInput("range_year",
                                                               label = "Cutoff year", sep = "",
                                                               min=2001, max = 2020,
                                                               value=c(2001,2020)),
                                                   selectizeInput("oa_status_filtered_table","OA status",
                                                                  names(open_cols_fn()),
                                                                  selected = names(open_cols_fn()), 
                                                                  multiple = TRUE, 
                                                                  options = list(maxOptions = 10,
                                                                                 placeholder="select oa status",
                                                                                 maxItems=10)))
                                          )
                                   ))
                             )) %>% shinyjs::hidden(),
                         div(id="shinyjsbox_upsetplot",
                             box(width = NULL, title = "Upsetplot (alternative to Venn Diagram)", 
                                 collapsible = TRUE, id = "box_upsetplot",
                                 boxPad(color = "teal",
                                        # to change the color of "teal"
                                        tags$style(HTML(".bg-teal {
                                           background-color:#FFFAFA!important;
                                           color:#000000!important;
                                          }")),
                                         fluidRow(
                                           column(width = 2, offset = 10,
                                                  shinyWidgets::dropdown(
                                                    style = "stretch", status= "primary",
                                                    tooltip = shinyWidgets::tooltipOptions(title = "Help"),
                                                    icon=icon("question"),
                                                    div(style="width:500px",
                                                      p("A Upset plot is an alternative to a Venn Diagram in that
                                                        it shows the size of different sets in which the publications can be found.")
                                                    )
                                                  ))
                                         ),
                                         plotOutput("plot_upset")
                                 )
                             )) %>% shinyjs::hidden()
                  ),
                  column(
                    width = 8,
                    div(id="shinyjsbox_histogram", 
                        tabBox(width = NULL, title = "Histogram of publications", id = "box_histogram",
                               tabPanel("Time", value = "box_histogram_panel_time",
                                        box(width = NULL, collapsible = TRUE, title = tags$p("Help",style="font-size:12px"), status = "info",
                                            icon = tags$i(class = "fas fa-question-circle", style="font-size: 12px"),
                                            column(width = 12,
                                                   p("The data shown in this histogram is filtered based on the inputs specified in the
                                                     box to the left ('Filter options')."),
                                                   p("To show specific publications for the same year and same open access status, 
                                                     click directly in the histogram on the respective section. A popup table
                                                     will appear, showing the entries.")
                                            )
                                        ),
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
                            # tags$head( 
                            #   tags$style(HTML(".box-header{height: 12px; padding-top: 0px; padding-bottom: 0px;}"))
                            # ),
                            box(width = NULL, collapsible = TRUE, title = tags$p("Help",style="font-size:12px"), status = "info",
                                icon = tags$i(class = "fas fa-question-circle", style="font-size: 12px"),
                                 column(width = 12,
                                        p("The Table below showing the list of publications can be further filtered."),
                                        p("Individual publications can be selected by directly clicking on the respective rows and 
                                          the filter can be applied by pressing 'Apply selection'"),
                                        p("To reverse the filter, press 'Reset selection'."),
                                        p("A Citation list of all displayed publications can be generated by pressing 'Bibtex citation'.
                                          This will start on confirmation and will be displayed underneath the table where also the
                                          possibility for download as a file exists.")
                                 )
                            ),
                            boxPad(fluidRow(
                                   actionButton(inputId = "apply_DT_selection",label = HTML("Apply <br/> selection")),
                                   actionButton(inputId = "reset_DT_selection",label = HTML("Reset <br/> selection")),
                                   actionButton(NS("bibtex", "create_bibtex"),HTML("Bibtex <br/> citation")))
                            ),
                            DT::dataTableOutput("table_selected_closed")
                        )) %>% shinyjs::hidden(),
                    div(id="shinyjsbox_bibtex", 
                        box(width = NULL, collapsible = TRUE,  id = "box_bibtex",
                            downloadButton(NS("bibtex", "bibtex_download"), "Download Bibtex citation") %>% 
                              shinyjs::hidden(),
                            verbatimTextOutput(NS("bibtex", "bibsummary"))
                        )) %>% shinyjs::hidden(),
                    # div(id="shinyjsbox_pubmetric_table", 
                    #     box(width = NULL, collapsible = TRUE,  id = "box_pubmetric_table",
                    #         DT::dataTableOutput("table_pubmetric")
                    #     )) %>% shinyjs::hidden(),
                    div(id="shinyjsbox_fulltext_download", 
                        box(width = NULL, collapsible = TRUE,  id = "box_fulltext_download", 
                            title = "Grey zone (Sci-hub)",collapsed = TRUE,
                            p("Retrieve fulltext links of closed publications from sci-hub"),
                            actionButton(NS("scihub", "fulltext_download_button"), label = "Get Fulltext links"),
                            DT::dataTableOutput(NS("scihub","table_fulltext_links"))
                        )) %>% shinyjs::hidden()
                  )
                )
        ),
        # Second tab content
        tabItem(tabName = "About",
                fluidPage(
                  withMathJax(
                    includeMarkdown(
                      file.path(
                        system.file("extdata","helpfiles",package = "uzhOS"),
                        "OA_monitor_documentation.md")
                    )
                  )
                )
                )
      )
    )
    # ,
    # footer=dashboardFooter("OA Monitor", "UZH")
  )
}
