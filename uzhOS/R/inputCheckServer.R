#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
pubmedActivateServer <- function(id, df_zora, df_orcid, df_pubmed, con) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$activate_pubmed,{
        showModal(modalDialog("This is an automatically generated query and is 
                          unlikely to find all correct entries. For more details see:",
                              a("NCBI pubmedhelp",href= "https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.How_do_I_search_by_author",target="_blank"),
                              title = "Pubmed query info", size="s",easyClose = TRUE))
        
        pubmed_value <- tryCatch({
          pubmed_search_string_from_zora_id(input_value(df_zora()),
                                            con, 
                                            cutoff_year= c(2000),
                                            orcid = unlist(ifelse(input_value(df_orcid()) == "",list(NULL),input_value(df_orcid()))))
        },error=function(e)"")
        assign_to_reactiveVal(df_pubmed, "input_value", pubmed_value)
        enable("pubmed")
      })
    })}





#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
orcidCheckServer <- function(id, df_orcid) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$orcid,{
        if(check_if_likely_orcid(input$orcid)){
          assign_to_reactiveVal(df_orcid, 
                                "valid_input",
                                any(tryCatch(rorcid::as.orcid(x = input$orcid),error=function(e) "") != ""))
        } else {
          assign_to_reactiveVal(df_orcid, "valid_input", FALSE)
        }
        shinyFeedback::feedbackWarning(
          "orcid", 
          (input$orcid != "" && !valid_input(df_orcid())),
          "Please select a valid Orcid!"
        ) 
        assign_to_reactiveVal(df_orcid, "input_value", input$orcid)
      })})}

#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
pubmedCheckServer <- function(id, df_pubmed) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$pubmed,{
        if(input$pubmed != ""){
          assign_to_reactiveVal(df_pubmed, "valid_input", TRUE)
        } else {
          assign_to_reactiveVal(df_pubmed, "valid_input", FALSE)
        }
        assign_to_reactiveVal(df_pubmed, "input_value", input$pubmed)
      })
    })}

#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
scholarCheckServer <- function(id, df_scholar) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$scholar,{
        if (check_if_likely_scholar(input$scholar)){
          assign_to_reactiveVal(df_scholar, "valid_input", any(tryCatch(scholar::get_profile(input$scholar),error=function(e) "") != ""))
        } else {
          assign_to_reactiveVal(df_scholar, "valid_input", FALSE)
        }
        shinyFeedback::feedbackWarning(
          "scholar", 
          (input$scholar != "" && !valid_input(df_scholar())),
          "Please select a valid Google scholar id!"
        )
        assign_to_reactiveVal(df_scholar, "input_value", input$scholar)
        
      })
    })}

#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
publonsCheckServer <- function(id, df_publons) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$publons,{
        if(input$publons != ""){
          assign_to_reactiveVal(df_publons, "valid_input", in_publons(input$publons))
          shinyFeedback::feedbackWarning(
            "publons", 
            !valid_input(df_publons()),
            "Please select a valid ResearcherID!"
          )
        }
        assign_to_reactiveVal(df_publons, "input_value", input$publons)
      })
      })}