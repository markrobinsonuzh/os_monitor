#' create zora module
#'
#' @param id for namespace
#' @param df_zora \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_zora}}
#' @param df_orcid \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_orcid}}
#' @param df_pubmed \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_pubmed}}
#' @param con database connection, e.g. odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#'
#' @export
#' @import shiny
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
        shinyjs::enable("pubmed")
      })
    })}





#' check orcid input module
#'
#' @param id for namespace
#' @param df_orcid \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_orcid}}
#'
#' @export
#' @import shiny
#' 
orcidCheckServer <- function(id, df_orcid) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$orcid,{
        inp_orcid <- stringr::str_trim(input$orcid)
        if(check_if_likely_orcid(inp_orcid)){
          assign_to_reactiveVal(df_orcid, 
                                "valid_input",
                                any(tryCatch(rorcid::as.orcid(x = inp_orcid),error=function(e) "") != ""))
        } else {
          assign_to_reactiveVal(df_orcid, "valid_input", FALSE)
        }
        shinyFeedback::feedbackWarning(
          "orcid", 
          (inp_orcid != "" && !valid_input(df_orcid())),
          "Please select a valid Orcid!"
        ) 
        assign_to_reactiveVal(df_orcid, "input_value", inp_orcid)
      })})}


#' ccheck pubmed input module
#'
#' @param id for namespace
#' @param df_pubmed \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_pubmed}}
#'
#' @export
#' @import shiny
#' 
pubmedCheckServer <- function(id, df_pubmed) {
  moduleServer(
    id,
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

#' check scholar input module
#'
#' @param id for namespace
#' @param df_scholar \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_scholar}}
#'
#' @export
#' @import shiny
#' 
scholarCheckServer <- function(id, df_scholar) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$scholar,{
        inp_scholar <- stringr::str_trim(input$scholar)
        if (check_if_likely_scholar(inp_scholar)){
          assign_to_reactiveVal(df_scholar, "valid_input", any(tryCatch(scholar::get_profile(inp_scholar),error=function(e) "") != ""))
        } else {
          assign_to_reactiveVal(df_scholar, "valid_input", FALSE)
        }
        shinyFeedback::feedbackWarning(
          "scholar", 
          (inp_scholar != "" && !valid_input(df_scholar())),
          "Please select a valid Google scholar id!"
        )
        assign_to_reactiveVal(df_scholar, "input_value", inp_scholar)
        
      })
    })}

#' check publons input module
#'
#' @param id for namespace
#' @param df_publons \code{\link[shiny]{reactiveVal}} of format \code{\link{empty_publons}}
#'
#' @export
#' @import shiny
#' 
publonsCheckServer <- function(id, df_publons) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$publons,{
        inp_publons <- stringr::str_trim(input$publons)
        if(inp_publons != ""){
          ID_in_publons <- in_publons(inp_publons)
          if(is.null(ID_in_publons)){
            shinyFeedback::feedbackWarning(
              "publons", 
              TRUE,
              "Publons API blocked. Try again later."
            )
            ID_in_publons <- FALSE
          } else {
            shinyFeedback::feedbackWarning(
              "publons", 
              !ID_in_publons,
              "Please select a valid ResearcherID!"
            )
          }
          assign_to_reactiveVal(df_publons, "valid_input", ID_in_publons)

        }
        assign_to_reactiveVal(df_publons, "input_value", inp_publons)
      })
      })}