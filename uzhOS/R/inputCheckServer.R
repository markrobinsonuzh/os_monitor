#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
pubmedActivateServer <- function(id, d, con) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
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
orcidCheckServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$orcid,{
        if(input$orcid != ""){
          if(check_if_likely_orcid(input$orcid)){
            d$is_valid_orcid <- any(tryCatch(rorcid::as.orcid(x = input$orcid),error=function(e) "") != "")
          } else {
            d$is_valid_orcid <- FALSE
          }
          shinyFeedback::feedbackWarning(
            "orcid", 
            !d$is_valid_orcid,
            "Please select a valid Orcid!"
          ) 
        }
        d$orcid <- input$orcid
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
pubmedCheckServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$pubmed,{
        if(input$pubmed != ""){
          d$is_valid_pubmed <- TRUE
        } else {
          d$is_valid_pubmed <- FALSE
        }
        d$pubmed <- input$pubmed
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
scholarCheckServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$scholar,{
        if(input$scholar != ""){
          d$is_valid_scholar <- any(tryCatch(scholar::get_profile(input$scholar),error=function(e) "") != "")
          shinyFeedback::feedbackWarning(
            "scholar", 
            !d$is_valid_scholar,
            "Please select a valid Google scholar id!"
          )
          d$scholar <- input$scholar
        }
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
publonsCheckServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent(input$publons,{
        if(input$publons != ""){
          d$is_valid_publons <- in_publons(input$publons)
          shinyFeedback::feedbackWarning(
            "publons", 
            !d$is_valid_publons,
            "Please select a valid ResearcherID!"
          )
        }
        d$publons <- input$publons
      })
      })}