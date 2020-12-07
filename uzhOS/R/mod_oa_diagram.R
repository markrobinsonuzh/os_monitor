#' UI module for oa diagram
#'
#' @param id for namespace
#'
#' @return taglist of checkboxgroupinput
#' @export
#' @import shiny
#' @import DiagrammeR
#' @import shinyWidgets
#'
oa_diagram_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(ns("dg"),"Open access status help",icon = icon("question-circle"),
               block = TRUE,style = "bordered",
               color = "primary",)
    # actionButton(ns("dg"),"Open access status help"),
  )
}


#' Server module for oa diagram
#'
#' @param id for namespace
#' @export
#' @import shiny
#' @import DiagrammeR
#'
oa_diagram_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dg <- renderGrViz({
        grViz("
          digraph dot {
          
          graph [layout = dot,
                 rankdir = LR]
          node [shape = rectangle,
                style = filled,
                color = black,
                fillcolor=white]
          a [label = 'open access', width=2]
          b [label = 'no ', width=0.3, shape=circle, fontsize=12]
          c [label = 'closed', fillcolor=Gray48]
          d [label = 'yes', width=0.3, shape=circle, fontsize=12]
          e [label = 'in repository', width=2]
          f [label = 'yes', width=0.3, shape=circle, fontsize=12]
          g [label = 'green', fillcolor=chartreuse4]
          h [label = 'no ', width=0.3, shape=circle, fontsize=12]
          i [label = 'open access\n journal', width=2]
          j [label = 'yes', width=0.3, shape=circle, fontsize=12]
          k [label = 'gold', fillcolor=gold]
          l [label = 'no ', width=0.3, shape=circle, fontsize=12]
          m [label = 'published under\n open licence',  width=2]
          n [label = 'yes', width=0.3, shape=circle, fontsize=12]
          o [label = 'hybrid', fillcolor=darkorange1]
          p [label = 'no ', width=0.3, shape=circle, fontsize=12]
          q [label = 'bronze', fillcolor=darkgoldenrod4]
          
          edge [color = grey]
          a -> {b d}
          b -> {c}
          d -> {e}
          e -> {f h}
          f -> {g}
          h -> {i}
          i -> {j l}
          j -> {k}
          l -> {m}
          m -> {n p}
          n -> {o}
          p -> {q}
          }")
      })
      observeEvent(input$dg,{
        showModal(modalDialog(grVizOutput(NS(id,"dg")),
                              p("The above chart shows the definition of the open access status as defined by unpaywall. For more details see:"),
                              a("Unpaywall support",href= "https://support.unpaywall.org/support/solutions/articles/44001777288-what-do-the-types-of-oa-status-green-gold-hybrid-and-bronze-mean-",target="_blank"),
                              p("or also"),
                              a("Wikipedia",href= "https://en.wikipedia.org/wiki/Open_access#Definitions",target="_blank"),
                              title = "Open access definitions", size="l",easyClose = TRUE))
      })
    })}

# shinyApp(ui=fluidPage(oa_diagram_UI("id")),
#          server = function(input,output,session){
#            oa_diagram_Server("id")
#          })
