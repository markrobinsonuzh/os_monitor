#' create zora module
#'
#' @param id for namespace
#' @param d reactive value containing input
#'
#' @export
#' @import shiny
#' @import future
#' 
datasetSelectionsServer <- function(id, d,selection_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
    observeEvent({
      purrr::map(seq_len(length(d$all_selection_choices)*3), function(i){input[[paste0("blab",i)]]})
    },{
      # req(input$show_report)
      req(d$all_selection_choices)
      if(selection_ls$init){
        for(i in seq_len(length(d$all_selection_choices)*3-1)){
          if(i%%3 == 1){
            selection_ls[[paste0("blab",i)]] <- "In"
          } else if(i%%3 == 2){
            selection_ls[[paste0("blab",i)]] <- stringr::str_replace_all(d$all_selection_choices[(i+1)/3],"in_","")
            
          } else {
            selection_ls[[paste0("blab",i)]] <- "Or"
          }
        }
        selection_ls$init <- FALSE
      } else{
        for(i in seq_len(length(d$all_selection_choices)*3-1)){
          selection_ls[[paste0("blab",i)]] <- input[[paste0("blab",i)]]
        }
      }
      selection_list <- purrr::map(seq_len(length(d$all_selection_choices)*3-1), ~ selection_ls[[paste0("blab",.x)]])
      nr_not_null <- purrr::map(selection_list, ~ !is.null(.x)) %>% purrr::reduce(sum)
      if(nr_not_null < 2){
        total_not_null_needed <- 2
      }else if(nr_not_null%%3 == 0){
        total_not_null_needed <- nr_not_null + 2
      } else if (nr_not_null%%3 == 1){
        total_not_null_needed <- nr_not_null + 1
      } else if (nr_not_null%%3 == 2 && !((nr_not_null+1)/3 == length(d$all_selection_choices))){
        total_not_null_needed <- nr_not_null + 1
      } else {
        total_not_null_needed <- nr_not_null
      }
      filter_all_selection_choices <- function(i, all_selection_choices){
        id_to_remove <- seq(2,i,by=3)
        id_to_remove <- id_to_remove[-length(id_to_remove)]
        ind_to_remove <- purrr::map(id_to_remove, ~ which(paste0("in_",selection_ls[[paste0("blab",.x)]]) == all_selection_choices)) %>% unlist()
        if(length(ind_to_remove) > 0){
          ret_sele <- all_selection_choices[-ind_to_remove]
        } else {
          ret_sele <- all_selection_choices
        }
        return(ret_sele %>% stringr::str_replace_all("in_","") %>% unlist())
      }
      
      list_all_choices <- purrr::map(seq_len(ceiling(total_not_null_needed/3)), function(i){filter_all_selection_choices((i*3)-1,d$all_selection_choices)})
      output$selection_rules <- renderUI({
        # splitLayout(cellWidths = rep(100,total_not_null_needed),
        purrr::map(seq_len(total_not_null_needed), function(i){
          if(i %% 3 == 1){
            div(style="display: inline-block;vertical-align:top; width: 100px;",
                selectizeInput(inputId = NS(id, paste0("blab",i)), label = NULL, 
                               choices = c("In","Not In"), options = list(maxItems=1),
                               selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
          } else if(i %% 3 == 2){
            div(style="display: inline-block;vertical-align:top; width: 150px;",
                selectizeInput(inputId = NS(id, paste0("blab",i)), label = NULL, 
                               choices = list_all_choices[[(i+1)/3]], options = list(maxItems=1), 
                               selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
          } else {
            div(style="display: vertical-align:top; width: 70px;",
                selectizeInput(inputId = NS(id, paste0("blab",i)), label = NULL, 
                               choices = c("And","Or"), options = list(maxItems=1), 
                               selected = selection_ls[[paste0("blab",i)]], multiple = TRUE))
          }
        }) 
        # )
      })
      
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
datasetSelectionsUpdateServer <- function(id, d,selection_ls) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
  observeEvent({
    purrr::map(seq_len(length(d$all_selection_choices)*3), function(i){input[[paste0("blab",i)]]})
  },{
    # req(input$show_report)
    req(d$all_selection_choices)
    tmpfilt <- purrr::map(seq_len(length(d$all_selection_choices)*3-1), function(i){
      if(i%%3 == 1){
        ifelse(input[[paste0("blab",i)]] == "Not In" && !is.null(input[[paste0("blab",i)]]), "!", "")
      } else if(i%%3 == 2){
        ifelse(!is.null(input[[paste0("blab",i)]]),paste0("in_",input[[paste0("blab",i)]]),"")
        
      } else {
        ifelse(input[[paste0("blab",i)]] == "And" && !is.null(input[[paste0("blab",i)]]), "&", "|")
      }
    }) 
    tmpfilt <- tmpfilt %>% unlist() 
    for (i in seq(2,length(tmpfilt),by=3)){
      if(tmpfilt[i] == ""){
        tmpfilt[(i-1):(i+1)] <- ""
      }
    }
    tmpfilt <- tmpfilt %>% paste(collapse = "")
    if(stringr::str_detect(tmpfilt,"\\|$|&$")){
      tmpfilt <- stringr::str_replace(tmpfilt,"\\|$|&$","")
    }
    
    if(tmpfilt != ""){
      d$m_sub_all_oa <- subset(d$m,eval(parse(text=tmpfilt)))
    }
  
  })
    }
  )
}




#' Title
#'
#' @param id 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
datasetSelectionsUpdateUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selection_rules")),
  )
}
