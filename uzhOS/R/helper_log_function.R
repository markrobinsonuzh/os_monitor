#' log function
#' 
#' @param x message to print
#' @param userData list with elements: 
#'   (userid = (reactiveVal containing username, etc.),
#'   col_nr= (integer between 1 and 6. 1=blue, 2=red, 3=green, 4=yellow,5=magenta, 6=cyan))
#' @export
shiny_print_logs <- function(x = "", userData=function(x) list(userid = "", col_nr=1)){
  if(!is.null(userData()$userid) && !is.null(x)){
    color_used <- switch(as.character(userData()$col_nr), 
                         '1'=crayon::blue, 
                         '2'=crayon::red, 
                         '3'=crayon::green, 
                         '4'=crayon::yellow, 
                         '5'=crayon::magenta, 
                         '6'=crayon::cyan,
                         crayon::black)
    message(glue::glue_col("{color_used {userData()$userid}  {Sys.time()}} {x}"))
  } 
}
