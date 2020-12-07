#' log function
#' 
#' @param x message to print
#' @param hostname reactiveVal containing username, etc.
#' 
#' @export
shiny_print_logs <- function(x = "", hostname = function(x) ""){
  if(!is.null(hostname) && !is.null(x)){
    message(glue::glue("{hostname()}  {Sys.time()} {x}"))
  } 
}
