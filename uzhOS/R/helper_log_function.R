
#' @export
shiny_print_logs <- function(x, hostname){
  message(glue::glue("{hostname()}  {Sys.time()} {x}"))
}
