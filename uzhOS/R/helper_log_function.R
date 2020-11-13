
#' @export
shiny_print_logs <- function(x, hostname){
  print(glue::glue("{hostname()}  {Sys.time()} {x}"))
}
