
#' java script for closing session after inactivity
#'
#' @param id namespace
#' @param timeoutSeconds double, number of seconds until timeout
#'
#' @return character string of js script.
#' @details From https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work
#' @export
#'
#' @examples
#' js_timeout_fun()
js_timeout_fun <- function(id="timeout", timeoutSeconds=3600){
  sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
Shiny.setInputValue('%s-timeOut', '%ss')
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, id, timeoutSeconds, timeoutSeconds*1000)
}


#' server module for timeout
#'
#' @param id namespace
#' @param sps reactiveVal userid
#'
#' @return
#' @export
#'
#' @examples
time_out_server <- function(id, sps) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$timeOut, { 
        shiny_print_logs(paste0("Session (", session$token, ") time out"), sps)
        showModal(modalDialog(
          title = "Timeout",
          paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
          footer = NULL
        ))
        session$close()
      })
    }
  )
}

#' ui module for timeout
#'
#' @param id namespace
#'
#' @return
#' @details To include somewhere in UI, will not be visible.
#' @export
#'
#' @examples
time_out_ui <- function(id) {
  tags$script(js_timeout_fun(id))
}