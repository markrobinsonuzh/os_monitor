#' retrieve from scihub server module
#'
#' @param id namespace
#' @param d reactive values object
#' @param sci_hub_pdf_links \code{\link[shiny]{reactiveVal}}
#'
#' @import shiny
#' @import future
#' @import promises
#' @importFrom magrittr %>% 
#' @export
#'
ScihubObserveActionbuttonServer <- function(id, d, sci_hub_pdf_links) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent({input$fulltext_download_button},{
        req(input$fulltext_download_button)
        d$m_sub_sel_closed <- d$m_sub_sel %>% 
          dplyr::filter(overall_oa %in% c("closed","blue")) 
        
        shiny_print_logs(paste("get", dim(d$m_sub_sel_closed)[1], "fulltexts"), d$sps)
        if(dim(d$m_sub_sel_closed)[1] != 0){
          shinyjs::disable("fulltext_download_button")
          future(seed=NULL,{
            pdf_link_from_scihub(dois)
          }, globals = list(dois=dplyr::pull(d$m_sub_sel_closed, doi),
                            pdf_link_from_scihub=pdf_link_from_scihub)) %...>% 
            sci_hub_pdf_links()
        }

      })
    }
  )
}

#' update data on scihub retrieval server module
#'
#' @param id namespace
#' @param d reactive values object
#' @param sci_hub_pdf_links \code{\link[shiny]{reactiveVal}}
#'
#' @import shiny
#' @importFrom magrittr %>% 
#' @export
#'
ScihubObservePdflinksServer <- function(id, d, sci_hub_pdf_links) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      observeEvent({sci_hub_pdf_links()},{
        req(sci_hub_pdf_links())
        shiny_print_logs(paste("retrieved", length(sci_hub_pdf_links()), "fulltexts"), d$sps)
        d$m_sub_sel_closed_pdflink <- d$m_sub_sel_closed %>% 
          dplyr::mutate(pdflink = sci_hub_pdf_links()) %>% 
          dplyr::select(pdflink, doi, dplyr::starts_with("title"))
        shinyjs::enable("fulltext_download_button")
      })
    }
  )
}

#' render datatable of scihub data server module
#'
#' @param id namespace
#' @param d reactive values object
#'
#' @import shiny
#' @export
#'
ScihubRenderDTServer <- function(id, d) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$table_fulltext_links <- DT::renderDataTable({
        req(d$m_sub_sel_closed_pdflink)
        sci_hub_datatable(d$m_sub_sel_closed_pdflink)
      })
    }
  )
}