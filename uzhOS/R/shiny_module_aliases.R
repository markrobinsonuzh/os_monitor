#' UI module for 'aliases_selected'
#'
#' @param id for namespace
#'
#' @return taglist of checkboxgroupinput
#' @export
#' @import shiny
#' @importFrom magrittr %>% 
#'
#' @examples
alias_selected_UI <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("aliases_selected"),"","") %>% 
      shinyjs::hidden() %>% 
      shinyhelper::helper(type="inline",
                          title = "Author selection help",
                          content = 'All entries for the given name(s) from above. 
                                                  Please choose all registries that match. Most often 
                                                  this is just one, but if you have a orcid linked
                                                  to ZORA this will probably be two.')
  )
}



#' Server module for 'alias' to find and parse aliases
#'
#' @param id for namespace
#' @param author_search 
#' @param tbl_unique_authorkeys_fullname
#' @param tbl_subjects
#' @param tbl_authorkeys
#' @param tbl_eprints
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
alias_selected_Server <- function(id,author_search,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints, fac_vec=NULL,dep_vec=NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      pot_aliases_ls_ls <- lapply(author_search, function(e) {
        pot_alias_and_affil(e,tbl_unique_authorkeys_fullname,tbl_subjects,tbl_authorkeys,tbl_eprints,fac_vec, dep_vec)
      })
      pot_aliases <- unlist(lapply(pot_aliases_ls_ls, function(p) p[[1]]))
      pot_aliases_ls <- lapply(seq_along(pot_aliases_ls_ls), 
                               function(i) lapply(seq_along(pot_aliases_ls_ls[[i]][["pot_affil"]]), 
                                                  function(j) pot_aliases_ls_ls[[i]][["pot_affil"]][[j]]))
      pot_aliases_ls <- list()
      pot_alias_names_ls <- list()
      k <- 1
      for(i in seq_along(pot_aliases_ls_ls)){
        for(j in seq_along(pot_aliases_ls_ls[[i]][["pot_affil"]])){
          org_unit_tmp <- pot_aliases_ls_ls[[i]][["pot_affil"]][[j]][["org_unit"]]
          fac_tmp <- pot_aliases_ls_ls[[i]][["pot_affil"]][[j]][["fac"]]
          type_tmp <- pot_aliases_ls_ls[[i]][["pot_affil"]][[j]][["type"]]
          print(fac_tmp)
          print(type_tmp)
          if (!(is.null(org_unit_tmp) | is.null(fac_tmp))){
            # fac_org_diff <- dim(org_unit_tmp)[1] - dim(fac_tmp)[1]
            # if(fac_org_diff>0){
            #   fac_tmp <- rbind(fac_tmp,data.frame(fac=rep("",fac_org_diff),count=rep("",fac_org_diff)))
            # } else if (fac_org_diff<0){
            #   org_unit_tmp <- rbind(org_unit_tmp,data.frame(fac=rep("",dim(fac_tmp)[1]-fac_org_diff),count=rep("",fac_org_diff)))
            # }
            fac_org_type_max <- max(c(dim(org_unit_tmp)[1], dim(fac_tmp)[1], dim(type_tmp)[1]))
            org_unit_tmp <- rbind(org_unit_tmp,data.frame(fac=rep("",fac_org_type_max-dim(org_unit_tmp)[1]),
                                                count=rep("",fac_org_type_max-dim(org_unit_tmp)[1])))
            fac_tmp <- rbind(fac_tmp,data.frame(fac=rep("",fac_org_type_max-dim(fac_tmp)[1]),
                                                count=rep("",fac_org_type_max-dim(fac_tmp)[1])))
            type_tmp <- rbind(type_tmp,data.frame(type=rep("",fac_org_type_max-dim(type_tmp)[1]),
                                                count=rep("",fac_org_type_max-dim(type_tmp)[1])))
            
            # type_org_diff <- dim(type_tmp)[1] - dim(fac_tmp)[1]
            # if(type_org_diff>0){
            #   fac_tmp <- rbind(fac_tmp,data.frame(fac=rep("",type_org_diff),count=rep("",type_org_diff)))
            # } else if (type_org_diff<0){
            #   type_tmp <- rbind(org_unit_tmp,data.frame(fac=rep("",dim(fac_tmp)[1]-type_org_diff),count=rep("",type_org_diff)))
            # }
            pot_aliases_ls[[k]] <- cbind(fac_tmp,org_unit_tmp,type_tmp)
            pot_alias_names_ls[[k]] <- pot_aliases_ls_ls[[i]][["pot_aliases"]][j]
            k <- k+1
          }
        }
      }
      pot_aliases_ls_text <- lapply(seq_along(pot_aliases_ls),function(i){
        tmp <- paste0(htmlTable::htmlTable(head(pot_aliases_ls[[i]],5), 
                                           rnames=FALSE,header=c("Name","Count","Name","Count","Name","Count"), 
                                           # caption=paste("Author id:",pot_alias_names_ls[[i]]),
                                           cgroup=rbind(c(paste("Author id:",pot_alias_names_ls[[i]]),NA,NA),c("Faculty","Department","Type")),
                                           n.cgroup=rbind(c(6,NA,NA),c(2,2,2))))
        HTML(paste0(tmp,"<br>"))
        # HTML(paste(pot_aliases_ls[[i]][["author_name"]],"<br>",
        #            paste(pot_aliases_ls[[i]][["org_unit"]],collapse = " - "),"<br>",
        #            paste(pot_aliases_ls[[i]][["fac"]],collapse = " - ")))
      })
      # d$pot_aliases_ls <- pot_aliases_ls
      if(length(pot_aliases_ls)==0){
        updateCheckboxGroupInput(session,"aliases_selected",label = "Found authors, please select entries", 
                                 choices = character(0))
      } else {
        updateCheckboxGroupInput(session,"aliases_selected",label = "Found authors, please select entries", 
                                 choiceNames = pot_aliases_ls_text, 
                                 choiceValues = unlist(pot_alias_names_ls))
      }
    }
  )
}


#' Server module for 'alias' to show or hide input
#'
#' @param id for namespace
#' @param author_search 
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
alias_selected_show_Server <- function(id,author_search) {
  moduleServer(
    id,
    function(input, output, session) {
      if (any(!is.null(author_search))){
        shinyjs::show(id="aliases_selected")
      } else {
        shinyjs::hide(id="aliases_selected")
      }
    }
  )
}



#' Server module for 'alias' to return orcid and selected aliases
#'
#' @param id for namespace
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
alias_selected_orcid_auth_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      reactive({
        orcid_ind <- str_which(input$aliases_selected,"([:alnum:]{4}-){3}[:alnum:]{4}")
        if(length(orcid_ind)>=1){
          orcid <- str_extract_all(input$aliases_selected[orcid_ind][1],"([:alnum:]{4}-){3}[:alnum:]{4}")
          author_vec <- input$aliases_selected
        } else{
          orcid <- NULL
          author_vec <- input$aliases_selected
        }
        return(list(orcid=orcid,author_vec=author_vec))
      })
      
    }
  )
}