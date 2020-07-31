#' possible alias author
#'
#' @param author_string name of author
#'
#' @return list with potential aliases
#' @export
#' @importFrom magrittr %>% 
#'
#' @examples
#' possible_alias_author("muster m m")
possible_alias_author <- function(author_string){
  middle_single_alpha <- stringr::str_locate_all(author_string,"([:space:]+[:alpha:]{1}(?=[:space:]+)){1}")[[1]]
  middle_single_alpha
  end_single_alpha <- stringr::str_locate_all(author_string,"([:space:]+[:alpha:]{1}$){1}")
  # elem <- middle_single_alpha[[1]]
  possible_alias <- lapply(seq_len(dim(middle_single_alpha)[1]), function(i){
    tmp <- stringr::str_c(stringr::str_sub(author_string,start=0,end=middle_single_alpha[i,1]-1), stringr::str_sub(author_string,start=middle_single_alpha[i,2]+1)) %>% 
      stringr::str_trim()
    ifelse(stringr::str_count(tmp,"[:alpha:]+")>1,tmp,NA)
  })
  tmp <- stringr::str_sub(author_string,start=0,end=end_single_alpha[[1]][1]-1) %>% 
    stringr::str_trim()
  possible_alias[[length(possible_alias)+1]] <- ifelse(stringr::str_count(tmp,"[:alpha:]+")>1,tmp,NA)
  unique(possible_alias[!is.na(possible_alias)])
}






