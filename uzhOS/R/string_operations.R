#' possible alias author
#'
#' @param author_string name of author
#'
#' @return
#' @export
#'
#' @examples
possible_alias_author <- function(author_string){
  middle_single_alpha <- stringr::str_locate_all(author_string,"([:space:]+[:alpha:]{1}(?=[:space:]+)){1}")[[1]]
  middle_single_alpha
  end_single_alpha <- stringr::str_locate_all(author_string,"([:space:]+[:alpha:]{1}$){1}")
  # elem <- middle_single_alpha[[1]]
  possible_alias <- lapply(seq_len(dim(middle_single_alpha)[1]), function(i){
    tmp <- str_c(str_sub(author_string,start=0,end=middle_single_alpha[i,1]-1), str_sub(author_string,start=middle_single_alpha[i,2]+1)) %>% 
      str_trim()
    ifelse(str_count(tmp,"[:alpha:]+")>1,tmp,NA)
  })
  tmp <- str_sub(author_string,start=0,end=end_single_alpha[[1]][1]-1) %>% 
    str_trim()
  possible_alias[[length(possible_alias)+1]] <- ifelse(str_count(tmp,"[:alpha:]+")>1,tmp,NA)
  possible_alias[!is.na(possible_alias)]
}




pot_alias_and_affil <- function(author_string,unique_authorkeys, unique_authorkeys_no_digits,tbl_subjects,tbl_authorkeys,tbl_eprints){
  ind_auth <- which(unique_authorkeys_no_digits$author==author_string)
  ind_pot <- lapply(possible_alias_author(author_string), function(auth){
    which(unique_authorkeys_no_digits$author==auth)
  })
  tpmind <- ind_pot[lapply(ind_pot,length)>0]
  if (length(tpmind)==0){
    pot_aliases <- unique_authorkeys[ind_auth]
  } else {
    pot_aliases <- c(unique_authorkeys[ind_auth],unique_authorkeys[tpmind[[1]]])
  }
  pot_affil <- lapply(pot_aliases, function(pot_alias){
    org_unit_fac(pot_alias,"",tbl_subjects,tbl_authorkeys,tbl_eprints)
  })
  names(pot_affil) <- pot_aliases
  return(list(pot_aliases=pot_aliases,pot_affil=pot_affil))
}
