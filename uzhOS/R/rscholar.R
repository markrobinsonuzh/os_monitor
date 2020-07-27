
#' Retrieve a table of records from Google Scholar
#'
#' @param scholar_id 
#'
#' @return
#' @export
#'
#' @examples
#' mr_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
retrieve_from_scholar <- function(scholar_id) {
  starts <- seq(0,1000,by=100)
  scholar_pubs <- lapply(starts, function(u) {
    scholar::get_publications(scholar_id, cstart = u, pagesize = 100, flush = FALSE)  
  })
  scholar_pubs <- do.call(rbind, scholar_pubs)
  scholar_pubs <- unique(scholar_pubs)
  scholar_pubs$title <- as.character(scholar_pubs$title)
  scholar_pubs$in_scholar <- TRUE
  scholar_pubs
}




#' Split title into named vector
#'
#' @param u character string of title
#'
#' @return named vector
#'
#' @examples
#' split_to_rank("Here is a title")
split_to_rank <- function(u) {
  ss <- strsplit(u, "[ -/\\]")
  ss <- lapply(ss, function(v) {
    v <- v[nchar(v)>0]
    n <- length(v)
    setNames(1:n, toupper(v))
  })
  setNames(ss, u)
}

#' Compute distance between two titles
#'
#' @param a first title
#' @param b second title
#'
#' @return
#' @export
#'
#' @examples
#' sentence_Dist("This is the first title",
#'               "This is the second title")
sentence_Dist <- function(a, b) {
  jaccard <- length(intersect(names(a),names(b))) / min(length(a),length(b),length(union(names(a),names(b))))
  n <- intersect(names(a), names(b))
  if(length(n) <= 4) return(0)
  return(cor(a[n],b[n], method = "spearman") * jaccard)
}

#' Calculate all pairwise scores for two vectors of 
#'
#' @param x first vector of titles
#' @param y second vector of titles
#'
#' @return
#' @export
#'
#' @examples
#' calcScore(c("A title","Another title"),
#'           c("This title","That title"))
calcScore <- function(x,y) {
  ss_x <- split_to_rank(x)
  ss_y <- split_to_rank(y)
  dist <- matrix(0, nrow=length(ss_x), ncol=length(ss_y))
  for(i in seq_along(ss_x))
    for(j in seq_along(ss_y))
      dist[i,j] <- sentence_Dist(ss_x[[i]], ss_y[[j]])
  list(dist=dist, rows=x, cols=y)
  # keep_x <- rowSums(dist)>0
  # keep_y <- colSums(dist)>0
  # list(dist=dist[keep_x,keep_y,drop=FALSE], 
  #      rows=x[keep_x], 
  #      cols=y[keep_y])
}

