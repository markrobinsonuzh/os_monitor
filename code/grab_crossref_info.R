

library(rjson)
library(XML)
library(dplyr)

# parse JSONs from crossref

ddir <- "~/Downloads/crossref/"

fs <- dir(ddir, pattern = ".json.gz$")

first_na <- function(x, subset=1) {
  x <- x[subset]
  ifelse(is.null(x) | is.list(x), NA, x)
}



u <- fs[1]

grab_crossref_info <- function(u) {
  this <- fromJSON(file = gzfile(file.path(ddir,u)))$items
  
  tbl_dois <- data.frame(doi=sapply(this, function(v) first_na(v$DOI)),
                         issn=sapply(this, function(v) first_na(v$ISSN)),
                         journal=sapply(this, function(v) first_na(v$`container-title)),
                         publisher=sapply(this, function(v) first_na(v$publisher)),
                         stringsAsFactors = FALSE)
  tbl_dois
}

fs_s <- split(fs, substr(fs,1,3))

tbls <- lapply(fs_s, function(z) {
  z <- z[seq_len(min(2,length(z)))]
  cat(z,"\n")
  bind_rows(lapply(z,grab_crossref_info))
})


saveRDS(tbls, file="tbls.rds")
