## Load packages
library(rjson)
library(XML)
library(dplyr)
library(magrittr)
library(purrr)
library(tibble)
library(dplyr)
library(rlang)
library(tidyr)
# library(mongolite)
library(DBI)
library(data.tree)

# mongodb connection
# mongourl <- "mongodb://172.18.0.2/16"
# mongourl <- "mongodb://db"
# mongourl <- "mongodb://192.168.16.2/20"

con <- dbConnect(odbc::odbc(), "PostgreSQL")



## Read in ZORA JSON files
setwd("/srv/shiny-server")
# outdir <- here::here("output")
outdir <- "/srv/shiny-server/data"
fs <- dir("/srv/shiny-server/data", ".js.gz$", full.names = TRUE)

first_na <- function(x, subset=1) {
  x <- x[subset]
  ifelse(is.null(x), NA, x)
}

# pull out some data.frames for each JSON file
df_lists <- lapply(fs, function(u) {
  cat(".")
  this <- fromJSON(file = gzfile(u))
  
  eprintid <- sapply(this, function(u) first_na(u$eprintid))
  
  published_doc <- sapply(this, function(v) {
    length(v$documents)>0
  })
  
  dewey <- lapply(this, function(u) as.character(u$dewey))
  n_dewey <- sapply(dewey, length)
  
  tbl_dewey <- data.frame(eprintid=rep(eprintid, n_dewey),
                          dewey = unlist(dewey),
                          stringsAsFactors = FALSE)
  
  orcid <- lapply(this, function(u) {
    unlist(lapply(u$contributor_lookup, function(v) v$orcid))
  })
  n_orcid <- sapply(orcid, length)
  tbl_orcid <- data.frame(eprintid=rep(eprintid, n_orcid),
                          orcid=unlist(orcid),
                          stringsAsFactors = FALSE)
  
  
  subjects <- lapply(this, function(u) as.character(u$subjects))
  n_subjects <- sapply(subjects, length)
  
  tbl_subjects <- data.frame(eprintid=rep(eprintid, n_subjects),
                             subjects = unlist(subjects),
                             stringsAsFactors = FALSE)
  
  
  tbl_eprints <- data.frame(eprintid=eprintid,
                            date=sapply(this, function(u) substr(first_na(u$date),1,4)),
                            doi=sapply(this, function(u) first_na(u$doi)),
                            title=sapply(this, function(u) first_na(u$title)),
                            type=sapply(this, function(u) first_na(u$type)),
                            refereed=sapply(this, function(u) first_na(u$refereed_set)),
                            institution=sapply(this, function(u) first_na(u$institution)),
                            oa_status=sapply(this, function(u) first_na(u$oa_status)),
                            published_doc = published_doc,
                            stringsAsFactors = FALSE)
  
  list_lookup <- lapply(this, function(uu) 
    sapply(seq_along(uu$contributor_lookup), function(i) {
      tryCatch({
        data.frame(authorkey=ifelse(is.null(unlist(uu$contributor_lookup[[i]]$lookup)),NA,unlist(uu$contributor_lookup[[i]]$lookup)),
                   author_name_family=ifelse(is.null(unlist(uu$contributor_stat[[i]]$name$family)),NA,unlist(uu$contributor_stat[[i]]$name$family)),
                   author_name_given=ifelse(is.null(unlist(uu$contributor_stat[[i]]$name$given)),NA,unlist(uu$contributor_stat[[i]]$name$given)))
      },error=function(e) {
        if (is.null(uu$contributor_lookup[[i]]$lookup)){
          data.frame(authorkey=NA,
                     author_name_family=NA,
                     author_name_given=NA)
        } else{
          data.frame(
            authorkey=ifelse(is.null(unlist(uu$contributor_lookup[[i]]$lookup)),NA,unlist(uu$contributor_lookup[[i]]$lookup)),
            author_name_family=rep(NA,length(unlist(uu$contributor_lookup[[i]]$lookup))),
            author_name_given=rep(NA,length(unlist(uu$contributor_lookup[[i]]$lookup))))
        }})
    }))
  
  
  names(list_lookup) <- tbl_eprints$eprintid
  n_list <- unlist(sapply(list_lookup, function(x) ifelse(length(ncol(x))>0,ncol(x),0)))
  tbl_authorkeys <- data.frame(eprintid=rep(eprintid, n_list),
                               authorkey=unlist(lapply(list_lookup,function(u) if(length(u)==0) NULL else u[1,])),
                               author_name_family=unlist(lapply(list_lookup,function(u) if(length(u)==0) NULL else  u[2,])),
                               author_name_given=unlist(lapply(list_lookup,function(u) if(length(u)==0) NULL else  u[3,])),
                               stringsAsFactors = FALSE)
  
  tbl_authorkeys <- tbl_authorkeys %>% mutate(authorkey_fullname = tolower(paste(author_name_family,author_name_given)))
  
  list(tbl_dewey = tbl_dewey,
       tbl_subjects = tbl_subjects,
       tbl_eprints = tbl_eprints,
       tbl_orcid = tbl_orcid,
       tbl_authorkeys = tbl_authorkeys,
       list_lookup = list_lookup)
})


tbl_eprints <- do.call(rbind, lapply(df_lists, .subset2, "tbl_eprints"))
tbl_eprints <- tbl_eprints %>% dplyr::filter(!is.na(oa_status), !is.na(date))
tbl_dewey <- do.call(rbind, lapply(df_lists, .subset2, "tbl_dewey"))
tbl_subjects <- do.call(rbind, lapply(df_lists, .subset2, "tbl_subjects"))
tbl_orcid <- do.call(rbind, lapply(df_lists, .subset2, "tbl_orcid"))
tbl_authorkeys <- do.call(rbind, lapply(df_lists, .subset2, "tbl_authorkeys"))
list_lookup <- do.call(c, lapply(df_lists, .subset2, "list_lookup"))

rm(fs, first_na); gc()

# saveRDS(tbl_orcid, file.path(outdir, "tbl_orcid.rds"))
# saveRDS(tbl_authorkeys, file.path(outdir, "tbl_authorkeys.rds"))
# tbl_authorkeys <- readRDS(file.path(outdir, "tbl_authorkeys.rds"))

## subjects
sub_list <- xmlToList(paste0(outdir,"/subjects_combined_20200108.xml"))


sub_df <- data.frame(subjects=sapply(sub_list, .subset2, "subjectid"),
                     name=sapply(sub_list, function(u) u$name[[1]]$name),
                     parent=sapply(sub_list, function(u) u$parents[[1]]),
                     stringsAsFactors = FALSE)

n_parents <- map(sub_list, ~length(.x$parents)) %>% unlist()
ind_double <- which(n_parents==2)
sub_df_double <- data.frame(subjects=sapply(sub_list[ind_double], .subset2, "subjectid"),
                            name=sapply(sub_list[ind_double], function(u) u$name[[1]]$name),
                            parent=sapply(sub_list[ind_double], function(u) u$parents[[2]]),
                            stringsAsFactors = FALSE)
sub_df <- rbind(sub_df,
                sub_df_double
                )

subject_lookup <- setNames(sub_df$name, sub_df$subjects)

# table for counter for double occurrences
sub_df_double_counter <- sub_df_double %>% mutate(count=1)

# recursive lookup function
rec_lookup <- function(first_parent){
  parent <- unname(subject_lookup[first_parent])
  if(parent %in% sub_df_double_counter$name){
    parent_subject <- sub_df %>% filter(subjects %in% first_parent) %>% 
      arrange(subjects) %>% 
      slice(!!sub_df_double_counter$count[sub_df_double_counter$name==parent]) %>%
      pull(parent)
    sub_df_double_counter[sub_df_double_counter$name==parent, "count"] <<- 2
  } else {
    parent_subject <- sub_df %>% filter(subjects %in% first_parent) %>% pull(parent)
  }
  if(parent[1] == "ROOT" || is.na(parent)){
    return("rt")
  } else{
    return(c(parent,rec_lookup(parent_subject)))
  }
}

tmptree <- lapply(seq_along(sub_df$subjects), function(i){
  leaf <- sub_df$subjects[i]
  as.data.frame(t(rev(rec_lookup(leaf))))
  # rec_to_list(rev(tmp))
})
tmptree <- plyr::rbind.fill(tmptree)
tmptree$pathString <- apply(tmptree, 1, function(x) paste(trimws(na.omit(x)), collapse="/"))
print(head(tmptree))
orgtree <- data.tree::as.Node(tmptree)
Sort(orgtree,"name")
jsonorgtree <- treeToJSON(orgtree, pretty = TRUE)
saveRDS(jsonorgtree, file.path(outdir, "orgtree.rds"))


sub_df$parent_name <- subject_lookup[sub_df$parent]

tbl_subjects <- tbl_subjects %>% left_join(sub_df)
# tbl_eprints <- tbl_eprints %>% left_join(tbl_subjects)
# saveRDS(tbl_subjects, file.path(outdir, "tbl_subjects.rds"))
# tbl_subjects <- readRDS(file.path(outdir, "tbl_subjects.rds"))
# saveRDS(tbl_eprints, file.path(outdir, "tbl_eprints.rds"))
# tbl_eprints <- readRDS(file.path(outdir, "tbl_eprints.rds"))


## create unique authorkeys 
tbl_unique_authorkeys <- tbl_authorkeys %>% group_by(authorkey)  %>% 
  summarise(author_name_family_given=max(unique(paste(ifelse(is.na(author_name_family),"",author_name_family),ifelse(is.na(author_name_given),"",author_name_given)))),
            authorkey_fullname=max(unique(authorkey_fullname))) %>% 
  mutate(id=row_number(),
         authorkey_processed=authorkey)

unique_authorkeys_which <-
  stringr::str_which(tbl_unique_authorkeys$authorkey,"([:digit:]{3,4} ?)+[:alpha:]?($| )")

unique_authorkeys_sub <- tibble(id=unique_authorkeys_which,
                                authorkeys_sub_no_digits = stringr::str_replace_all(tbl_unique_authorkeys$authorkey[unique_authorkeys_which],"([:digit:]{3,4} ?)+[:alpha:]?($| )","") %>%
                                  stringr::str_trim() ,
                                authorkeys_sub_only_digits = stringr::str_extract_all(tbl_unique_authorkeys$authorkey[unique_authorkeys_which],"([:digit:]{3,4} ?)+[:alpha:]?($| )") %>%
                                  stringr::str_trim())

tbl_unique_authorkeys$authorkey_processed[unique_authorkeys_which] <- unique_authorkeys_sub$authorkeys_sub_no_digits


## create unique authorkeys fullname
tbl_unique_authorkeys_fullname <- tbl_authorkeys %>% group_by(authorkey_fullname)  %>% 
  summarise(authorkey_fullname=max(unique(authorkey_fullname)),
            authorkey=max(unique(authorkey))) %>% 
  ungroup() %>% 
  mutate(id=row_number(),
         authorname=authorkey_fullname)

unique_authorkeys_fullname_which <-
  stringr::str_which(tbl_unique_authorkeys_fullname$authorkey_fullname,"\\(orcid: ([:alnum:]{4}-?){4}\\)")

unique_authorkeys_fullname_sub <- tibble(id=unique_authorkeys_fullname_which,
                                         authorkeys_sub_no_digits = stringr::str_replace_all(tbl_unique_authorkeys_fullname$authorkey_fullname[unique_authorkeys_fullname_which],"\\(orcid: ([:alnum:]{4}-?){4}\\)","") %>%
                                           stringr::str_trim() ,
                                         authorkeys_sub_only_digits = stringr::str_extract_all(tbl_unique_authorkeys_fullname$authorkey_fullname[unique_authorkeys_fullname_which],"\\(orcid: ([:alnum:]{4}-?){4}\\)") %>%
                                           stringr::str_trim(),
                                         orcid = stringr::str_extract_all(authorkeys_sub_only_digits,"([:alnum:]{4}-?){4}") %>%
                                           stringr::str_trim())

tbl_unique_authorkeys_fullname$authorname[unique_authorkeys_fullname_which] <- unique_authorkeys_fullname_sub$authorkeys_sub_no_digits
# tbl_unique_authorkeys_fullname %>% filter(authorkey_fullname != authorname)
# save
# saveRDS(tbl_unique_authorkeys, file.path(outdir, "tbl_unique_authorkeys.rds"))
# saveRDS(tbl_unique_authorkeys_fullname, file.path(outdir, "tbl_unique_authorkeys_fullname.rds"))
# tbl_unique_authorkeys_fullname <- readRDS(file.path(outdir, "tbl_unique_authorkeys_fullname.rds"))

tbl_eprints <- tbl_eprints %>% mutate(doi=tolower(doi))
#tbl_eprints <- tbl_eprints %>% select(-c("subjects","name","parent","parent_name")) %>% unique() 

# dbExecute(con, "DELETE FROM subjects;")
# dbExecute(con, "DELETE FROM authors;")
# dbExecute(con, "DELETE FROM authorkeys;")
# dbExecute(con, "DELETE FROM eprints;")

# eprints_names <- c("eprintid","doi","date","title","type","refereed","institution","oa_status","published_doc")
# dbWriteTable(con, "eprints_tmp", tbl_eprints,)
# dbExecute(con,
#           "INSERT INTO eprints(eprintid,doi, date, title, type, refereed, institution, oa_status, published_doc) 
#           SELECT eprintid,doi, date, title, type, refereed, institution, oa_status, published_doc FROM eprints_tmp")
dbExecute(con,
          "CREATE TABLE IF NOT EXISTS oa.zora_update_date(
          is_updated BOOLEAN,
          date DATE NOT NULL DEFAULT CURRENT_DATE
          )")
tryCatch({
  dbExecute(con,
  "
  DROP TABLE authors;
  DROP TABLE subjects;
  DROP TABLE authorkeys;
  DROP TABLE eprints;
  
  CREATE TABLE authorkeys(
    authorkey_fullname TEXT,
    authorkey TEXT,
    authorname TEXT
  );
  
  CREATE TABLE eprints(
    eprintid INT,
    doi TEXT,
    date CHAR(4),
    title TEXT,
    type VARCHAR(255),
    refereed VARCHAR(12),
    institution TEXT,
    oa_status VARCHAR(16),
    published_doc BOOLEAN
    
  );
  
  CREATE TABLE subjects(
    eprintid INT,
    subjects VARCHAR(16),
    name TEXT,
    parent VARCHAR(16),
    parent_name TEXT
  );
  
  CREATE TABLE authors(
    eprintid INT NOT NULL,
    authorkey_fullname TEXT
  );
  ")
  
  dbWriteTable(con, "eprints", tbl_eprints, overwrite=FALSE, append=TRUE)
  # dbAppendTable(con, "eprints", tbl_eprints)
  #dbExecute(con, "CREATE INDEX idx_doi ON eprints(doi);")
  #dbExecute(con, "CREATE INDEX idx_eprintid ON eprints(eprintid);")
  
  
  dbWriteTable(con, "subjects", tbl_subjects, overwrite=FALSE, append=TRUE)
  # dbAppendTable(con, "subjects", tbl_subjects)
  #dbExecute(con, "CREATE INDEX idx_name ON subjects(name);")
  #dbExecute(con, "CREATE INDEX idx_parent_name ON subjects(parent_name);")
  
  tbl_unique_authorkeys_fullname <- tbl_unique_authorkeys_fullname %>% select(-id)
  
  dbWriteTable(con, "authorkeys", tbl_unique_authorkeys_fullname,overwrite=FALSE, append=TRUE)
  # dbAppendTable(con, "authorkeys", tbl_unique_authorkeys_fullname)
  
  
  tbl_authorkeys <- tbl_authorkeys %>% select(eprintid,authorkey_fullname)
  
  dbWriteTable(con, "authors", tbl_authorkeys,overwrite=FALSE, append=TRUE)
  # dbAppendTable(con, "authors", tbl_authorkeys)
  
  dbExecute(con,"
            ALTER TABLE authorkeys ADD PRIMARY KEY (authorkey_fullname);
            
            ALTER TABLE eprints ADD PRIMARY KEY (eprintid);
            
            ALTER TABLE subjects 
              ADD CONSTRAINT subj_eprints_fkey 
              FOREIGN KEY (eprintid) 
              REFERENCES eprints (eprintid);
  
            ALTER TABLE authors 
              ADD CONSTRAINT auth_authorkeys_fkey 
              FOREIGN KEY (authorkey_fullname) 
              REFERENCES authorkeys (authorkey_fullname);
              
            ALTER TABLE authors 
              ADD CONSTRAINT auth_eprints_fkey 
              FOREIGN KEY (eprintid) 
              REFERENCES eprints (eprintid);
            ")
  dbExecute(con, "INSERT INTO zora_update_date(is_updated,date) VALUES (TRUE,CURRENT_DATE)")
  
}, error= function(e){
  dbExecute(con, "INSERT INTO zora_update_date(is_updated,date) VALUES (FALSE,CURRENT_DATE)")
})
