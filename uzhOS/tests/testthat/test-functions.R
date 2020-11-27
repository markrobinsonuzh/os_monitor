# setwd("../uzhOS/tests/testthat/")
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")

con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")

# con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
# testfun <- function(con){
#   tbl(con,"unpaywall") %>% filter(doi=="10.1038/2211089b0") %>% collect()
# }
# 
# start_db_capturing()
# con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
# # create_tbl_author(author_vec, con, fac_vec=NULL, dep_vec=NULL)
# testfun(con)
# stop_db_capturing()
# 
# with_mock_db(
#   test_that("bla",{
#     con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
#     out <- testfun(con)
#     expect_equal(out$doi,"10.1038/2211089b0")
#     # create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec=NULL, dep_vec=NULL)
#     # expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
#   })
# )


author_vec <- c("robinson mark d","robinson mark d (orcid: 0000-0002-3048-5518)")
pri_author <- "robinson m 0000 0002 3048 5518"
sec_author <- "robinson m d"
fac_vec <- "04 Faculty of Medicine"
dep_vec <- "Institute of Molecular Life Sciences"


test_that("create_tbl_author correct",{
  names_tbl_author <- c("eprintid","authorkey","authorname",
                        "authorkey_fullname","date","doi","title", 
                        "type","refereed","institution","oa_status",
                        "published_doc","subjects","name","parent","parent_name","year" )
  create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec=NULL, dep_vec=NULL)
  expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
  create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec=fac_vec, dep_vec=NULL)
  expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
  create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec=fac_vec, dep_vec=dep_vec)
  expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
  create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec=NULL, dep_vec=dep_vec)
  expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
  create_tbl_author_out <- create_tbl_author(author_vec, con, fac_vec="fac_vec", dep_vec="dep_vec")
  expect_equal(sort(names(create_tbl_author_out)),sort(names_tbl_author))
})

test_that("org_unit_fac correct",{
  tmptbl <- org_unit_fac(author_vec,con)
  expect_equal(names(tmptbl),c("org_unit","fac","type","author_name"))
  expect_equal(tmptbl$author_name,author_vec)
  expect_true(is(tmptbl$org_unit,"data.frame"))
  expect_equal(names(tmptbl$org_unit),c("dept","count"))
  expect_equal(names(tmptbl$type),c("type","count"))
  expect_true(is(tmptbl$fac,"data.frame"))
  expect_equal(names(tmptbl$fac),c("fac","count"))
})


test_that("create_zora correct",{
  col_names <- c("eprintid","authorkey","authorname", 
                 "authorkey_fullname","date","doi","title", 
                 "type","refereed","institution","oa_status","published_doc","year","in_zora")
  # tbl_author <- create_tbl_author(author_vec, con,fac_vec=NULL, dep_vec=NULL)
  create_zora_out <- create_zora(author_vec, con)
  expect_true(is(create_zora_out,"data.frame"))
  expect_equal(sort(names(create_zora_out)), sort(col_names))
  create_zora_out <- create_zora("", con)
  expect_equal(sort(names(create_zora_out)), sort(col_names))
  expect_true(is(create_zora_out,"data.frame"))
})

test_that("oadoi_fetch_local correct",{
  oaf <- oadoi_fetch_local("10.1177/000271625529700159",con)
  expect_equal(names(oaf),c("doi","oa_status"))
  expect_equal(dim(oaf),c(1,2))  
  oaf <- oadoi_fetch_local("asdfasdf",con)
  expect_equal(names(oaf),c("doi","oa_status"))
  expect_equal(dim(oaf),c(0,2))  
  oaf <- oadoi_fetch_local(c("10.1177/000271625529700159","10.1177/000271625529700159"),con)
  expect_equal(names(oaf),c("doi","oa_status"))
  expect_equal(dim(oaf),c(1,2))
  oaf <- oadoi_fetch_local(c("10.1177/000271625529700159","asdfasdf"),con)
  expect_equal(names(oaf),c("doi","oa_status"))
  expect_equal(dim(oaf),c(1,2))
})
  

# test_that("create_combined_data compute all needed dfs",{
df_zora <- create_zora(author_vec, con)
df_orcid <- retrieve_from_orcid("0000-0002-3048-5518")
search_string <- pubmed_search_string_from_zora_id(author_vec,con)
df_pubmed <- retrieve_from_pubmed(search_string)
df_publons <- retrieve_from_publons("0000-0002-3048-5518")
# })

all_combs <- expand.grid(df_orcid=c(TRUE,FALSE),
            df_zora=c(TRUE,FALSE),
            df_publons=c(TRUE,FALSE),
            df_pubmed=c(TRUE,FALSE))

test_comb <- function(df_orcid,df_zora,df_publons,df_pubmed,title=""){
  test_that(paste("create_combined_data for: ",title),{
    tbl_merge <- suppressWarnings(create_combined_data(df_orcid,df_pubmed,df_zora,df_publons,con))
    is(tbl_merge,"data.frame")
    expect_true("doi" %in% names(tbl_merge))
    expect_true("overall_oa" %in% names(tbl_merge))
    expect_true("title" %in% names(tbl_merge))
    expect_true("year" %in% names(tbl_merge))
  })
}

for(i in seq_along(all_combs[,1])){
  comb_vec <- all_combs[i,]
  args_list <- list()
  if(comb_vec$df_orcid){
    args_list[["df_orcid"]] <- df_orcid
  } else {
    args_list[["df_orcid"]] <- slice(df_orcid,0)
  }
  if(comb_vec$df_zora){
    args_list[["df_zora"]] <- df_zora
  }else {
    args_list[["df_zora"]] <- slice(df_zora,0)
  }
  if(comb_vec$df_publons){
    args_list[["df_publons"]] <- df_publons
  }else {
    args_list[["df_publons"]] <- slice(df_publons,0)
  }
  if(comb_vec$df_pubmed){
    args_list[["df_pubmed"]] <- df_pubmed
  }else {
    args_list[["df_pubmed"]] <- slice(df_pubmed,0)
  }
  args_list[["title"]] <- paste0(names(comb_vec)[unlist(comb_vec)],collapse = ",")
  suppressWarnings(do.call(test_comb, args = args_list))
}




tbl_merge <- create_combined_data(df_orcid,df_pubmed,df_zora,df_publons,con)
test_that("df_scholar_matching correct",{
  df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
  
  df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_rcrossref=FALSE)
  expect_equal(dim(df_scholar)[1],dim(df_scholar_matched)[1])
  expect_equal(dim(df_scholar)[2],dim(df_scholar_matched)[2]-1)
  expect_true(all(names(df_scholar) %in% names(df_scholar_matched)))
  expect_true("doi" %in% names(df_scholar_matched))
  
  # df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_rcrossref=TRUE)
  # expect_equal(dim(df_scholar)[1],dim(df_scholar_matched)[1])
  # expect_equal(dim(df_scholar)[2],dim(df_scholar_matched)[2]-1)
  # expect_true(all(names(df_scholar) %in% names(df_scholar_matched)))
  # expect_true("doi" %in% names(df_scholar_matched))
  
  df_scholar <- retrieve_from_scholar("noresult")
  
  df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_rcrossref=FALSE)
  expect_equal(dim(df_scholar)[1],dim(df_scholar_matched)[1])
  expect_equal(dim(df_scholar)[2],dim(df_scholar_matched)[2]-1)
  expect_true(all(names(df_scholar) %in% names(df_scholar_matched)))
  expect_true("doi" %in% names(df_scholar_matched))
  })



