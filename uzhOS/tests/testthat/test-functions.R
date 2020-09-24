setwd("../uzhOS/tests/testthat/")
Sys.setenv(ORCID_TOKEN="8268867c-bf2c-4841-ab9c-bfeddd582a9c")

con <- DBI::dbConnect(RPostgres::Postgres(),
                 dbname = 'oa',
                 host = 'db',
                 port = 5432, 
                 user = 'shiny',
                 password = 'flora',
                 options="-c search_path=oa")


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
  


