skip_on_ci()
url_pass <- "https://www.nature.com/articles/nprot.2013.099.pdf?origin=ppub"
url_error <- "ölkj"
url_no_doi <- "google.com"

test_that("metadata_from_url works",{
  expect_true(dim(metadata_from_url(url_pass))[1] == 1)
  expect_true(dim(metadata_from_url(url_no_doi))[1] == 1)
  expect_true(dim(metadata_from_url(url_error))[1] == 0)
  expect_true(dim(metadata_from_url(url_pass,"translation-server"))[1] == 1)
  suppressWarnings(expect_true(dim(metadata_from_url(url_pass,"asdfasdf"))[1] == 0))
})


test_that("doi_from_metadata works",{
  expect_equal(dim(doi_from_metadata(metadata_from_url(url_pass))),c(1,1))
  expect_equal(dim(doi_from_metadata(metadata_from_url(url_no_doi))),c(0,1))
  expect_equal(dim(doi_from_metadata(metadata_from_url(url_error))),c(0,1))
})

test_that("get_urls_from_scholar_cid works",{
  cid <- "7747102342361266349"
  first_page <- get_urls_from_scholar_cid(cid)
  all_pages <- get_urls_from_scholar_cid(cid, all = TRUE)
  expect_type(first_page,"character")
  expect_type(all_pages,"character")
  
  cid <- "7747102342361266"
  expect_error(get_urls_from_scholar_cid(cid))
  expect_error(get_urls_from_scholar_cid(cid, all = TRUE))
})


test_that("get_urls_from_scholar_cid works",{
  cid <- "7747102342361266349"
  doi_out <- doi_from_cid(cid)
  expect_s3_class(doi_out, "tbl_df")
  expect_equal(names(doi_out), "DOI")

  cid <- "7747102342361266"
  expect_error(doi_from_cid(cid))
})