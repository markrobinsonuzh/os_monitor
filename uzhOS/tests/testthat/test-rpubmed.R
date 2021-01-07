# testing pubmed_search_string_from_zora_id
con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
author_vec <- "robinson mark d (orcid: 0000-0002-3048-5518)"
test_that("pubmed_search_string_from_zora_id correct",{
  expect_equal(pubmed_search_string_from_zora_id(author_vec,con),
  "(robinson mark d[au] or robinson m[au] or robinson md[au]) AND (2001:2021[pdat]) AND (zurich[affiliation])")
  expect_equal(pubmed_search_string_from_zora_id(author_vec,con,cutoff_year = 2019),
               "(robinson mark d[au] or robinson m[au] or robinson md[au]) AND (2019:2021[pdat]) AND (zurich[affiliation])")
  expect_equal(pubmed_search_string_from_zora_id(author_vec,con,cutoff_year = 2019, orcid = "0000-0002-3048-5518"),
               "(robinson mark d[au] or robinson m[au] or robinson md[au]) AND (2019:2021[pdat]) AND (zurich[affiliation]) OR (orcid 0000-0002-3048-5518 [auid])")
  expect_equal(pubmed_search_string_from_zora_id("",con),"(max muster[au] or max m[au] or Max M[au]) AND (2001:2021[pdat]) AND (zurich[affiliation])")
})

test_that("pubmed_search_string_from_zora_id correct",{
  search_string <- pubmed_search_string_from_zora_id(author_vec,con)
  out <- retrieve_from_pubmed(search_string)
  expect_true(is(out,"data.frame"))
  expect_true(all(names(out) %in% c("pubyear","title","authors","journal","doi","pmid","in_pubmed")))
  expect_equal(typeof(out$pubyear),"integer")
  expect_equal(typeof(out$in_pubmed),"logical")
  out <- retrieve_from_pubmed("noresult")
  expect_true(all(names(out) %in% c("pubyear","title","authors","journal","doi","pmid","in_pubmed")))
  expect_equal(typeof(out$pubyear),"integer")
  expect_equal(typeof(out$in_pubmed),"logical")
})



row_names_out <- c("doi","pmid" )
dois <- c("10.1186/1471-2105-3-35","10.1128/MCB.24.12.5534-5547.2004")
test_that("rec_req_id_converter correct",{
  pub_df <- rec_req_id_converter(dois)
  expect_equal(dim(pub_df),c(2,2))
  expect_equal(names(pub_df), row_names_out)
  pub_df <- rec_req_id_converter(c(dois[1],"error_doi"))
  expect_equal(dim(pub_df),c(1,2))
  expect_equal(names(pub_df), row_names_out)
  pub_df <- rec_req_id_converter("error_doi")
  expect_equal(dim(pub_df),c(0,2))
  expect_equal(names(pub_df), row_names_out)
})




# testing retrieve_from_pubmed_with_doi
row_names_out <- c("doi","relative_citation_ratio", "nih_percentile","citation_count","in_pubmetric" )
dois <- c("10.1186/1471-2105-3-35","10.1128/MCB.24.12.5534-5547.2004")
test_that("retrieve_from_pubmed_with_doi correct",{
  pub_df <- retrieve_from_pubmed_with_doi(dois)
  expect_equal(dim(pub_df),c(2,5))
  expect_equal(names(pub_df), row_names_out)
  pub_df <- retrieve_from_pubmed_with_doi(c(dois[1],"error_doi"))
  expect_equal(dim(pub_df),c(1,5))
  expect_equal(names(pub_df), row_names_out)
  pub_df <- retrieve_from_pubmed_with_doi("error_doi")
  expect_equal(dim(pub_df),c(0,5))
  expect_equal(names(pub_df), row_names_out)
})
