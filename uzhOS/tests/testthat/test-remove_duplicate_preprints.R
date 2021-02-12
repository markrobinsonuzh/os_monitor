testthat::skip_on_ci()

con <- odbc::dbConnect(odbc::odbc(), "PostgreSQL")
df_orcid <- retrieve_from_orcid("0000-0002-3048-5518")
df_publons <- retrieve_from_publons("0000-0002-3048-5518")
df_scholar <- retrieve_from_scholar("XPfrRQEAAAAJ")
tbl_merge <- suppressWarnings(create_combined_data(df_orcid,uzhOS::empty_pubmed(),uzhOS::empty_zora(),df_publons,con))
df_scholar_matched <- df_scholar_matching(tbl_merge,df_scholar, with_zotero = FALSE,with_rcrossref=FALSE)
tbl_merge_comb <- uzhOS::merge_scholar_into_tbl_merge(tbl_merge, df_scholar_matched)

test_that("remove_duplicate_preprints works",{
  out <- remove_duplicate_preprints(tbl_merge_comb)
  expect_lte(dim(out)[1],dim(tbl_merge_comb)[1])
  expect_equal(dim(out)[2],dim(tbl_merge_comb)[2])
  expect_equal(names(out), names(tbl_merge_comb))  
  
  out <- remove_duplicate_preprints(df_orcid)
  expect_lte(dim(out)[1],dim(df_orcid)[1])
  expect_equal(dim(out)[2],dim(df_orcid)[2])
  expect_equal(names(out), names(df_orcid))  
  
  out <- remove_duplicate_preprints(df_scholar)
  expect_lte(dim(out)[1],dim(df_scholar)[1])
  expect_equal(dim(out)[2],dim(df_scholar)[2])
  expect_equal(names(out), names(df_scholar))  
})
