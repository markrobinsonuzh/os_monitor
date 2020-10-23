testthat::test_that("check_if_likely_orcid correct",{
  expect_true(check_if_likely_orcid("0000-0002-3048-551X"))
  expect_true(check_if_likely_orcid("0000-0002-3048-5511"))
  expect_false(check_if_likely_orcid("0000-0002-3048-55X1"))
  expect_false(check_if_likely_orcid("0000 0002 3048 5511"))
})




testthat::test_that("retrieve_from_orcid correct",{
  mr_orcs <- retrieve_from_orcid("0000-0002-3048-5518")
  expect_true(all(names(mr_orcs) %in% c("title","journal","type","doi","year","in_orcid")))
  expect_equal(typeof(mr_orcs$year),"integer")
  expect_equal(typeof(mr_orcs$in_orcid),"logical")
  mr_orcs <- retrieve_from_orcid("noreport")
  expect_true(all(names(mr_orcs) %in% c("title","journal","type","doi","year","in_orcid")))
  expect_equal(typeof(mr_orcs$year),"integer")
  expect_equal(typeof(mr_orcs$in_orcid),"logical")
  mr_orcs <- retrieve_from_orcid("0000-0000-0000-0000")
  expect_true(all(names(mr_orcs) %in% c("title","journal","type","doi","year","in_orcid")))
  expect_equal(typeof(mr_orcs$year),"integer")
  expect_equal(typeof(mr_orcs$in_orcid),"logical")
})