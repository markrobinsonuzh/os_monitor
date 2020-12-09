
test_that("Bibtex citation retrieval works",{
  doi <- "10.1177/000271625529700159"
  bibfut <- GetBibEntryWithDOI_no_temp(doi)
  temp <- tempfile()
  on.exit(unlink(temp))
  writeLines(future::value(bibfut), con = temp)
  expect_true(RefManageR::is.BibEntry(suppressWarnings(RefManageR::ReadBib(file = temp))))
  
  doi <- rep("10.1177/000271625529700159",2)
  bibfut <- GetBibEntryWithDOI_no_temp(doi)
  temp <- tempfile()
  on.exit(unlink(temp))
  writeLines(future::value(bibfut), con = temp)
  expect_true(RefManageR::is.BibEntry(suppressWarnings(RefManageR::ReadBib(file = temp))))
  
  doi <- "examplestring"
  bibfut <- GetBibEntryWithDOI_no_temp(doi)
  temp <- tempfile()
  on.exit(unlink(temp))
  suppressMessages(expect_null(future::value(bibfut)))
  })

