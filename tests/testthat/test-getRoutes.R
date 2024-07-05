test_that("getRoutes works with categories", {

  # none in our mock cdm
  cdm <- mockVocabRef()
  expect_no_error(getRoutes(cdm, category = FALSE))
  expect_no_error(getRoutes(cdm, category = TRUE))

})
