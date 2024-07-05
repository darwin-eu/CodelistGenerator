test_that("getRoutes works with categories", {

  # none in our mock cdm
  cdm <- mockVocabRef()
  expect_true(length(getRoutes(cdm, category = FALSE)) ==0 )
  expect_true(length(getRoutes(cdm, category = TRUE)) ==0 )

})
