test_that("getRoutes works with categories", {
  # none in our mock cdm
  cdm <- mockVocabRef()
  expect_no_error(getRouteCategories(cdm))
  expect_equal(getRouteCategories(cdm), c("topical","transmucosal_nasal","unclassified_route"))
})
