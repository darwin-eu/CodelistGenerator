test_that("get dose unit", {

  # none in our mock
  cdm <- mockVocabRef()
  getDoseUnit(cdm)

  # expected error
  expect_error(getDoseUnit("a"))


})
