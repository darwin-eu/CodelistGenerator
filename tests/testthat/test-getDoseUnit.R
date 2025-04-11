test_that("get dose unit", {

  # none in our mock
  cdm <- mockVocabRef()
  expect_no_error(getDoseUnit(cdm))

  # expected error
  expect_error(getDoseUnit("a"))

  cdm <- omock::mockPerson(seed = 1) |>
    omock::mockObservationPeriod(seed = 1) |>
    omock::mockConcepts(conceptSet = 1, seed = 1) |>
    omock::mockVocabularyTables()

  expect_identical(c("milligram", "percent"), getDoseUnit(cdm))

})
