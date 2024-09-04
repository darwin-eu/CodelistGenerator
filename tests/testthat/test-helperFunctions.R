test_that("input validation", {
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    expect_no_error(
      availableIngredients(cdm)
    )
    expect_no_error(
      availableATC(cdm)
    )
    expect_no_error(
      availableICD10(cdm)
    )
  }
})
