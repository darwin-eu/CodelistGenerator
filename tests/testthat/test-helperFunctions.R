test_that("input validation", {
  skip_on_cran()
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
      availableATC(cdm, level = c("ATC 2nd"))
    )
    expect_error(
      availableATC(cdm, level = c("ATC 3nd"))
    )
    expect_no_error(
      availableICD10(cdm)
    )
    expect_no_error(
      availableICD10(cdm, level = "ICD10 Chapter")
    )
    expect_error(
      availableICD10(cdm, level = "ICD10 Chapters")
    )
  }
})

test_that("available ingredients", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    expect_no_error(
      res <- availableIngredients(cdm)
    )
    manual_res <- cdm$concept |>
      dplyr::filter(standard_concept == "S" & concept_class_id == "Ingredient") |>
      dplyr::pull("concept_name")
    expect_true(setequal(res, manual_res))
  }
})

test_that("available ATC", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    expect_no_error(
      res1 <- availableATC(cdm, level = c("ATC 1st", "ATC 2nd"))
    )
    expect_no_error(
      res2 <- availableATC(cdm, level = c("ATC 1st"))
    )
    expect_true(setequal(intersect(res2, res1), res2)) # res 2 is a subset of res 1
  }
})

test_that("available ICD10", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    expect_no_error(
      res1 <- availableICD10(cdm)
    )
    expect_no_error(
      res2 <- availableICD10(cdm, level = c("ICD10 Chapter"))
    )
    expect_true(setequal(intersect(res2, res1), res2)) # res 2 is a subset of res 1
  }
})
