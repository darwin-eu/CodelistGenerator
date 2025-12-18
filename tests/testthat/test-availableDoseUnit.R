test_that("available dose unit", {
  skip_on_cran()
  # none in our mock
  cdm <- mockVocabRef()
  namesInitial <- names(cdm)

  expect_no_error(x <- availableDoseUnits(cdm))
  expect_equal(c("milligram", "percent"), x)
  expect_true(all(names(cdm) == namesInitial))
  expect_no_error(x <- availableDoseUnits(cdm, standardConcept = "Non-standard"))
  expect_true(length(x) == 0)
  expect_no_error(r <- associatedDoseUnits(cdm, x = omopgenerics::newCodelist(list("codes1" = c(21L),
                                                                         "codes2" = c(22L)))))
  expect_equal(names(r), c("codes1", "codes2"))
  expect_true(r$codes1 == "milligram")
  expect_true(length(r$codes2) == 0)
  expect_true(all(names(cdm) == namesInitial))

  x <- list("codes" = dplyr::tibble("concept_id" = 21L,
                                    "concept_name" = "nitrogen Topical Liquefied Gas",
                                    "domain_id" = "Drug",
                                    "vocabulary_id" = "RxNorm",
                                    "ancestor_concept_id" = 999L)) |>
    omopgenerics::newCodelistWithDetails()

  expect_no_error(x <- associatedDoseUnits(cdm, x = x))
  expect_true(x$codes == "milligram")

  # expected errors
  expect_error(availableDoseUnits("a"))
  expect_error(availableDoseUnits(cdm, x = 123))

  expect_error(associatedDoseUnits("a"))
  expect_error(associatedDoseUnits(cdm, x = 123))
  expect_error(associatedDoseUnits(cdm, list("codes" = c(21L))))
  expect_error(associatedDoseUnits(cdm, x, standardConcept = "maybe"))

})
