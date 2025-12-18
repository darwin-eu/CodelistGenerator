test_that("subsetOnDoseUnit in mock", {
  skip_on_cran()
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units - empty codelist
  expect_warning(x <- subsetOnDoseUnit(x = ing, cdm = cdm,
                                        doseUnit = "hello"))
  expect_true(inherits(x, "codelist"))
  expect_true(length(x) == 0)

  # argument NEGATE
  codes <- omopgenerics::newCodelist(list("codes" = c(20L,21L)))
  codes1 <- subsetOnDoseUnit(x = codes, cdm = cdm, doseUnit = c("milligram"))
  codes2 <- subsetOnDoseUnit(x = codes, cdm = cdm, doseUnit = c("percent"), negate = TRUE)
  expect_identical(codes1, codes2)

  # lower case
  codes3 <- subsetOnDoseUnit(x = codes, cdm = cdm, doseUnit = c("Milligram"))
  expect_identical(codes1, codes3)

  # Check if dose = NA are included
  codes1 <- subsetOnDoseUnit(x = omopgenerics::newCodelist(list("codes" = c(10L,21L))), cdm = cdm, doseUnit = c("milligram"))
  expect_identical(codes1$codes, 21L)

  # Check codelist with details
  codes_wd <- codes |> asCodelistWithDetails(cdm)
  codes1 <- subsetOnDoseUnit(x = codes_wd, cdm, doseUnit = "milligram")
  expect_true(inherits(codes1, "codelist_with_details"))
  expect_true(!"codelist" %in% class(codes1))
  expect_identical(codes1$codes,
                   dplyr::tibble(
                     "concept_id" = 21L,
                     "concept_name" = "nitrogen Topical Liquefied Gas",
                     "domain_id" = "Drug",
                     "vocabulary_id" = "RxNorm",
                     "standard_concept" = "standard"
                   ))


  # check empty codelists
  expect_warning(x <- subsetOnDoseUnit(omopgenerics::emptyCodelist(), cdm,
                                       doseUnit = "milligram"))
  expect_true(inherits(x, "codelist"))
  expect_warning(x <- subsetOnDoseUnit(omopgenerics::emptyCodelistWithDetails(),
                                       cdm, doseUnit = "milligram"))
  expect_true(inherits(x, "codelist_with_details"))

  # expected errors
  expect_error(subsetOnDoseUnit(x = ing, cdm = cdm,
                                doseUnit = 1))
  expect_error(subsetOnDoseUnit(x = ing, cdm = "a",
                                doseUnit = "milligram"))
  expect_error(subsetOnDoseUnit(x = list("a" = 1L), cdm = cdm,
                                doseUnit = "milligram"))

})
