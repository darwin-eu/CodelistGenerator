test_that("stratifyByDoseUnit in mock", {
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units in the mock
  expect_no_error(x <- stratifyByDoseUnit(x = ing, cdm = cdm))
  expect_identical(names(x),
                   c("1234_adalimumab_unclassified_dose_unit",
                     "1234_other_ingredient_unclassified_dose_unit"))

  # if concepts are not from the drug domain we should get only unclassified
  oa <- getCandidateCodes(cdm = cdm, "osteoarthritis")
  oa_str <- stratifyByDoseUnit(omopgenerics::newCodelist(list(oa = oa$concept_id)),
                               cdm, keepOriginal = FALSE)
  expect_true(names(oa_str)=="oa_unclassified_dose_unit")
  expect_true("codelist" %in% class(oa_str))

  oa_str <- stratifyByDoseUnit(omopgenerics::newCodelistWithDetails(list(oa = oa)),
                               cdm, keepOriginal = FALSE)
  expect_true(names(oa_str)=="oa_unclassified_dose_unit")
  expect_true(inherits(oa_str,"codelist_with_details"))

  # expected errors
  expect_error(stratifyByDoseUnit(x = ing, cdm = "a"))
  expect_error(stratifyByDoseUnit(x = "a", cdm = cdm))
  expect_error(stratifyByDoseUnit(x = list("a" = 1L), cdm = cdm))
  expect_error(stratifyByDoseUnit(x = ing, cdm = cdm, keepOriginal = "a"))

})
