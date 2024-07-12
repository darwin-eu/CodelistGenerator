test_that("stratifyByDoseUnit in mock", {
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units in the mock
  expect_no_error(stratifyByDoseUnit(x = ing, cdm = cdm))

  # if concepts are not from the drug domain we should get empty codelist back
  oa <- getCandidateCodes(cdm = cdm, "osteoarthritis")
  oa_str <- stratifyByDoseUnit(list(oa = oa$concept_id),
                                    cdm, keepOriginal = FALSE)
  expect_true(length(oa_str)==0)

  oa_str <- stratifyByDoseUnit(omopgenerics::newCodelistWithDetails(list(oa = oa)),
                                    cdm, keepOriginal = FALSE)
  expect_true(length(oa_str)==0)

  # expected errors
  expect_error(stratifyByDoseUnit(x = ing, cdm = "a"))
  expect_error(stratifyByDoseUnit(x = "a", cdm = cdm))

})
