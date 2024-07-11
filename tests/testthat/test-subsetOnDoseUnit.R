test_that("subsetOnDoseUnit in mock", {
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units in the mock
  expect_no_error(subsetOnDoseUnit(x = ing, cdm = cdm,
                                   doseUnit = "milligram"))


  # expected errors
  expect_error(subsetOnDoseUnit(x = ing, cdm = cdm,
                                   doseUnit = 1))
  expect_error(subsetOnDoseUnit(x = ing, cdm = "a",
                                doseUnit = "milligram"))
  expect_error(subsetOnDoseUnit(x = "a", cdm = "a",
                                doseUnit = "milligram"))

  })
