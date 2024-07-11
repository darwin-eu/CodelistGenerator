test_that("stratifyByDoseUnit in mock", {
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units in the mock
  expect_no_error(stratifyByDoseUnit(x = ing, cdm = cdm))


  # expected errors
  expect_error(stratifyByDoseUnit(x = ing, cdm = "a"))
  expect_error(stratifyByDoseUnit(x = "a", cdm = cdm))

})
