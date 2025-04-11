test_that("subsetOnDoseUnit in mock", {
  cdm <- mockVocabRef()
  ing <- getDrugIngredientCodes(cdm = cdm)

  # no dose units
  expect_no_error(subsetOnDoseUnit(x = ing, cdm = cdm,
                                   doseUnit = "hello"))

  # argument NEGATE
  codes <- list("codes" = c(20,21))
  codes1 <- subsetOnDoseUnit(x = codes, cdm = cdm, doseUnit = c("milligram"))
  codes2 <- subsetOnDoseUnit(x = codes, cdm = cdm, doseUnit = c("percent"), negate = TRUE)
  expect_identical(codes1, codes2)

  # Check if dose = NA are included
  codes1 <- subsetOnDoseUnit(x = list("codes" = c(10,21)), cdm = cdm, doseUnit = c("milligram"))
  expect_identical(codes1$codes, 21L)

  # expected errors
  expect_error(subsetOnDoseUnit(x = ing, cdm = cdm,
                                   doseUnit = 1))
  expect_error(subsetOnDoseUnit(x = ing, cdm = "a",
                                doseUnit = "milligram"))
  expect_error(subsetOnDoseUnit(x = "a", cdm = "a",
                                doseUnit = "milligram"))

  })
