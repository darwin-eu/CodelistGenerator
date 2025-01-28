test_that("subset on route category", {

  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    ing_codes <- getDrugIngredientCodes(cdm)

  ing_codes_sub <- subsetOnRouteCategory(ing_codes, cdm, "oral")
  # all will have been empty and dropped
  expect_true(length(ing_codes_sub) == 0)

  ing_codes_sub <- subsetOnRouteCategory(ing_codes,  cdm, "oral")

  ing_codes_sub2 <- subsetOnRouteCategory(ing_codes,  cdm, "unclassified_route")
  expect_identical(ing_codes, ing_codes_sub2)

  # expected errors
  expect_error(subsetOnRouteCategory("a",  cdm, "oral"))
  expect_error(subsetOnRouteCategory(ing_codes,  "a", "oral"))
  expect_error(subsetOnRouteCategory(ing_codes,  cdm, 1234))

  if (backends[[i]] == "database") {
    CDMConnector::cdmDisconnect(cdm)
  }
  }

  })

