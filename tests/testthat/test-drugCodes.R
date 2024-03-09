test_that("getATCCodes working", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
  cdm <- mockVocabRef(backend = backends[i])
  atcCodes <- getATCCodes(cdm, level = "ATC 1st")
  expect_true("codelist" %in% class(atcCodes))
  expect_true(all(atcCodes[[1]] == c(12,13)))
  expect_true(c("alimentary_tract_and_metabolism") %in%
                names(atcCodes))

  atcCodes2 <- getATCCodes(cdm, level = "ATC 1st",
                           name = "ALIMENTARY TRACT AND METABOLISM")
  expect_true(all(atcCodes2[[1]] == c(12,13)))

  atcCodes3 <- getATCCodes(cdm, level = "ATC 1st",
                           name = "ALIMENTARY TRACT AND METABOLISM",
                           withConceptDetails = TRUE)
  expect_true(!is.null(atcCodes3[[1]]$concept_name))


  if (backends[[i]] == "database") {
    CDMConnector::cdm_disconnect(cdm)
  }

  }

})

test_that("getATCCodes expected errors", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getATCCodes(cdm, level = "Not an ATC level"))
    expect_error(getATCCodes(cdm, level = "ATC 1st",
                             name = "Not an ATC name"))

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }

})

test_that("getDrugIngredientCodes working", {

  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    ing_codes <- getDrugIngredientCodes(cdm)
    expect_true(all(ing_codes[[1]] == c(10,13)))

    ing_codes2 <- getDrugIngredientCodes(cdm, name = "Adalimumab")
    expect_true(all(ing_codes2[[1]] == c(10,13)))
    expect_true(names(ing_codes2) == "adalimumab")

    ing_codes3 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injectable")
    expect_true(all(ing_codes3[[1]] == c(13)))

    ing_codes4 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection")
    expect_true(all(ing_codes4[[1]] == c(10)))

    ing_codes5 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection",
                                         withConceptDetails = TRUE)
    expect_true(!is.null(ing_codes5[[1]]$concept_name))


    # limiting on ingredients
    ing_codes_all <- getDrugIngredientCodes(cdm,
                                        ingredientRange = c(1,Inf))
    ing_codes_mono <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(1,1))
    ing_codes_comb <- getDrugIngredientCodes(cdm,
                                             ingredientRange = c(2,Inf))

    expect_equal(ing_codes_all, ing_codes)
    expect_true(all(c(10) %in% ing_codes_mono$adalimumab))
    expect_null(ing_codes_mono$other_ingredient)
    expect_true(all(c(13) %in% ing_codes_comb$adalimumab))
    expect_true(all(c(13) %in% ing_codes_comb$other_ingredient))

    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3,2)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(3)))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c("a", "b")))
    expect_error(getDrugIngredientCodes(cdm,
                                        ingredientRange = c(-1, 25)))




    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }

  }
})

test_that("getDrugIngredientCodes expected errors", {

  backends <- c("database","data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getDrugIngredientCodes(cdm, name = "Not an Ingredient"))

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }

})

