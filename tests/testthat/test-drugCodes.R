test_that("getATCCodes working", {

  backends <- c("database", "arrow", "data_frame")
  for (i in seq_along(backends)) {
  cdm <- mockVocabRef(backend = backends[i])
  atcCodes <- getATCCodes(cdm, level = "ATC 1st")
  expect_true(all(atcCodes[[1]] == c(12,13)))

  atcCodes2 <- getATCCodes(cdm, level = "ATC 1st",
                           name = "ALIMENTARY TRACT AND METABOLISM")
  expect_true(all(atcCodes2[[1]] == c(12,13)))

  if (backends[[i]] == "database") {
    DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
  }

  }

})

test_that("getATCCodes expected errors", {

  backends <- c("database", "arrow", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getATCCodes(cdm, level = "Not an ATC level"))
    expect_error(getATCCodes(cdm, level = "ATC 1st",
                             name = "Not an ATC name"))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }

})

test_that("getDrugIngredientCodes working", {

  backends <- c("database", "arrow", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    ing_codes <- getDrugIngredientCodes(cdm)
    expect_true(all(ing_codes[[1]] == c(10,13)))

    ing_codes2 <- getDrugIngredientCodes(cdm, name = "Adalimumab")
    expect_true(all(ing_codes2[[1]] == c(10,13)))

    ing_codes3 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injectable")
    expect_true(all(ing_codes3[[1]] == c(13)))

    ing_codes4 <- getDrugIngredientCodes(cdm,
                                         name = "Adalimumab",
                                         doseForm = "injection")
    expect_true(all(ing_codes4[[1]] == c(10)))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }

  }
})

test_that("getDrugIngredientCodes expected errors", {

  backends <- c("database", "arrow", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backend = backends[i])
    expect_error(getDrugIngredientCodes(cdm, name = "Not an Ingredient"))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }

})

