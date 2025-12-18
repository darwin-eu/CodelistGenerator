test_that("subset on ingredient range works", {
  skip_on_cran()
  con <- DBI::dbConnect(duckdb::duckdb(),
                        CDMConnector::eunomiaDir("synpuf-1k", "5.3"))
  cdm <- CDMConnector::cdmFromCon(con = con,
                                  cdmName = "Eunomia Synpuf",
                                  cdmSchema   = "main",
                                  writeSchema = "main",
                                  achillesSchema = "main")

  n1 <- names(cdm)
  x <- omopgenerics::newCodelist(list("codes1" = c(43260756L, 44054657L,44084854L),
                                      "codes2" = c(41025222L, 41448307L,42721793L)))
  # codelist
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(5, 10),
                                                 negate = FALSE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist"))
  expect_equal(res$codes1, c(43260756L, 44054657L))
  expect_equal(res$codes2, c(41448307L))

  # same ingredient range
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(8, 8),
                                                 negate = FALSE))

  # codelist + negate argument
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(5, 10),
                                                 negate = TRUE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist"))
  expect_equal(res$codes1, c(44084854L))
  expect_equal(res$codes2, c(41025222L, 42721793L))

  # empty 1 codelist
  expect_warning(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(13, 23),
                                                 negate = FALSE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist"))
  expect_equal(names(res), "codes2")

  expect_no_error(res <- subsetOnIngredientRange(omopgenerics::emptyCodelist(),
                                                 cdm,
                                                 ingredientRange = c(13, 23),
                                                 negate = FALSE))
  expect_true(inherits(res, "codelist"))

  # empty codelist with details
  expect_no_error(res <- subsetOnIngredientRange(omopgenerics::emptyCodelistWithDetails(),
                                                 cdm,
                                                 ingredientRange = c(13, 23),
                                                 negate = FALSE))
  expect_true(inherits(res, "codelist_with_details"))

  # codelist with details
  x <- x |> asCodelistWithDetails(cdm)
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(5, 10),
                                                 negate = FALSE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist_with_details"))
  expect_equal(res$codes1,
               x$codes1 |>
                 dplyr::filter(concept_id %in% c(43260756L, 44054657L)))
  expect_equal(res$codes2,
               x$codes2 |>
                 dplyr::filter(concept_id %in% c(41448307L)))

  # all ingredeint range
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(1, Inf),
                                                 negate = FALSE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist_with_details"))
  expect_equal(length(res$codes1$concept_id),3L)
  expect_equal(length(res$codes2$concept_id),3L)

  # no drug concept
  x <- omopgenerics::newCodelist(list("a" = c(44517160L, 44084854L)))
  expect_no_error(res <- subsetOnIngredientRange(x,
                                                 cdm,
                                                 ingredientRange = c(1, Inf),
                                                 negate = FALSE))
  expect_equal(n1, names(cdm))
  expect_true(inherits(res, "codelist"))
  expect_equal(res$a,44084854L)

  # check empty codelists
  x <- subsetOnIngredientRange(omopgenerics::emptyCodelist(), cdm, ingredientRange = c(1, Inf))
  expect_true(inherits(x, "codelist"))
  x <- subsetOnIngredientRange(omopgenerics::emptyCodelistWithDetails(), cdm, ingredientRange = c(1, Inf))
  expect_true(inherits(x, "codelist_with_details"))

  # expected errors:
  expect_error(subsetOnIngredientRange(list("a" = 1L), cdm, ingredientRange = c(5,10)))
  expect_error(subsetOnIngredientRange(x, "a", ingredientRange = c(5,10)))
  expect_error(subsetOnIngredientRange(x, cdm, ingredientRange = c("hola")))
  expect_error(subsetOnIngredientRange(x, cdm, ingredientRange = c(10,5)))

})

