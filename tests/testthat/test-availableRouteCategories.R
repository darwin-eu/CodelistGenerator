test_that("availableRouteCategories works with categories", {
  skip_on_cran()
  # none in our mock cdm
  cdm <- mockVocabRef()

  codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                  dplyr::filter(concept_id %in% c(10L, 15L, 24L)) |>
                                  dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                  dplyr::mutate("ancestor_concept_id" = 999L) |>
                                  dplyr::collect()) |>
    omopgenerics::newCodelistWithDetails()

  codelist <- list("first_codelist" = c(1L, 20L), "second_codelist" = c(7L, 9L)) |>
    omopgenerics::newCodelist()

  # Whole db
  expect_no_error(x <- availableRouteCategories(cdm))
  expect_true(inherits(x, "character"))
  expect_true(all(c("injectable", "topical", "transmucosal_nasal") == x))

  # Codelist with details
  expect_no_error(x <- associatedRouteCategories(cdm, x = codelist_with_details))
  expect_true(names(x) == "codes")
  expect_identical(x$codes,c("injectable", "unclassified_route_category"))

  # Codelist
  expect_no_error(x <- associatedRouteCategories(cdm, x = codelist))
  expect_identical(x$first_codelist, c("transmucosal_nasal", "unclassified_route_category"))
  expect_identical(x$second_codelist, c("unclassified_route_category"))

  # Expected errors
  expect_error(availableRouteCategories("a"))
  expect_error(availableRouteCategories(cdm, x = 1243))
  expect_error(availableRouteCategories(cdm, x = list("codes" = 10L)))

  expect_error(associatedRouteCategories("a"))
  expect_error(associatedRouteCategories(cdm))
  expect_error(associatedRouteCategories(cdm, x = list("codes" = 10L)))
  expect_error(associatedRouteCategories(cdm, x, "maybe"))
})
