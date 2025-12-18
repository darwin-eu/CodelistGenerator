test_that("check asCodelist", {
  a <- newCodelistWithDetails(list(asthma = dplyr::tibble(concept_id = 1L,
                                                          concept_name = "a"),
                                   copd = dplyr::tibble(concept_id = 2L,
                                                        concept_name = "b")))
  expect_no_error(asCodelist(a))
  expect_true(inherits(asCodelist(a), "codelist"))

  expect_no_error(omopgenerics::emptyCodelist() |> asCodelist())
  expect_no_error(omopgenerics::emptyConceptSetExpression() |> asCodelist())
  expect_no_error(omopgenerics::emptyCodelistWithDetails() |> asCodelist())

  a <- getCandidateCodes(mockVocabRef(), keywords = "ost")
  expect_no_error(asCodelist(a))
  expect_true(inherits(asCodelist(a), "codelist"))

})

test_that("asCodelist resolving concept set expressions", {
  cdm <- mockVocabRef()
  cse <- omopgenerics::newConceptSetExpression(
    list("a" = dplyr::tibble("concept_id" = 1L,
                             "excluded" = FALSE,
                             "descendants" = TRUE,
                             "mapped" = FALSE)))
  expect_identical(
    getDescendants(cdm, 1L) |>
    dplyr::pull("concept_id"),
  asCodelist(cse, cdm)[[1]])

  cse <- omopgenerics::newConceptSetExpression(
    list("a" = dplyr::tibble("concept_id" = c(1L, 10L),
                             "excluded" = FALSE,
                             "descendants" = TRUE,
                             "mapped" = FALSE)))
  expect_identical(
    getDescendants(cdm, c(1L, 10L)) |>
      dplyr::pull("concept_id"),
    asCodelist(cse, cdm)[[1]])

  cse <- omopgenerics::newConceptSetExpression(
    list("a" = dplyr::tibble("concept_id" = c(1L, 10L),
                             "excluded" = c(FALSE, TRUE),
                             "descendants" = c(TRUE, FALSE),
                             "mapped" = c(FALSE, FALSE))))
  expect_identical(
    getDescendants(cdm, c(1L)) |>
      dplyr::pull("concept_id"),
    asCodelist(cse, cdm)[[1]])

})

test_that("checkCodelist function", {
  x <- NULL
  expect_no_error(checkCodelist(x, allowNULL = TRUE))
  expect_error(checkCodelist(x, allowNULL = FALSE))

  x <- list("a" = c(1L, 2L))
  expect_error(checkCodelist(x))

  x <- omopgenerics::newCodelist(list("a" = c(1L, 2L)))
  expect_no_error(checkCodelist(x))

  x <- omopgenerics::newCodelistWithDetails(list("a" = dplyr::tibble("concept_id" = c(1L, 2L))))
  expect_no_error(checkCodelist(x))

  x <- omopgenerics::newConceptSetExpression(list("a" = dplyr::tibble("concept_id" = 1L, "excluded" = FALSE, "descendants" = FALSE, "mapped" = FALSE)))
  expect_no_error(checkCodelist(x, allowConceptSetExpression = TRUE))
  expect_error(checkCodelist(x, allowConceptSetExpression = FALSE))
})
