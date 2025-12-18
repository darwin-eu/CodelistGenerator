test_that("codelist to concept set expression", {

 cl <- list(a = c(1L),
            b = c(2L, 3L),
            c = 3L) |>
    omopgenerics::newCodelist()

 expect_identical(
   asConceptSetExpression(cl)[["a"]],
   dplyr::tibble(concept_id = 1L,
                 excluded = FALSE,
                 descendants = FALSE,
                 mapped = FALSE)
   )
 expect_identical(
   asConceptSetExpression(cl)[["b"]],
   dplyr::tibble(concept_id = c(2L, 3L),
                 excluded = c(FALSE, FALSE),
                 descendants = c(FALSE, FALSE),
                 mapped = c(FALSE, FALSE))
 )
 expect_identical(
   asConceptSetExpression(cl)[["c"]],
   dplyr::tibble(concept_id = 3L,
                 excluded = FALSE,
                 descendants = FALSE,
                 mapped = FALSE)
 )

})

test_that("codelist to concept set expression", {

  cl <- list(a = dplyr::tibble(concept_id = c(1L),
                               concept_name = "aaa"),
             b = dplyr::tibble(concept_id = c(2L, 3L),
                               concept_name = "aaa"),
             c = dplyr::tibble(concept_id = 3L,
                               concept_name = "aaa")) |>
    omopgenerics::newCodelistWithDetails()

  expect_identical(
    asConceptSetExpression(cl)[["a"]],
    dplyr::tibble(concept_id = 1L,
                  excluded = FALSE,
                  descendants = FALSE,
                  mapped = FALSE)
  )
  expect_identical(
    asConceptSetExpression(cl)[["b"]],
    dplyr::tibble(concept_id = c(2L, 3L),
                  excluded = c(FALSE, FALSE),
                  descendants = c(FALSE, FALSE),
                  mapped = c(FALSE, FALSE))
  )
  expect_identical(
    asConceptSetExpression(cl)[["c"]],
    dplyr::tibble(concept_id = 3L,
                  excluded = FALSE,
                  descendants = FALSE,
                  mapped = FALSE)
  )

})
