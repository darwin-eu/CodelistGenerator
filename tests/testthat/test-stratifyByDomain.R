test_that("stratify by domain works", {

  cdm <- mockVocabRef()

  codes <- omopgenerics::newCodelist(list("concepts_1" = c(20L,21L), "concepts_2" = c(13L,21L,22L)))
  expect_no_error(new_codes <- stratifyByDomain(x = codes, cdm = cdm, keepOriginal = FALSE))
  expect_true(inherits(new_codes,"codelist"))
  expect_true(all(names(new_codes) == c("concepts_1_drug", "concepts_2_drug", "concepts_2_unit")))
  expect_equal(new_codes$concepts_1_drug, c(20L, 21L))
  expect_equal(new_codes$concepts_2_drug, c(13L, 21L))
  expect_equal(new_codes$concepts_2_unit, c(22L))

  newCodelist <- list("concepts" = cdm[["concept"]] |>
                        dplyr::filter(.data$concept_id %in% c(13L, 20L, 21L, 24L)) |>
                        dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id", "standard_concept") |>
                        dplyr::collect()) |>
    omopgenerics::newCodelistWithDetails()

  expect_no_error(new_codes <- stratifyByDomain(x = newCodelist,
                                                cdm = cdm,
                                                keepOriginal = TRUE))

  expect_true(inherits(new_codes,"codelist_with_details"))
  expect_identical(c("concepts", "concepts_condition", "concepts_drug"), names(new_codes))
  expect_identical(new_codes$concepts, newCodelist$concepts)
  expect_identical(new_codes$concepts_condition, newCodelist$concepts |> dplyr::filter(.data$domain_id == "Condition"))
  expect_identical(new_codes$concepts_drug, newCodelist$concepts |> dplyr::filter(.data$domain_id == "Drug"))

  # expected errors
  expect_error(stratifyByDomain(list("a" = c(1L)), cdm, keepOriginal = TRUE))
  expect_error(stratifyByDomain(newCodelist, "a", keepOriginal = TRUE))
  expect_error(stratifyByDomain(newCodelist, cdm, keepOriginal = "a"))

})

