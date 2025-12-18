test_that("stratify by vocabulary works", {
  cdm <- mockVocabRef()

  # codelist
  codelist <- newCodelist(list("codes1" = c(1L, 5L, 10L),
                               "codes2" = c(2L, 15L, 22L)))
  expect_no_error(x <- stratifyByVocabulary(codelist, cdm, keepOriginal = TRUE))
  expect_true(inherits(x, "codelist"))
  expect_identical(names(x), c("codes1", "codes1_rx_norm", "codes1_snomed",
                               "codes2", "codes2_icd10", "codes2_snomed",
                               "codes2_ucum"))
  expect_identical(x$codes1, c(1L, 5L, 10L))
  expect_identical(x$codes1_rx_norm, c(10L))
  expect_identical(x$codes1_snomed, c(1L, 5L))
  expect_identical(x$codes2, c(2L, 15L, 22L))
  expect_identical(x$codes2_icd10, 15L)
  expect_identical(x$codes2_snomed, 2L)
  expect_identical(x$codes2_ucum, 22L)

  # codelist + without keep original
  expect_no_error(x <- stratifyByVocabulary(codelist, cdm))
  expect_true(inherits(x, "codelist"))
  expect_identical(sort(names(x)), sort(c("codes1_rx_norm", "codes1_snomed",
                               "codes2_icd10", "codes2_snomed",
                               "codes2_ucum")))

  # codelist with details
  codelist_with_details <- asCodelistWithDetails(codelist, cdm)
  expect_no_error(x <- stratifyByVocabulary(codelist_with_details, cdm, keepOriginal = TRUE))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(sort(names(x)), sort(c("codes1_rx_norm", "codes1_snomed",
                               "codes2_icd10", "codes2_snomed",
                               "codes2_ucum", "codes1", "codes2")))
  expect_identical(x$codes2, codelist_with_details$codes2)
  expect_identical(x$codes1_rx_norm, codelist_with_details$codes1 |> dplyr::filter(.data$concept_id == 10L))

  # codelist with details without vocabulary_id column and another extra column and negate
  codelist_with_details <- purrr::map(codelist_with_details,
                                      ~dplyr::select(.x, -"vocabulary_id") |>
                                        dplyr::mutate("extra" = "hello")) |>
    newCodelistWithDetails()
  expect_no_error(x <- stratifyByVocabulary(codelist_with_details, cdm))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(names(x), c("codes1_rx_norm", "codes1_snomed", "codes2_icd10", "codes2_snomed", "codes2_ucum"))
  expect_identical(x$codes2_icd10, codelist_with_details$codes2 |> dplyr::filter(.data$concept_id %in% c(15L)))

  # unclassified vocabulary
  cdm[["concept"]] <- cdm[["concept"]] |>
    dplyr::mutate("vocabulary_id" = dplyr::if_else(.data$concept_id == 1, NA, .data$vocabulary_id))
  expect_no_error(x <- stratifyByVocabulary(codelist_with_details, cdm))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(sort(names(x)), sort(c("codes1_rx_norm", "codes1_snomed", "codes1_unclassified_vocabulary",
                               "codes2_icd10", "codes2_snomed","codes2_ucum")))
  expect_identical(x$codes1_unclassified_vocabulary$concept_id, 1L)

  # empty codelist
  expect_warning(x <- stratifyByVocabulary(omopgenerics::emptyCodelist(), cdm))
  expect_identical(omopgenerics::emptyCodelist(), x)
  expect_warning(x <- stratifyByVocabulary(omopgenerics::emptyCodelistWithDetails(), cdm))
  expect_identical(omopgenerics::emptyCodelistWithDetails(), x)

  # expected errors
  expect_error(stratifyByVocabulary(list("a" = 1L),  cdm))
  expect_error(stratifyByVocabulary(codelist,  "a"))
  expect_error(stratifyByVocabulary(codelist,  cdm, keepOriginal = "maybe"))
})
