test_that("subset on vocabulary works", {
  skip_on_cran()
  cdm <- mockVocabRef()

  # codelist
  codelist <- newCodelist(list("codes1" = c(1L, 5L, 10L),
                               "codes2" = c(2L, 15L, 22L)))
  expect_no_error(x <- subsetOnVocabulary(codelist, cdm, vocabulary = "SNOMED"))
  expect_true(inherits(x, "codelist"))
  expect_identical(names(x), c("codes1", "codes2"))
  expect_identical(x$codes1, c(1L, 5L))
  expect_identical(x$codes2, c(2L))

  # codelist + negate
  expect_no_error(x <- subsetOnVocabulary(codelist,
                                          cdm,
                                          vocabulary = "SNOMED",
                                          negate = TRUE))
  expect_true(inherits(x, "codelist"))
  expect_identical(names(x), c("codes1", "codes2"))
  expect_identical(x$codes1, c(10L))
  expect_identical(x$codes2, c(15L, 22L))

  # codelist with details
  codelist_with_details <- asCodelistWithDetails(codelist, cdm)
  expect_no_error(x <- subsetOnVocabulary(codelist_with_details, cdm, vocabulary = "SNOMED"))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(names(x), c("codes1", "codes2"))
  expect_identical(x$codes1, codelist_with_details[[1]] |> dplyr::filter(.data$concept_id != 10L))
  expect_identical(x$codes2, codelist_with_details[[2]] |> dplyr::filter(!.data$concept_id %in% c(15L, 22L)))

  # codelist with details without vocabulary_id column and another extra column and negate
  codelist_with_details <- purrr::map(codelist_with_details,
                                      ~dplyr::select(.x, -"vocabulary_id") |>
                                        dplyr::mutate("extra" = "hello")) |>
    newCodelistWithDetails()
  expect_no_error(x <- subsetOnVocabulary(codelist_with_details, cdm, vocabulary = "SNOMED", negate = TRUE))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(names(x), c("codes1", "codes2"))
  expect_identical(x$codes1, codelist_with_details[[1]] |> dplyr::filter(.data$concept_id == 10L))
  expect_identical(x$codes2, codelist_with_details[[2]] |> dplyr::filter(.data$concept_id %in% c(15L, 22L)))

  # lower case + more than one vocab
  expect_no_error(x <- subsetOnVocabulary(codelist_with_details, cdm, vocabulary = c("snomed", "UCUM"), negate = FALSE))
  expect_identical(x$codes1$concept_id, c(1L, 5L))
  expect_identical(x$codes2$concept_id, c(2L, 22L))
  expect_no_error(x <- subsetOnVocabulary(codelist_with_details, cdm, vocabulary = c("snomed", "UCUM"), negate = TRUE))
  expect_identical(x$codes1$concept_id, c(10L))
  expect_identical(x$codes2$concept_id, c(15L))

  # 1 codelist ends up empty
  expect_warning(x <- subsetOnVocabulary(codelist_with_details, cdm, vocabulary = "UCUM"))
  expect_true(inherits(x, "codelist_with_details"))
  expect_identical(names(x), c("codes2"))
  expect_warning(x <- subsetOnVocabulary(codelist, cdm, vocabulary = "UCUM"))
  expect_true(inherits(x, "codelist"))
  expect_identical(names(x), c("codes2"))

  # all codelists ends up empty
  expect_warning(x <- subsetOnVocabulary(
    omopgenerics::newCodelistWithDetails(list("codes1" = codelist_with_details$codes1)),
    cdm,
    vocabulary = "UCUM"))
  expect_equal(omopgenerics::emptyCodelistWithDetails(), x)

  # empty codelist
  expect_warning(x <- subsetOnVocabulary(omopgenerics::emptyCodelist(), cdm, vocabulary = "UCUM"))
  expect_identical(omopgenerics::emptyCodelist(), x)
  expect_warning(x <- subsetOnVocabulary(omopgenerics::emptyCodelistWithDetails(), cdm, vocabulary = "UCUM"))
  expect_identical(omopgenerics::emptyCodelistWithDetails(), x)

  # expected errors
  expect_error(subsetOnVocabulary(list("a" = 1L),  cdm, "snomed"))
  expect_error(subsetOnVocabulary(codelist,  "a", "snomed"))
  expect_error(subsetOnVocabulary(codelist,  cdm, vocabulary =  1234))
  expect_error(subsetOnVocabulary(codelist,  cdm, vocabulary = NULL))
  expect_error(subsetOnVocabulary(codelist,  cdm, "snomed", negate = 123))
})
