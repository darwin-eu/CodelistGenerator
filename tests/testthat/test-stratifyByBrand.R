test_that("stratify by brand works", {
  skip_on_cran()
  cdm <- mockVocabRef()

  codes <- omopgenerics::newCodelist(list("concepts_1" = c(20L,21L), "concepts_2" = c(13L,21L,22L)))
  expect_warning(new_codes <- stratifyByBrand(x = codes,cdm = cdm, keepOriginal = FALSE))
  expect_true(inherits(new_codes,"codelist"))
  expect_true(all(names(new_codes) == c("concepts_1_brand_1", "concepts_1_brand_2", "concepts_2_brand_2", "concepts_2_unclassified_brand")))
  expect_true(new_codes$concepts_1_brand_1 == c(20L))
  expect_equal(new_codes$concepts_1_brand_2, c(20L, 21L))
  expect_true(all(new_codes$concepts_2_brand_2 == c(13L, 21L)))
  expect_true(new_codes$concepts_2_unclassified_brand == c(22L))

  # Only unclassified
  expect_no_error(x <- stratifyByBrand(x = omopgenerics::newCodelist(list("concepts_1" = c(1L,2L,3L))), cdm))
  expect_true(inherits(x, "codelist"))
  expect_true(names(x) == "concepts_1_unclassified_brand")

  # Mixed and empty result
  c <- list("concepts_1" = c(1L,2L,3L), "concepts_2" = c(10L, 20L)) |> omopgenerics::newCodelist()
  expect_warning(x <- stratifyByBrand(x = c, cdm))
  expect_true(all(names(x) == c("concepts_1_unclassified_brand", "concepts_2_brand_1",  "concepts_2_brand_2", "concepts_2_unclassified_brand")))
  expect_true(inherits(x, "codelist"))
  expect_equal(x$concepts_2_unclassified_brand, 10L)

  newCodelist <- list("concepts" = cdm[["concept"]] |>
                        dplyr::filter(.data$concept_id %in% c(13L, 20L, 21L, 24L)) |>
                        dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id", "standard_concept") |>
                        dplyr::mutate("ancestor_concept_id" = 999L) |>
                        dplyr::collect()) |>
    omopgenerics::newCodelistWithDetails()

  expect_warning(new_codes <- stratifyByBrand(x = newCodelist,
                                              cdm = cdm,
                                              keepOriginal = TRUE))

  expect_true(inherits(new_codes,"codelist_with_details"))
  expect_true(all(names(new_codes) == c("concepts", "concepts_brand_1", "concepts_brand_2", "concepts_unclassified_brand")))
  expect_true(new_codes$concepts_brand_1$concept_id == c(20L))
  expect_equal(new_codes$concepts_brand_2$concept_id,  c(13L, 20L, 21L))
  expect_equal(colnames(new_codes$concepts_brand_1), c("concept_id","concept_name", "domain_id", "vocabulary_id", "standard_concept", "ancestor_concept_id"))
  expect_equal(colnames(new_codes$concepts_brand_2), c("concept_id","concept_name", "domain_id", "vocabulary_id", "standard_concept", "ancestor_concept_id"))
  expect_equal(colnames(new_codes$concepts), c("concept_id","concept_name", "domain_id", "vocabulary_id", "standard_concept", "ancestor_concept_id"))

  # Expected errors
  expect_error(stratifyByBrand(list("a" = c(1L)), cdm))
  expect_error(stratifyByBrand(newCodelist, "a"))
  expect_error(stratifyByBrand(newCodelist, cdm, keepOriginal = "a"))
})
