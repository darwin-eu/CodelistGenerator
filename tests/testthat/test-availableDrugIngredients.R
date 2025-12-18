test_that("available ingredients", {
  skip_on_cran()
  backends <- c("database", "data_frame")
  for (i in seq_along(backends)) {
    cdm <- mockVocabRef(backends[[i]])
    namesInitial <- names(cdm)
    expect_no_error(res <- availableDrugIngredients(cdm))

    # Whole CDM
    manual_res <- cdm$concept |>
      dplyr::filter(standard_concept == "S" & concept_class_id == "Ingredient") |>
      dplyr::pull("concept_name")
    expect_true(setequal(res, manual_res))

    # codelist
    codelist <- list("first_codelist" = c(10L), "second_codelist" = c(7L, 9L)) |>
      omopgenerics::newCodelist()

    expect_no_error(res1 <- associatedDrugIngredients(cdm, x = codelist))
    expect_equal(names(res1), c("first_codelist", "second_codelist"))
    expect_true(res1$first_codelist == "Adalimumab")
    expect_true(length(res1$second_codelist) == 0)
    expect_equal(names(cdm), namesInitial)

    c <- list("descendant_drug" = c(13L), "ingredient_concept" = c(10L)) |>
      omopgenerics::newCodelist() |>
      associatedDrugIngredients(cdm)
    expect_identical(c[[1]], c("Adalimumab", "Other ingredient"))
    expect_identical(c[[2]], c("Adalimumab"))

    codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                    dplyr::filter(concept_id %in% c(10L)) |>
                                    dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                    dplyr::mutate("ancestor_concept_id" = 999L) |>
                                    dplyr::collect()) |>
      omopgenerics::newCodelistWithDetails()
    expect_no_error(res3 <- associatedDrugIngredients(cdm, x = codelist_with_details))
    expect_true(res3$codes == "Adalimumab")
    expect_no_error(res3 <- associatedDrugIngredients(cdm, x = codelist_with_details, standardConcept = "Non-standard"))
    expect_true(length(res3$codes) == 0)

    # Expected errors
    expect_error(availableDrugIngredients(cdm, x = list("codes" = 10L)))
    expect_error(availableDrugIngredients(cdm = 1))
    expect_error(availableDrugIngredients(cdm, standardConcept = "S"))

    expect_error(associatedDrugIngredients(cdm, x = list("codes" = 10L)))
    expect_error(associatedDrugIngredients(cdm))
    expect_error(associatedDrugIngredients(cdm, x, standardConcept = "S"))

  }
})
