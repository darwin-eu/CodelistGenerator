test_that("tests with mock db", {
  skip_on_cran()
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])
    codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                    dplyr::filter(concept_id %in% c(10L, 15L, 24L)) |>
                                    dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                    dplyr::mutate("ancestor_concept_id" = 999L) |>
                                    dplyr::collect()) |>
      omopgenerics::newCodelistWithDetails()

    codelist <- list("first_codelist" = c(1L, 20L), "second_codelist" = c(10L, 15L)) |>
      omopgenerics::newCodelist()

    # Whole db
    expect_no_error(doseForms <- availableDoseForms(cdm = cdm))
    expect_identical(c("Injectable Foam", "Injection", "Nasal Powder", "Topical Liquefied Gas"),
                     doseForms)
    expect_no_error(doseForms <- associatedDoseForms(cdm = cdm, x = codelist))
    expect_true(all(names(doseForms) == c("first_codelist", "second_codelist")))
    expect_identical(doseForms$first_codelist, c("nasal_powder", "unclassified_dose_form"))
    expect_identical(doseForms$second_codelist, c("injection", "unclassified_dose_form"))


    # expected errors
    expect_error(availableDoseForms(cdm = "a"))

    expect_error(associatedDoseForms(cdm = "a"))
    expect_error(associatedDoseForms(cdm, x = NULL))
    expect_error(associatedDoseForms(cdm, x = 123))
    expect_error(associatedDoseForms(cdm, x = list(123)))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
