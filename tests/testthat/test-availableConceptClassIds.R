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

    codelist <- omopgenerics::newCodelist(list("codes1" = c(10L, 15L, 24L), "codes2" = c(1L, 2L, 3L, 9L)))

    # Available concept class id ----
    expect_no_error(conceptClasses <- availableConceptClassIds(cdm = cdm, standardConcept = c("Standard", "Non-standard")))
    expect_true(all(conceptClasses == c("ATC 1st", "Brand Name", "Clinical Drug Form",
                                        "Clinical Finding", "Diagnosis", "Dose Form",
                                        "Drug", "ICD Code", "ICD10 Chapter",
                                        "ICD10 SubChapter", "Ingredient", "Observation",
                                        "Unit" )))

    expect_no_error(conceptClasses <- availableConceptClassIds(cdm = cdm, domain = "Condition"))
    expect_true(conceptClasses == "Clinical Finding")

    expect_no_error(conceptClasses <- associatedConceptClassIds(cdm = cdm, x = codelist))
    expect_true(inherits(conceptClasses, "list"))
    expect_true(all(conceptClasses$codes1 == c("Ingredient")))
    expect_true(all(conceptClasses$codes2 == c("Clinical Finding", "Observation")))

    expect_no_error(conceptClasses <- associatedConceptClassIds(cdm = cdm, x = codelist_with_details))
    expect_true(all(conceptClasses$codes == c("Ingredient")))

    # expected errors
    expect_error(availableConceptClassIds(cdm = "a"))
    expect_error(availableConceptClassIds(cdm, standardConcept = FALSE))
    expect_error(availableConceptClassIds(cdm, x = 123))
    expect_error(availableConceptClassIds(cdm, x = list("c" = 12)))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
