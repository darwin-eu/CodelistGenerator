test_that("tests with mock db", {
  skip_on_cran()
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])
    namesInitial <- names(cdm)
    codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                    dplyr::filter(concept_id %in% c(10L, 15L, 24L)) |>
                                    dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                    dplyr::mutate("ancestor_concept_id" = 999L) |>
                                    dplyr::collect()) |>
      omopgenerics::newCodelistWithDetails()

    # Whole cdm
    vocabs <- availableVocabularies(cdm = cdm, domain = "Condition", standardConcept = "Standard")
    expect_true(inherits(vocabs, "character"))
    expect_true(length(vocabs) >= 1)
    expect_true(vocabs == "SNOMED")
    vocabs <- availableVocabularies(cdm = cdm, domain = "Drug", standardConcept = "Non-standard")
    expect_true(all(vocabs == c("ATC", "OMOP", "RxNorm", "RxNorm Extension")))

    # List
    expect_error(x <- associatedVocabularies(cdm, list("codes" = c(1L,20L))))

    # Codelist
    codelist <- list("first_codelist" = c(1L, 20L), "second_codelist" = c(7L, 9L)) |>
      omopgenerics::newCodelist()
    expect_no_error(x <- associatedVocabularies(codelist, cdm, standardConcept = c("Standard", "Non-standard", "Classification")))
    expect_true(all(names(x) == c("first_codelist", "second_codelist")))
    expect_true(all(x[[1]] == c("RxNorm", "SNOMED")))
    expect_true(all(x[[2]] == c("LOINC", "Read")))

    expect_no_error(x <- associatedVocabularies(codelist, cdm, standardConcept = c("Standard")))
    expect_true(all(names(x) == c("first_codelist", "second_codelist")))
    expect_true(all(x[[2]] == c("LOINC")))

    # Codelist with details
    expect_no_error(x <- associatedVocabularies(codelist_with_details, cdm, standardConcept = c("Standard", "Non-standard", "Classification")))
    expect_true(names(x) == "codes")
    expect_true(all(x[[1]] == c("ICD10", "RxNorm", "SNOMED")))

    # expected errors
    expect_error(availableVocabularies(cdm, x = 123))
    expect_error(availableVocabularies(cdm, x = list("a" = 123)))
    expect_error(availableVocabularies(cdm = "a"))

    expect_error(associatedVocabularies(cdm))
    expect_error(associatedVocabularies(cdm, x = list("a" = 123)))
    expect_error(associatedVocabularies(cdm = "a"))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
