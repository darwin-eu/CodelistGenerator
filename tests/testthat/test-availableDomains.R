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

    codelist <- omopgenerics::newCodelist(list("codes1" = c(1L, 20L), "codes2" = c(1L, 2L, 3L, 9L)))

    # Available domains ----
    expect_no_error(domains <- availableDomains(cdm = cdm))
    expect_true(all(c("Condition", "Drug", "Observation", "Unit") == domains))
    expect_no_error(domains <- associatedDomains(cdm = cdm, x = codelist))
    expect_true(inherits(domains, "list"))
    expect_true(all(c("Condition", "Drug") == domains$codes1))
    expect_true(all(c("Condition", "Observation") == domains$codes2))
    expect_no_error(domains <- associatedDomains(cdm = cdm, x = codelist_with_details))
    expect_true(inherits(domains, "list"))
    expect_true(all(c("Drug") == domains$codes))


    # expected errors
    expect_error(availableDomains(cdm = "a"))
    expect_error(availableDomains(cdm, standardConcept = FALSE))

    expect_error(associatedDomains(cdm = "a"))
    expect_error(associatedDomains(cdm))
    expect_error(associatedDomains(cdm, x = list("a" = 123)))
    expect_error(associatedDomains(cdm, standardConcept = FALSE))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
