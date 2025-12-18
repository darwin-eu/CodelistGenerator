test_that("tests with mock db", {
  skip_on_cran()
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])
    namesInitial <- names(cdm)
    codelist_with_details <- list("codes" = cdm[["concept"]] |>
                                    dplyr::filter(concept_id %in% c(10L, 15L)) |>
                                    dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id") |>
                                    dplyr::mutate("ancestor_concept_id" = 999L) |>
                                    dplyr::collect()) |>
      omopgenerics::newCodelistWithDetails()

    codelist <- omopgenerics::newCodelist(list("codes1" = c(3L, 6L), "codes2" = c(24L)))

    # Available relationships ID ----
    expect_equal(sort(cdm$concept_relationship |>
                        dplyr::select("relationship_id") |>
                        dplyr::distinct() |>
                        dplyr::pull()),
                 availableRelationshipIds(cdm,
                                 standardConcept1 = c("Standard", "Non-standard", "Classification"),
                                 standardConcept2 = c("Standard", "Non-standard", "Classification"),
                                 domains1 = c("Condition", "Drug"),
                                 domains2 = c("Condition", "Drug")))

    expect_equal("Mapped from",
                 availableRelationshipIds(cdm,
                                 standardConcept1 = "Standard",
                                 standardConcept2 = "Non-standard",
                                 domains1 = "Condition",
                                 domains2 = "Condition"))
    expect_equal("Maps to",
                 availableRelationshipIds(cdm,
                                 standardConcept1 = "Non-standard",
                                 standardConcept2 = "Standard",
                                 domains1 = "Condition",
                                 domains2 = "Condition"))

    # codelist
    expect_no_error(x <- associatedRelationshipIds(cdm, x = codelist))
    expect_equal(names(x), c("codes1", "codes2"))
    expect_true(x$codes1 == "Due to of")
    expect_true(length(x$codes2) == 0)
    expect_equal(namesInitial, names(cdm))

    expect_no_error(x <- associatedRelationshipIds(cdm,
                                         x = codelist_with_details,
                                         standardConcept1 = c("Standard", "Non-standard"),
                                         standardConcept2 = c("Standard", "Non-standard"),
                                         domains1 = c("Condition", "Drug"),
                                         domains2 = c("Condition", "Drug")))
    expect_equal(x$codes, c("Has brand name", "RxNorm has dose form", "Subsumes"))

    # expected errors
    expect_error(availableRelationshipIds("cdm"))
    expect_error(availableRelationshipIds(cdm,
                                 standardConcept1 = "Standard",
                                 standardConcept2 = "Something else"))
    expect_error(availableRelationshipIds(cdm,
                                 domains1 = 22,
                                 domains2 = "Condition"))
    expect_error(x <- availableRelationshipIds(cdm, x = list("codes" = c(3L, 6L))))


    expect_error(associatedRelationshipIds("cdm"))
    expect_error(associatedRelationshipIds(cdm, x = 123))
    expect_error(associatedRelationshipIds(cdm,
                                          standardConcept1 = "Standard",
                                          standardConcept2 = "Something else"))
    expect_error(associatedRelationshipIds(cdm,
                                          domains1 = 22,
                                          domains2 = "Condition"))
    expect_error(x <- associatedRelationshipIds(cdm, x = list("codes" = c(3L, 6L))))

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
