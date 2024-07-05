test_that("tests with mock db", {
  backends <- c("database", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    version <- getVocabVersion(cdm = cdm)
    expect_true(length(version) == 1)
    expect_true(is.character(version))

    vocabs <- getVocabularies(cdm = cdm)
    expect_true(length(vocabs) >= 1)
    expect_true(is.character(vocabs))

    domains <- getDomains(cdm = cdm)
    expect_true(all(c("Condition", "Observation") %in% domains))
    expect_true(is.character(domains))

    conceptClasses <- getConceptClassId(cdm = cdm)
    expect_true(is.character(conceptClasses))

    conceptClasses <- getConceptClassId(
      cdm = cdm,
      domain = "Condition"
    )
    expect_true(is.character(conceptClasses))

    descendants1 <- getDescendants(
      cdm = cdm,
      conceptId = 1,
      withAncestor = FALSE
    )
    expect_true(all(sort(descendants1$concept_id) == c(1, 2, 3, 4, 5)))
    expect_true("concept_name" %in% colnames(descendants1))

    descendants2 <- getDescendants(
      cdm = cdm,
      conceptId = 1,
      withAncestor = TRUE
    )
    expect_true(all(sort(descendants2$concept_id) == c(1, 2, 3, 4, 5)))
    expect_true("ancestor_concept_id" %in% colnames(descendants2))
    expect_true(all(descendants2$ancestor_concept_id == 1))

    descendants3 <- getDescendants(
      cdm = cdm,
      conceptId = 10,
      withAncestor = FALSE,
      doseForm = c("Injection", "Injectable")
    )
    expect_true(all(sort(descendants3$concept_id) == c(10, 13)))

    descendants4 <- getDescendants(
      cdm = cdm,
      conceptId = 10,
      withAncestor = TRUE,
      doseForm = c("Injection", "Injectable")
    )
    expect_true(all(sort(descendants4$concept_id) == c(10, 13)))


    descendants5 <- getDescendants(
      cdm = cdm,
      conceptId = 10,
      withAncestor = TRUE,
      doseForm = c("Injectable")
    )
    expect_true(all(descendants5$concept_id == c(13)))



    doseForms <- getDoseForm(cdm = cdm)
    expect_true(all(doseForms %in% c("Injection", "Injectable")))

    # all relationships in mock
    expect_equal(sort(cdm$concept_relationship |>
                        dplyr::select("relationship_id") |>
                        dplyr::distinct() |>
                        dplyr::pull()),
                 getRelationshipId(cdm,
                                   standardConcept1 = c("standard", "non-standard", "classification"),
                                   standardConcept2 = c("standard", "non-standard", "classification"),
                                   domains1 = c("condition", "drug"),
                                   domains2 = c("condition", "drug")))

    # specific relationships
    expect_equal("Mapped from",
                 getRelationshipId(cdm,
                                   standardConcept1 = "standard",
                                   standardConcept2 = "non-standard",
                                   domains1 = "condition",
                                   domains2 = "condition"))
    expect_equal("Maps to",
                 getRelationshipId(cdm,
                                   standardConcept1 = "non-standard",
                                   standardConcept2 = "standard",
                                   domains1 = "condition",
                                   domains2 = "condition"))

    # casing of inputs will be ignored
    expect_equal(getRelationshipId(cdm,
                                   standardConcept1 = "Standard",
                                   standardConcept2 = "Non-standard",
                                   domains1 = "Condition",
                                   domains2 = "Condition"),
                 getRelationshipId(cdm,
                                   standardConcept1 = "standard",
                                   standardConcept2 = "non-standard",
                                   domains1 = "condition",
                                   domains2 = "condition"))



    # expected errors
    expect_error(getVocabVersion(cdm = "a"))
    expect_error(getVocabularies(cdm = "a"))
    expect_error(getDomains(cdm = "a"))
    expect_error(getDomains(cdm, standardConcept = FALSE))
    expect_error(getConceptClassId(cdm = "a"))
    expect_error(getConceptClassId(cdm, standardConcept = FALSE))
    expect_error(getDescendants(cdm = "a"))
    expect_error(getDoseForm(cdm = "a"))
    expect_error(getRelationshipId("cdm"))
    expect_error(getRelationshipId(cdm,
                      standardConcept1 = "Standard",
                      standardConcept2 = "Something else"))
   expect_error(getRelationshipId(cdm,
                    domains1 = 22,
                      domains2 = "Condition"))



    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})
