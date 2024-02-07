test_that("comparing two codelists", {
  backends <- c("database")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    codes1 <- getCandidateCodes(
      cdm = cdm,
      keywords = "Arthritis",
      domains = "Condition",
      includeDescendants = TRUE
    )

    codes2 <- getCandidateCodes(
      cdm = cdm,
      keywords = c("knee osteoarthritis", "arthrosis"),
      domains = "Condition",
      includeDescendants = TRUE
    )

    codesCompared <- compareCodelists(
      codelist1 = codes1,
      codelist2 = codes2
    )

    # tests
    expect_true(all(c(
      "concept_id",
      "concept_name",
      "codelist"
    ) %in%
      names(codesCompared)))

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 3) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Only codelist 1")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 5) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Only codelist 1")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 4) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Both")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 2) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Only codelist 2")

    # expected errors
    expect_error(compareCodelists(
      codelist1 = codes1,
      codelist2 = "a"
    ))
    expect_error(compareCodelists(
      codelist1 = "a",
      codelist2 = codes2
    ))

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})

test_that("comparing two codelists- same codes found different ways", {
  backends <- c("database")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    codes1 <- getCandidateCodes(
      cdm = cdm,
      keywords = "Arthritis",
      domains = "Condition",
      includeDescendants = TRUE
    )

    codes2 <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthrosis"),
      searchInSynonyms = TRUE,
      domains = "Condition",
      includeDescendants = TRUE
    )

    codesCompared <- compareCodelists(
      codelist1 = codes1,
      codelist2 = codes2
    )

    # tests
    expect_true(nrow(codesCompared) == 4)

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 3) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Both")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 4) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Both")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 5) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Both")

    expect_true(codesCompared %>%
      dplyr::filter(concept_id == 2) %>%
      dplyr::select(codelist) %>%
      dplyr::pull() == "Only codelist 2")

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})
