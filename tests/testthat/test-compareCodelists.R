test_that("comparing two codelists", {
  skip_on_cran()
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

    # candidate_class codelist
    codesCompared <- compareCodelists(
      codelist1 = codes1,
      codelist2 = codes2
    )

    # compare codelist with details
    codesCompared2 <- compareCodelists(
      codelist1 = omopgenerics::newCodelistWithDetails(list("codelist1" = codes1)),
      codelist2 = omopgenerics::newCodelistWithDetails(list("codelist2" = codes2))
    )

    expect_identical(codesCompared, codesCompared2)

    expect_true(all(c(
      "concept_id",
      "concept_name",
      "codelist"
    ) %in%
      names(codesCompared)))

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 3) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Only in codelist codelist1")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 5) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Only in codelist codelist1")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 4) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Both")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 2) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Only in codelist codelist2")

    # expected errors
    expect_error(compareCodelists(
      codelist1 = codes1,
      codelist2 = "a"
    ))
    expect_error(compareCodelists(
      codelist1 = "a",
      codelist2 = codes2
    ))

    # compare codelist
    codesCompared <- compareCodelists(
      omopgenerics::newCodelist(list("a" = c(1L, 2L), "c" = c(4L))),
      omopgenerics::newCodelist(list("b" = c(2L, 3L), "d" = c(4L)))
    )

    expect_true(inherits(codesCompared, "list"))
    expect_equal(names(codesCompared), c("a_b", "a_d", "c_b", "c_d"))
    expect_true(codesCompared$a_b |>
                  dplyr::filter(concept_id == 1) |>
                  dplyr::select("codelist") |>
                  dplyr::pull() == "Only in codelist a")

    expect_true(codesCompared$c_d |>
                  dplyr::select("codelist") |>
                  dplyr::pull() == "Both")

    expect_true(codesCompared$a_b |>
                  dplyr::filter(concept_id == 3) |>
                  dplyr::select("codelist") |>
                  dplyr::pull() == "Only in codelist b")

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
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
      codelist1 = omopgenerics::newCodelistWithDetails(list("codes1" = codes1)),
      codelist2 = omopgenerics::newCodelistWithDetails(list("codes2" = codes2))
    )

    # tests
    expect_true(inherits(codesCompared, "tbl"))
    expect_true(nrow(codesCompared) == 4)

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 3) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Both")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 4) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Both")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 5) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Both")

    expect_true(codesCompared |>
      dplyr::filter(concept_id == 2) |>
      dplyr::select(codelist) |>
      dplyr::pull() == "Only in codelist codes2")

    if (backends[[i]] == "database") {
      CDMConnector::cdmDisconnect(cdm)
    }
  }
})
