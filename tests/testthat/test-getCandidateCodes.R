test_that("tests with mock db", {
  backends <- c("database", "arrow", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    # test keywords search - exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 1 &
      codes$concept_name[1] == "Musculoskeletal disorder"))
    # variable names
    expect_true(all(c(
      "concept_id", "concept_name",
      "domain_id", "vocabulary_id"
    ) %in%
      names(codes)))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c(
        "knee osteoarthritis",
        "hip osteoarthritis"
      ),
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 2 &
      codes$concept_name[1] == "Osteoarthritis of knee" &
      codes$concept_name[2] == "Osteoarthritis of hip"))

    # test keywords search - fuzzy
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Arthritis"),
      fuzzyMatch = TRUE,
      maxDistanceCost = 0.2,
      domains = "Condition",
      includeDescendants = FALSE
    )
    expect_true(any(codes$concept_name %in% "Arthritis"))
    expect_true(any(codes$concept_name %in% "Osteoarthritis of knee"))
    expect_true(any(codes$concept_name %in% "Osteoarthritis of hip"))
    # with fuzzy, should pick up arthrosis
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    # test include descendants
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = TRUE
    )
    expect_true((nrow(codes) == 5 &
      all(codes$concept_id %in% c(1:5)) &
      all(!codes$concept_id %in% c(6, 7))))

    # test include ancestor
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Arthritis"),
      domains = "Condition",
      includeAncestor = TRUE
    )
    expect_true(any(codes$concept_name %in% "Musculoskeletal disorder"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Osteoarthritis of knee"),
      domains = "Condition",
      includeAncestor = TRUE
    )
    # nb includeAncestor should only include one level above
    expect_true(!any(codes$concept_name %in% "Musculoskeletal disorder"))
    expect_true(any(codes$concept_name %in% "Arthritis"))

    # test standardConcept
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Arthritis",
      domains = "Condition",
      standardConcept = c("Standard", "Non-standard"),
      includeDescendants = TRUE,
      searchViaSynonyms = FALSE
    )
    expect_true((nrow(codes) == 4 &
      all(codes$concept_id %in% c(3, 4, 5, 8)) &
      all(!codes$concept_id %in% c(1, 2, 7))))


    # test searchInSynonyms
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "osteoarthrosis",
      domains = "Condition",
      searchInSynonyms = TRUE
    )
    expect_true(any(codes$concept_name %in% "Arthritis"))

    # test searchViaSynonyms
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = "Condition",
      searchViaSynonyms = TRUE
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    # test exclusion
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = "Osteoarthritis of hip",
      domains = "Condition"
    )
    expect_true(any(!codes$concept_name %in% "Osteoarthritis of hip"))

    # test non-standard
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Musculoskeletal", "Degenerative arthropathy"),
      searchNonStandard = TRUE,
      includeDescendants = FALSE,
      domains = "Condition"
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Degenerative arthropathy"),
      searchNonStandard = TRUE,
      fuzzyMatch = TRUE,
      includeDescendants = FALSE,
      domains = "Condition"
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    # test vocabularyId
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis"),
      standardConcept = c("standard", "non-standard"),
      vocabularyId = "SNOMED",
      searchNonStandard = TRUE,
      domains = "Condition"
    )
    expect_true(all(codes$vocabulary_id %in% "snomed"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis"),
      standardConcept = c("standard", "non-standard"),
      vocabularyId = "Read",
      searchNonStandard = TRUE,
      domains = "Condition"
    )
    expect_true(all(codes$vocabulary_id %in% "read"))

    # test sequalae
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis"),
      includeSequela = TRUE,
      domains = "Condition"
    )
    expect_true("Osteonecrosis" %in% codes$concept_name)

    # test verbose
    expect_message(getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = "Condition",
      verbose = TRUE
    ))

    # all options used with exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = "Condition",
      conceptClassId = "Clinical Finding",
      standardConcept = "Standard",
      searchInSynonyms = TRUE,
      searchViaSynonyms = TRUE,
      searchNonStandard = TRUE,
      fuzzyMatch = FALSE,
      exclude = "Childhood asthma",
      includeDescendants = TRUE,
      includeAncestor = TRUE,
      verbose = TRUE
    )
    expect_true(nrow(codes) >= 1)

    # all options used with fuzzy
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Arthritis",
      exclude = "Osteoarthritis of hip",
      domains = "Condition",
      conceptClassId = "Clinical Finding",
      standardConcept = "Standard",
      searchInSynonyms = TRUE,
      searchViaSynonyms = TRUE,
      searchNonStandard = TRUE,
      fuzzyMatch = TRUE,
      maxDistanceCost = 0.1,
      includeDescendants = TRUE,
      includeAncestor = TRUE,
      verbose = TRUE
    )
    expect_true(nrow(codes) >= 1)

    # test search in drug
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "adalimumab",
      fuzzyMatch = TRUE,
      domains = "Drug",
      searchInSynonyms = TRUE,
      searchViaSynonyms = TRUE,
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(all(c(
      "concept_id", "concept_name",
      "domain_id", "vocabulary_id", "found_from", "ingredient_concept_id",
      "amount_value", "amount_unit_concept_id",
      "numerator_value", "numerator_unit_concept_id", "denominator_value",
      "denominator_unit_concept_id", "box_size", "dose_form"
    ) %in%
      names(codes)))
    expect_true(codes$concept_id == "10")

    # restrict on dose form
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "adalimumab",
      fuzzyMatch = TRUE,
      domains = "Drug",
      doseForm = "injection",
      searchInSynonyms = TRUE,
      searchViaSynonyms = TRUE,
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(codes$concept_id == "10")

    # search for drug and condition
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis", "adalimumab"),
      fuzzyMatch = TRUE,
      domains = c("Condition", "Drug"),
      searchInSynonyms = TRUE,
      searchViaSynonyms = TRUE,
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(all(c(
      "concept_id", "concept_name",
      "domain_id", "vocabulary_id", "found_from", "ingredient_concept_id",
      "amount_value", "amount_unit_concept_id",
      "numerator_value", "numerator_unit_concept_id", "denominator_value",
      "denominator_unit_concept_id", "box_size", "dose_form"
    ) %in%
      names(codes)))
    expect_true(any(codes$concept_id %in% "10"))

    ## Edge cases
    # check empty candidate set
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "asthmaX",
      domains = "Condition"
    )
    expect_true(nrow(codes) == 0)

    # keywords that don´t exist
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("Musculoskeletal disorder", "XXXXX"),
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    )
    expect_true("1" %in% codes$concept_id)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "XXXXX",
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 0)

    # conceptClassId that doesn´t exist
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      conceptClassId = c("clinical finding", "Something that doesn´t exist"),
      includeDescendants = FALSE
    )
    expect_true("1" %in% codes$concept_id)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      conceptClassId = "Something that doesn´t exist",
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 0)

    # vocabularyID that doesn´t exist
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      vocabularyId = c("SNOMED", "Something that doesn´t exist"),
      includeDescendants = FALSE
    )
    expect_true("1" %in% codes$concept_id)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      vocabularyId = c("Something that doesn´t exist"),
      includeDescendants = FALSE
    )
    expect_true(nrow(codes) == 0)

    # domain that doesn´t exist
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = c("Condition", "Some other table")
    )
    expect_true(nrow(codes) > 0)

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = c("Some other table")
    )
    expect_true(nrow(codes) == 0)


    ## Expected errors
    # keyword should be a character
    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = 35,
      standardConcept = c("Standard"),
      includeDescendants = FALSE
    ))

    expect_error(getCandidateCodes(
      cdm = "Not a cdm ref",
      keywords = "a",
      searchViaSynonyms = TRUE,
      fuzzyMatch = TRUE,
      exclude = NULL,
      includeDescendants = TRUE,
      includeAncestor = FALSE
    ))

    # standardConcept that doesn´t exist
    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      standardConcept = c("Standard", "Something that doesn´t exist"),
      includeDescendants = FALSE
    ))

    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      standardConcept = "Something that doesn´t exist",
      includeDescendants = FALSE
    ))

    cdm1 <- cdm
    cdm1$concept <- NULL
    expect_error(getCandidateCodes(
      cdm = cdm1,
      keywords = "Musculoskeletal disorder"
    ))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }
})

test_that("tests with mock db - multiple domains", {
  backends <- c("database", "arrow", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    # test keywords search - exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 4 &
      all(codes$concept_id %in% c(3:5, 9)) &
      all(!codes$concept_id %in% c(1, 2, 6, 7, 8))))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "H/O osteoarthritis",
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true(all(nrow(codes) == 1 &
      codes$concept_id == 9))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }
})

test_that("tests exact match", {
  backends <- c("database", "arrow", "data_frame")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    # test keywords search - exact
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exactMatch = TRUE,
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true((nrow(codes) == 1 &
                   codes$concept_id == 3 &
                   !codes$concept_id %in% c(1, 2, 4:8)))

    # without exact match
    codes1 <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthriti",
      exactMatch = TRUE,
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true(nrow(codes1) == 0)

    # expect error if fuzzy and exact
    expect_error(getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exactMatch = TRUE,
      fuzzyMatch = TRUE
    ))

    if (backends[[i]] == "database") {
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
    }
  }


})
