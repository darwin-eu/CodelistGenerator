test_that("tests with mock db", {
  backends <- c("database",
                "data_frame")

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
    expect_true(nrow(codes) == 2)
    expect_true("Osteoarthritis of knee" %in%
      (codes %>%
      dplyr::pull("concept_name")))
    expect_true("Osteoarthritis of hip" %in%
                  (codes %>%
                     dplyr::pull("concept_name")))

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
      includeDescendants = TRUE
    )
    expect_true((nrow(codes) == 5 &
      all(codes$concept_id %in% c(3, 4, 5, 8, 17)) &
      all(!codes$concept_id %in% c(1, 2, 7))))


    # test searchInSynonyms
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "osteoarthrosis",
      domains = "Condition",
      searchInSynonyms = TRUE
    )
    expect_true(any(codes$concept_name %in% "Arthritis"))

    # test exclusion
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = "Osteoarthritis of hip",
      domains = "Condition"
    )
    expect_true(any(!codes$concept_name %in% "Osteoarthritis of hip"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("Osteoarthritis of hip", "something else", "shoulder"),
      domains = "Condition"
    )
    expect_true(all(codes$concept_name != "Osteoarthritis of hip"))

    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      exclude = c("something else", "shoulder", "Osteoarthritis of hip"),
      domains = "Condition"
    )
    expect_true(all(codes$concept_name != "Osteoarthritis of hip"))


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
      includeDescendants = FALSE,
      domains = "Condition"
    )
    expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

    # all options
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "arthritis",
      domains = "Condition",
      standardConcept = "Standard",
      searchInSynonyms = TRUE,
      searchNonStandard = TRUE,
      exclude = "Childhood asthma",
      includeDescendants = TRUE,
      includeAncestor = TRUE
    )
    expect_true(nrow(codes) >= 1)


    # test search in drug
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "adalimumab",
      domains = "Drug",
      searchInSynonyms = TRUE,
      includeDescendants = FALSE,
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

    # search for drug and condition
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = c("arthritis", "adalimumab"),
      domains = c("Condition", "Drug"),
      searchInSynonyms = TRUE,
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
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})

test_that("tests with mock db - multiple domains", {
  backends <- c("database",
                "data_frame")

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
      keywords = "h o osteoarthritis",
      domains = c("Condition", "Observation"),
      includeDescendants = FALSE
    )
    expect_true(all(
      nrow(codes) == 3 &
      c(4,5,9) %in% codes$concept_id))

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})
