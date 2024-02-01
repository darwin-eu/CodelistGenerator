test_that("tests with mock", {
  backends <- c("database")

  for (i in seq_along(backends)) {
    # mock db
    cdm <- mockVocabRef(backends[[i]])

    # tests
    codes <- getCandidateCodes(
      cdm = cdm,
      keywords = "Musculoskeletal disorder",
      domains = "Condition",
      includeDescendants = TRUE
    )
    mappings <- getMappings(
      cdm = cdm,
      candidateCodelist = codes,
      nonStandardVocabularies = "READ"
    )
    expect_true(
      any(mappings$standard_concept_name %in% "Osteoarthrosis")
    )
    expect_true(
      any(mappings$non_standard_concept_name %in% "Degenerative arthropathy")
    )
    expect_true(
      any(mappings$standard_concept_name %in% "Osteoarthritis of knee")
    )
    expect_true(
      any(mappings$non_standard_concept_name %in% "Knee osteoarthritis")
    )

    expect_true(all(c(
      "standard_concept_id",
      "standard_concept_name",
      "standard_vocabulary_id",
      "non_standard_concept_id",
      "non_standard_concept_name",
      "non_standard_concept_code",
      "non_standard_vocabulary_id"
    ) %in%
      names(mappings)))

    # expect error if not a cdm reference
    expect_error(getMappings(
      cdm = "Not a cdm",
      candidateCodelist = codes,
      nonStandardVocabularies = "READ"
    ))

    # expect error if nonStandardVocabularies does not exist
    # expect works
    mappings <- getMappings(
      cdm = cdm,
      candidateCodelist = codes,
      nonStandardVocabularies = "READ"
    )
    # expect error
    expect_error(getMappings(
      cdm = cdm,
      candidateCodelist = codes,
      nonStandardVocabularies = "READX"
    ))
    expect_error(getMappings(
      cdm = cdm,
      candidateCodelist = codes,
      nonStandardVocabularies = c("Read", "READX")
    ))

    if (backends[[i]] == "database") {
      CDMConnector::cdm_disconnect(cdm)
    }
  }
})
