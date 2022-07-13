test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- generateMockVocabDb()

  # tests
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  mappings <- showMappings(
    candidateCodelist = codes,
    sourceVocabularies="READ",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(
    any(mappings$standard_concept_name %in% "Osteoarthrosis")
  )
  expect_true(
    any(mappings$source_concept_name %in% "Degenerative arthropathy")
  )
  expect_true(
    any(mappings$standard_concept_name %in% "Osteoarthritis of knee")
  )
  expect_true(
    any(mappings$source_concept_name %in% "Knee osteoarthritis")
  )

  expect_true(all(c(
    "standard_concept_id",
    "standard_concept_name",
    "standard_vocabulary_id",
    "source_concept_id",
    "source_concept_name",
    "source_concept_code",
    "source_vocabulary_id"
  ) %in%
    names(mappings)))

  # expect error if not dbi connection
  expect_error(showMappings(
    candidateCodelist = codes,
    sourceVocabularies="READ",
    db = "a",
    vocabularyDatabaseSchema = "main"
  ))
  # expect error if vocabularyDatabaseSchema does not exist
    expect_error(showMappings(
    candidateCodelist = codes,
    sourceVocabularies="READ",
    db = db,
    vocabularyDatabaseSchema = "a"
  ))

    # expect error if sourceVocabularies does not exist
    # expect works
      mappings <- showMappings(
    candidateCodelist = codes,
    sourceVocabularies="READ",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
    # expect error
    expect_error(showMappings(
    candidateCodelist = codes,
    sourceVocabularies="READX",
    db = db,
    vocabularyDatabaseSchema = "main"
  ))
      expect_error(showMappings(
    candidateCodelist = codes,
    sourceVocabularies=c("Read","READX"),
    db = db,
    vocabularyDatabaseSchema = "main"
  ))



  DBI::dbDisconnect(db)
})
