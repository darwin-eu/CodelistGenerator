test_that("tests with mock db sqlite", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab()

  # tests
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  mappings <- getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    db = db,
    vocabularyDatabaseSchema = "main"
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

  # expect error if not dbi connection
  expect_error(getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    db = "a",
    vocabularyDatabaseSchema = "main"
  ))
  # expect error if vocabularyDatabaseSchema does not exist
  expect_error(getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    db = db,
    vocabularyDatabaseSchema = "a"
  ))

  # expect error if nonStandardVocabularies does not exist
  # expect works
  mappings <- getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  # expect error
  expect_error(getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READX",
    db = db,
    vocabularyDatabaseSchema = "main"
  ))
  expect_error(getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = c("Read", "READX"),
    db = db,
    vocabularyDatabaseSchema = "main"
  ))



  DBI::dbDisconnect(db)
})

test_that("tests with mock db duckdb", {
  library(DBI)
  library(duckdb)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab(dbType = "duckdb")

  # tests
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = NULL
  )
  mappings <- getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    db = db,
    vocabularyDatabaseSchema = NULL
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


  DBI::dbDisconnect(db)
})


test_that("tests with mock arrow", {
  library(DBI)
  library(arrow)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab()
  dOut <- tempdir()
    downloadVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE,
    arrowDirectory=dOut
  )
  mappings <- getMappings(
    candidateCodelist = codes,
    nonStandardVocabularies = "READ",
    arrowDirectory=dOut
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

})
