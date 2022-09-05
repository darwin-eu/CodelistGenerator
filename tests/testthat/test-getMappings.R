test_that("tests with mock db sqlite", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = "main",
                      select = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))

  # tests
  codes <- getCandidateCodes(cdm=cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE
  )
  mappings <- getMappings(cdm=cdm,
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
  expect_error(getMappings(cdm="Not a cdm",
    candidateCodelist = codes,
    nonStandardVocabularies = "READ"
  ))

  # expect error if nonStandardVocabularies does not exist
  # expect works
  mappings <- getMappings(cdm=cdm,
    candidateCodelist = codes,
    nonStandardVocabularies = "READ"
  )
  # expect error
  expect_error(getMappings(cdm=cdm,
    candidateCodelist = codes,
    nonStandardVocabularies = "READX"
  ))
  expect_error(getMappings(cdm=cdm,
    candidateCodelist = codes,
    nonStandardVocabularies = c("Read", "READX")
  ))

  DBI::dbDisconnect(db)
})

test_that("tests with mock db duckdb", {
  library(DBI)
  library(duckdb)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,
                      select = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))
  # tests
  codes <- getCandidateCodes(cdm=cdm,
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE
  )
  mappings <- getMappings(cdm=cdm,
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


  DBI::dbDisconnect(db)
})

test_that("tests with mock arrow", {
  library(DBI)
  library(arrow)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = NULL,
                      select = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))
  dOut <- tempdir()
    downloadVocab(
    cdm = cdm,
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )
  codes <- getCandidateCodes(cdm=cdm,
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
