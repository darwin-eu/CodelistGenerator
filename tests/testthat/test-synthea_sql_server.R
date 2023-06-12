test_that("test with synthea on sql server", {
  skip_if(Sys.getenv("darwinDbDatabaseServer") == "")

  db <- DBI::dbConnect(odbc::odbc(),
    Driver   = "ODBC Driver 11 for SQL Server",
    Server   = Sys.getenv("darwinDbDatabaseServer"),
    Database = Sys.getenv("darwinDbDatabase"),
    UID      = Sys.getenv("darwinDbUser"),
    PWD      = Sys.getenv("darwinDbPassword"),
    Port     = Sys.getenv("darwinDbDatabasePort")
  )

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "cdm_synthea_100k",
    cdm_tables = tidyselect::all_of(c(
      "concept",
      "concept_relationship",
      "concept_ancestor",
      "concept_synonym",
      "vocabulary"
    ))
  )
  vocabVersion <- getVocabVersion(cdm = cdm)
  expect_true(length(vocabVersion) == 1)
  expect_true(is.character(vocabVersion))

  # search in database
  asthma <- getCandidateCodes(
    cdm = cdm,
    keywords = "asthma",
    domains = "Condition"
  )
  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id"
  ) %in%
    names(asthma)))
  expect_true(nrow(asthma) > 0)

  asthmaIcdMappings <- getMappings(
    cdm = cdm,
    candidateCodelist = asthma,
    nonStandardVocabularies = "ICD10CM"
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
    names(asthmaIcdMappings)))
  expect_true(nrow(asthmaIcdMappings) > 0)

  dbDisconnect(db)
})
