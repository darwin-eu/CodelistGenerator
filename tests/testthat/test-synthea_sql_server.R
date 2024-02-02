test_that("test with synthea on sql server", {
  skip_if(Sys.getenv("darwinDbDatabaseServer") == "")

  db <-DBI::dbConnect(odbc::odbc(),
                      Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                      # Driver   = "ODBC Driver 17 for SQL Server", #asdf
                      Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                      Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                      UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                      PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                      TrustServerCertificate="yes",
                      Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]])

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
