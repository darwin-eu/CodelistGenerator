test_that("tests with mock db", {

  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db - sqlite
  db <- mockVocab(dbType="SQLite")
  cdm <- cdm_from_con(con = db,cdm_schema = "main",
                      cdm_tables = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))

  version <- getVocabVersion(cdm=cdm)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  vocabs <- getVocabularies(cdm=cdm)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  domains<-getDomains(cdm=cdm)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getconceptClassId(cdm=cdm)
  expect_true(is.character(concept_classes))

  concept_classes <- getconceptClassId(cdm=cdm,
                    domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

  # mock db - duckdb
  db <- mockVocab(dbType="duckdb")
  cdm <- cdm_from_con(con = db,
                      cdm_tables = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))
  version <- getVocabVersion(cdm=cdm)

  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(cdm=cdm)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  vocabs <- getVocabularies(cdm=cdm)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  concept_classes <- getconceptClassId(cdm = cdm)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(cdm = cdm,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

  # with arrow
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = NULL,
                      cdm_tables = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))
  dOut <- tempfile()
  dir.create(dOut)
  CDMConnector::stow(cdm, dOut)

  cdm_arrow <- CDMConnector::cdm_from_files(
    path = dOut,
    cdm_tables = tidyselect::all_of(c("concept",
                                      "concept_relationship",
                                      "concept_ancestor",
                                      "concept_synonym",
                                      "vocabulary")),
    as_data_frame = FALSE
  )

  version <- getVocabVersion(cdm = cdm_arrow)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(cdm = cdm_arrow)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  vocabs <- getVocabularies(cdm = cdm_arrow)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  concept_classes <- getconceptClassId(cdm = cdm_arrow)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(cdm = cdm_arrow,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))
  dbDisconnect(db)

  # in R
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = NULL,
                      cdm_tables = tidyselect::all_of(c("concept",
                                                        "concept_relationship",
                                                        "concept_ancestor",
                                                        "concept_synonym",
                                                        "vocabulary")))
  dOut <- tempfile()
  dir.create(dOut)
  CDMConnector::stow(cdm, dOut)

  cdm_df <- CDMConnector::cdm_from_files(
    path = dOut,
    cdm_tables = tidyselect::all_of(c("concept",
                                      "concept_relationship",
                                      "concept_ancestor",
                                      "concept_synonym",
                                      "vocabulary")),
    as_data_frame = TRUE
  )

  version <- getVocabVersion(cdm = cdm_df)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(cdm = cdm_df)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  vocabs <- getVocabularies(cdm = cdm_df)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  concept_classes <- getconceptClassId(cdm = cdm_df)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(cdm = cdm_df,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))


  dbDisconnect(db)

})
