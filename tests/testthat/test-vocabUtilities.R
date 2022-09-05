test_that("tests with mock db", {

  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db - sqlite
  db <- mockVocab(dbType="SQLite")
  cdm <- cdm_from_con(con = db,cdm_schema = "main",
                      select = tidyselect::all_of(c("concept",
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
                      select = tidyselect::all_of(c("concept",
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
  version <- getVocabVersion(arrowDirectory = dOut)
  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(arrowDirectory = dOut)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  vocabs <- getVocabularies(arrowDirectory = dOut)
  expect_true(length(vocabs)>=1)
  expect_true(is.character(vocabs))

  concept_classes <- getconceptClassId(arrowDirectory = dOut)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(arrowDirectory = dOut,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

})
