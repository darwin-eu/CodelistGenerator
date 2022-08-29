test_that("tests with mock db", {

  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db - sqlite
  db <- mockVocab(dbType="SQLite")
  version <- getVocabVersion(db=db,
                  vocabularyDatabaseSchema = "main")

  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(db=db,
             vocabularyDatabaseSchema = "main")
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getconceptClassId(db = db,
                                      vocabularyDatabaseSchema = "main")
  expect_true(is.character(concept_classes))

  concept_classes <- getconceptClassId(db = db,
                    vocabularyDatabaseSchema = "main",
                    domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

  # mock db - duckdb
  db <- mockVocab(dbType="duckdb")
  version <- getVocabVersion(db=db,
                             vocabularyDatabaseSchema = NULL)

  expect_true(length(version)==1)
  expect_true(is.character(version))

  domains<-getDomains(db=db)
  expect_true(all(c("Condition", "Observation") %in% domains))
  expect_true(is.character(domains))

  concept_classes <- getconceptClassId(db = db)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(db = db,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

  # with arrow
  db <- mockVocab()
  dOut <- tempdir()
  downloadVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
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

  concept_classes <- getconceptClassId(arrowDirectory = dOut)
  expect_true(is.character(concept_classes))
  concept_classes <- getconceptClassId(arrowDirectory = dOut,
                                       domain = "Condition")
  expect_true(is.character(concept_classes))

  dbDisconnect(db)

})
