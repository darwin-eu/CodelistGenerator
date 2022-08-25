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
  dbDisconnect(db)

  # mock db - duckdb
  db <- mockVocab(dbType="duckdb")
  version <- getVocabVersion(db=db,
                             vocabularyDatabaseSchema = NULL)

  expect_true(length(version)==1)
  expect_true(is.character(version))
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

  dbDisconnect(db)

})
