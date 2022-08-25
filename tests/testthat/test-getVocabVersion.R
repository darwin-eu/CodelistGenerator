test_that("tests with mock db", {

  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db - sqlite
  db <- generateMockVocabDb(dbType="SQLite")
  version <- getVocabVersion(db=db,
                  vocabularyDatabaseSchema = "main")

  expect_true(length(version)==1)
  expect_true(is.character(version))
  dbDisconnect(db)

  # mock db - duckdb
  db <- generateMockVocabDb(dbType="duckdb")
  version <- getVocabVersion(db=db,
                             vocabularyDatabaseSchema = NULL)

  expect_true(length(version)==1)
  expect_true(is.character(version))
  dbDisconnect(db)

  # with arrow
  db <- generateMockVocabDb()
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
