test_that("import vocab check", {
  library(DBI)
  library(arrow)
  library(dplyr)
  db <- generateMockVocabDb()
  dOut <- tempdir()
  downloadVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )
  # check overwrite by running again
  downloadVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE
  )
  # error as exists
  expect_error(
    downloadVocab(
      db = db,
      vocabularyDatabaseSchema = "main",
      dirOut = dOut,
      errorIfExists = TRUE
    )
  )
})
