test_that("import vocab check", {
  library(DBI)
  library(arrow)
  library(dplyr)
  db <- generateMockVocabDb()
  dOut <- tempdir()
  importVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )
  # check overwrite by running again
  importVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE
  )
  # error as exists
  expect_error(
    importVocab(
      db = db,
      vocabularyDatabaseSchema = "main",
      dirOut = dOut,
      errorIfExists = TRUE
    )
  )
})
