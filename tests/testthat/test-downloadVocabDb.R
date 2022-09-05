test_that("import vocab check", {
  library(DBI)
  library(arrow)
  library(dplyr)
  library(CDMConnector)
  library(here)

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
  expect_true("concept.parquet" %in%
                list.files(dOut))
  # check overwrite by running again
  downloadVocab(
    cdm = cdm,
    dirOut = dOut,
    errorIfExists = FALSE
  )
  # error as exists
  expect_error(
    downloadVocab(
      cdm = cdm,
      dirOut = dOut,
      errorIfExists = TRUE
    )
  )

  # to folder that doesn't yet exist
  dOut <- here(tempdir(), "db")

  downloadVocab(
    cdm = cdm,
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )
  expect_true("concept.parquet" %in%
                list.files(dOut))
})
