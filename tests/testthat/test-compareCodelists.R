test_that("comparing two codelists", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = "main",
                      select = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))

  # tests
  codes1 <- getCandidateCodes(cdm=cdm,
    keywords = "Arthritis",
    domains = "Condition",
    includeDescendants = TRUE
  )

  codes2 <- getCandidateCodes(cdm=cdm,
    keywords = c("knee osteoarthritis", "arthrosis"),
    domains = "Condition",
    includeDescendants = TRUE
  )

  codesCompared <- compareCodelists(
    codelist1 = codes1,
    codelist2 = codes2
  )

  # tests
  expect_true(all(c(
    "concept_id",
    "concept_name",
    "codelist"
  ) %in%
    names(codesCompared)))

  expect_true(codesCompared %>%
    filter(concept_id == 3) %>%
    select(codelist) %>%
    pull() == "Only codelist 1")

  expect_true(codesCompared %>%
    filter(concept_id == 5) %>%
    select(codelist) %>%
    pull() == "Only codelist 1")

  expect_true(codesCompared %>%
    filter(concept_id == 4) %>%
    select(codelist) %>%
    pull() == "Both")

  expect_true(codesCompared %>%
    filter(concept_id == 2) %>%
    select(codelist) %>%
    pull() == "Only codelist 2")

  # expected errors
  expect_error(compareCodelists(
    codelist1 = codes1,
    codelist2 = "a"
  ))
  expect_error(compareCodelists(
    codelist1 = "a",
    codelist2 = codes2
  ))
})

test_that("comparing two codelists- same codes found but in differnt ways", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)
  library(CDMConnector)

  # mock db
  db <- mockVocab()
  cdm <- cdm_from_con(con = db,cdm_schema = "main",
                      select = tidyselect::all_of(c("concept",
                                                    "concept_relationship",
                                                    "concept_ancestor",
                                                    "concept_synonym",
                                                    "vocabulary")))

  # tests
  codes1 <- getCandidateCodes(cdm=cdm,
    keywords = "Arthritis",
    domains = "Condition",
    includeDescendants = TRUE
  )

  codes2 <- getCandidateCodes(cdm=cdm,
    keywords = c("arthrosis"),
    searchInSynonyms = TRUE,
    domains = "Condition",
    includeDescendants = TRUE
  )

  codesCompared <- compareCodelists(
    codelist1 = codes1,
    codelist2 = codes2
  )

  # tests
  expect_true(nrow(codesCompared) == 4)

  expect_true(codesCompared %>%
                filter(concept_id == 3) %>%
                select(codelist) %>%
                pull() == "Both")

  expect_true(codesCompared %>%
                filter(concept_id == 4) %>%
                select(codelist) %>%
                pull() == "Both")

  expect_true(codesCompared %>%
                filter(concept_id == 5) %>%
                select(codelist) %>%
                pull() == "Both")

  expect_true(codesCompared %>%
                filter(concept_id == 2) %>%
                select(codelist) %>%
                pull() == "Only codelist 2")


})
