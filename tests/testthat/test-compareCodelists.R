test_that("comparing two codelists", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- generateMockVocabDb()

  # tests
  codes1 <- getCandidateCodes(
    keywords = "Arthritis",
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )

  codes2 <- getCandidateCodes(
    keywords = c("knee osteoarthritis", "arthrosis"),
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )

  codesCompared <- compareCodelists(
    codelist1 = codes1,
    codelist2 = codes2
  )

  # tests
   expect_true(all(c(
     "concept_id",
     "concept_name",
     "domain_id",
     "vocabulary_id",
     "codelist"
   ) %in%
     names(codesCompared)))

   expect_true(codesCompared %>%
     filter(concept_id==3) %>%
     select(codelist) %>%
     pull() == "Only codelist 1")

   expect_true(codesCompared %>%
     filter(concept_id==5) %>%
     select(codelist) %>%
     pull() == "Only codelist 1")

   expect_true(codesCompared %>%
     filter(concept_id==4) %>%
     select(codelist) %>%
     pull() == "Both")

   expect_true(codesCompared %>%
     filter(concept_id==2) %>%
     select(codelist) %>%
     pull() == "Only codelist 2")

   #expected errors
   expect_error(compareCodelists(
    codelist1 = codes1,
    codelist2 = "a"
  ))
      expect_error(compareCodelists(
    codelist1 = "a",
    codelist2 = codes2
  ))


})
