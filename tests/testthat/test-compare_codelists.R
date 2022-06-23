test_that("comparing two codelists", {
  library(DBI)
  library(RSQLite)
  library(dplyr)

  # mock db
  db <- generate_mock_vocab_db()

  # tests
  codes_1 <- get_candidate_codes(
    keywords = "Arthritis",
    domains = "Condition",
    include_descendants = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )

  codes_2 <- get_candidate_codes(
    keywords = c("knee osteoarthritis", "arthrosis"),
    domains = "Condition",
    include_descendants = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )

  codes_compared <- compare_codelists(
    codelist_1 = codes_1,
    codelist_2 = codes_2
  )

  # tests
   expect_true(all(c(
     "concept_id",
     "concept_name",
     "domain_id",
     "vocabulary_id",
     "codelist"
   ) %in%
     names(codes_compared)))

   expect_true(codes_compared %>%
     filter(concept_id==3) %>%
     select(codelist) %>%
     pull() == "Only codelist_1")

   expect_true(codes_compared %>%
     filter(concept_id==5) %>%
     select(codelist) %>%
     pull() == "Only codelist_1")

   expect_true(codes_compared %>%
     filter(concept_id==4) %>%
     select(codelist) %>%
     pull() == "Both")

   expect_true(codes_compared %>%
     filter(concept_id==2) %>%
     select(codelist) %>%
     pull() == "Only codelist_2")

   #expected errors
   expect_error(compare_codelists(
    codelist_1 = codes_1,
    codelist_2 = "a"
  ))
      expect_error(compare_codelists(
    codelist_1 = "a",
    codelist_2 = codes_2
  ))


})
