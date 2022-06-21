test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dplyr)

  # mock db
  db <- generate_mock_vocab_db()

  # tests
  codes <- get_candidate_codes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    include_descendants = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  mappings <- show_mappings(
    candidate_codelist = codes,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(
    any(mappings$`Standard concept_id name` %in% "Osteoarthrosis")
  )
  expect_true(
    any(mappings$`Source name` %in% "Degenerative arthropathy")
  )
  expect_true(
    any(mappings$`Standard concept_id name` %in% "Osteoarthritis of knee")
  )
  expect_true(
    any(mappings$`Source name` %in% "Knee osteoarthritis")
  )

  expect_true(all(c(
    "Standard concept_id (mapped to)",
    "Standard concept_id name",
    "Standard vocabulary",
    "Source concept_id (mapped from)",
    "Source name",
    "Source code",
    "Source vocabulary"
  ) %in%
    names(mappings)))

  # expect error if not dbi connection
  expect_error(show_mappings(
    candidate_codelist = codes,
    db = "a",
    vocabulary_database_schema = "main"
  ))


  DBI::dbDisconnect(db)
})
