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
    any(mappings$standard_concept_name %in% "Osteoarthrosis")
  )
  expect_true(
    any(mappings$source_concept_name %in% "Degenerative arthropathy")
  )
  expect_true(
    any(mappings$standard_concept_name %in% "Osteoarthritis of knee")
  )
  expect_true(
    any(mappings$source_concept_name %in% "Knee osteoarthritis")
  )

  expect_true(all(c(
    "standard_concept_id",
    "standard_concept_name",
    "standard_vocabulary_id",
    "source_concept_id",
    "source_concept_name",
    "source_concept_code",
    "source_vocabulary_id"
  ) %in%
    names(mappings)))

  # expect error if not dbi connection
  expect_error(show_mappings(
    candidate_codelist = codes,
    db = "a",
    vocabulary_database_schema = "main"
  ))
  # expect error if vocabulary_database_schema does not exist
    expect_error(show_mappings(
    candidate_codelist = codes,
    db = db,
    vocabulary_database_schema = "a"
  ))

  DBI::dbDisconnect(db)
})
