test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- generate_mock_vocab_db()

  # tests
  # test keywords search - exact
  codes <- get_candidate_codes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    include_descendants = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true((nrow(codes) == 1 &
    codes$concept_name[1] == "Musculoskeletal disorder"))
  # variable names
  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id"
  ) %in%
    names(codes)))

  codes <- get_candidate_codes(
    keywords = c(
      "knee osteoarthritis",
      "hip osteoarthritis"
    ),
    domains = "Condition",
    include_descendants = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true((nrow(codes) == 2 &
    codes$concept_name[1] == "Osteoarthritis of knee" &
    codes$concept_name[2] == "Osteoarthritis of hip"))

  # test keywords search - fuzzy
  codes <- get_candidate_codes(
    keywords = c("Arthritis"),
    fuzzy_match = TRUE,
    max_distance_cost = 0.2,
    domains = "Condition",
    include_descendants = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(any(codes$concept_name %in% "Arthritis"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of knee"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of hip"))
  # with fuzzy, should pick up arthrosis
  expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

  # test include descendants
  codes <- get_candidate_codes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    include_descendants = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true((nrow(codes) == 5 &
    all(codes$concept_id %in% c(1:5)) &
    all(!codes$concept_id %in% c(6, 7))))

  # test include ancestor
  codes <- get_candidate_codes(
    keywords = c("Arthritis"),
    domains = "Condition",
    include_ancestor = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(any(codes$concept_name %in% "Musculoskeletal disorder"))

  codes <- get_candidate_codes(
    keywords = c("Osteoarthritis of knee"),
    domains = "Condition",
    include_ancestor = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  # nb include_ancestor should only include one level above
  expect_true(!any(codes$concept_name %in% "Musculoskeletal disorder"))
  expect_true(any(codes$concept_name %in% "Arthritis"))

  # test standard_concept
  codes <- get_candidate_codes(
    keywords = "Arthritis",
    domains = "Condition",
    standard_concept = c("Standard", "Non-standard"),
    include_descendants = TRUE,
    search_synonyms = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true((nrow(codes) == 4 &
    all(codes$concept_id %in% c(3, 4, 5, 7)) &
    all(!codes$concept_id %in% c(1, 2, 6))))

  # test search_synonyms
  codes <- get_candidate_codes(
    keywords = "arthritis",
    domains = "Condition",
    search_synonyms = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

  # test exclusion
  codes <- get_candidate_codes(
    keywords = "arthritis",
    exclude = "Osteoarthritis of hip",
    domains = "Condition",
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(any(!codes$concept_name %in% "Osteoarthritis of hip"))

  # test verbose
  expect_message(get_candidate_codes(
    keywords = "arthritis",
    domains = "Condition",
    verbose = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  ))

  ## Edge cases
  # check empty candidate set
  codes <- get_candidate_codes(
    keywords = "asthmaX",
    domains = "Condition",
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_null(codes)


  # all options used with exact
  codes <- get_candidate_codes(
    keywords = "arthritis",
    domains = "Condition",
    search_synonyms = TRUE,
    search_source = TRUE,
    fuzzy_match = FALSE,
    exclude = "Childhood asthma",
    include_descendants = TRUE,
    include_ancestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(codes) >= 1)

  # all options used with fuzzy
  codes <- get_candidate_codes(
    keywords = "Arthritis",
    exclude = "Osteoarthritis of hip",
    domains = "Condition",
    search_synonyms = TRUE,
    search_source = TRUE,
    fuzzy_match = TRUE,
    max_distance_cost = 0.1,
    include_descendants = TRUE,
    include_ancestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(codes) >= 1)





  ## Expected errors
  expect_error(get_candidate_codes(
    keywords = "a",
    search_synonyms = TRUE,
    fuzzy_match = TRUE,
    exclude = NULL,
    include_descendants = TRUE,
    include_ancestor = FALSE,
    db = "chr",
    vocabulary_database_schema = "main"
  ))

  expect_error(get_candidate_codes(
    keywords = "arthritis",
    domains = c("Condition", "Some other table"),
    db = db,
    vocabulary_database_schema = "main"
  ))


  DBI::dbDisconnect(db)
})
