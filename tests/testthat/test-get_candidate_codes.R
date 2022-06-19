test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dplyr)

  # mock db
  concept <- data.frame(
    concept_id = 1:7,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Degenerative arthropathy",
      "Knee osteoarthritis"
    ),
    domain_id = "Condition",
    vocabulary_id = c(
      rep("SNOMED", 5),
      rep("Read", 2)
    ),
    standard_concept = c(
      rep("S", 5),
      rep(NA, 2)
    ),
    concept_code = NA
  )
  concept_ancestor <- bind_rows(
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 2,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 3,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 4,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 5,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 3,
      descendant_concept_id = 4,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3,
      descendant_concept_id = 5,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  )
  concept_synonym <- data.frame(
    concept_id = 3,
    concept_synonym_name = "Osteoarthrosis"
  )
  concept_relationship <- bind_rows(
    data.frame(
      concept_id_1 = 2,
      concept_id_2 = 6,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4,
      concept_id_2 = 7,
      relationship_id = "Mapped from"
    )
  )
  db <- dbConnect(RSQLite::SQLite(), ":memory:")
  dbWithTransaction(db, {
    dbWriteTable(db, "concept",
                 concept, overwrite = TRUE)
  })
  dbWithTransaction(db, {
    dbWriteTable(db, "concept_ancestor",
                 concept_ancestor, overwrite = TRUE)
  })
  dbWithTransaction(db, {
    dbWriteTable(db, "concept_synonym",
                 concept_synonym, overwrite = TRUE)
  })
  dbWithTransaction(db, {
    dbWriteTable(db, "concept_relationship",
                 concept_relationship, overwrite = TRUE)
  })


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
    domains = "Condition",
    include_descendants = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(any(codes$concept_name %in% "Arthritis"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of knee"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of hip"))
  # with fuzzy, should pick up arthrosis
  # expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

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
  # TO ADD - need to add into mock db
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
    max_distance_substitutions = 0.1,
    max_distance_insertions = 0.1,
    max_distance_deletions = 0.1,
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
