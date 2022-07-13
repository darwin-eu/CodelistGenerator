test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- generateMockVocabDb()

  # tests
  # test keywords search - exact
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 1 &
    codes$concept_name[1] == "Musculoskeletal disorder"))
  # variable names
  expect_true(all(c(
    "concept_id", "concept_name",
    "domain_id", "vocabulary_id"
  ) %in%
    names(codes)))

  codes <- getCandidateCodes(
    keywords = c(
      "knee osteoarthritis",
      "hip osteoarthritis"
    ),
    domains = "Condition",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 2 &
    codes$concept_name[1] == "Osteoarthritis of knee" &
    codes$concept_name[2] == "Osteoarthritis of hip"))

  # test keywords search - fuzzy
  codes <- getCandidateCodes(
    keywords = c("Arthritis"),
    fuzzyMatch = TRUE,
    maxDistanceCost = 0.2,
    domains = "Condition",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(codes$concept_name %in% "Arthritis"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of knee"))
  expect_true(any(codes$concept_name %in% "Osteoarthritis of hip"))
  # with fuzzy, should pick up arthrosis
  expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

  # test include descendants
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 5 &
    all(codes$concept_id %in% c(1:5)) &
    all(!codes$concept_id %in% c(6, 7))))

  # test include ancestor
  codes <- getCandidateCodes(
    keywords = c("Arthritis"),
    domains = "Condition",
    includeAncestor = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(codes$concept_name %in% "Musculoskeletal disorder"))

  codes <- getCandidateCodes(
    keywords = c("Osteoarthritis of knee"),
    domains = "Condition",
    includeAncestor = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  # nb includeAncestor should only include one level above
  expect_true(!any(codes$concept_name %in% "Musculoskeletal disorder"))
  expect_true(any(codes$concept_name %in% "Arthritis"))

  # test standardConcept
  codes <- getCandidateCodes(
    keywords = "Arthritis",
    domains = "Condition",
    standardConcept = c("Standard", "Non-standard"),
    includeDescendants = TRUE,
    searchSynonyms = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 4 &
    all(codes$concept_id %in% c(3, 4, 5, 7)) &
    all(!codes$concept_id %in% c(1, 2, 6))))

  # test searchSynonyms
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    searchSynonyms = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

  # test exclusion
  codes <- getCandidateCodes(
    keywords = "arthritis",
    exclude = "Osteoarthritis of hip",
    domains = "Condition",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(!codes$concept_name %in% "Osteoarthritis of hip"))

  # test source
codes <- getCandidateCodes(
    keywords = c("Musculoskeletal","Degenerative arthropathy"),
    searchSource = TRUE,
    includeDescendants = FALSE,
    domains = "Condition",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

codes <- getCandidateCodes(
    keywords = c("Musculoskeletal","Degenerative arthropathy"),
    searchSource = TRUE,
    fuzzyMatch=TRUE,
    includeDescendants = FALSE,
    domains = "Condition",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

# test verbose
  expect_message(getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    verbose = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

  ## Edge cases
  # check empty candidate set
  codes <- getCandidateCodes(
    keywords = "asthmaX",
    domains = "Condition",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_null(codes)


  # all options used with exact
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    conceptClassId = "Clinical Finding",
    standardConcept = "Standard",
    searchSynonyms = TRUE,
    searchSource = TRUE,
    fuzzyMatch = FALSE,
    exclude = "Childhood asthma",
    includeDescendants = TRUE,
    includeAncestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes) >= 1)

  # all options used with fuzzy
  codes <- getCandidateCodes(
    keywords = "Arthritis",
    exclude = "Osteoarthritis of hip",
    domains = "Condition",
    conceptClassId = "Clinical Finding",
    standardConcept = "Standard",
    searchSynonyms = TRUE,
    searchSource = TRUE,
    fuzzyMatch = TRUE,
    maxDistanceCost = 0.1,
    includeDescendants = TRUE,
    includeAncestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes) >= 1)





  ## Expected errors
  expect_error(getCandidateCodes(
    keywords = "a",
    searchSynonyms = TRUE,
    fuzzyMatch = TRUE,
    exclude = NULL,
    includeDescendants = TRUE,
    includeAncestor = FALSE,
    db = "chr",
    vocabularyDatabaseSchema = "main"
  ))

  expect_error(getCandidateCodes(
    keywords = "arthritis",
    domains = c("Condition", "Some other table"),
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

  expect_error(getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    conceptClassId = "Something that doesn´t exist",
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

   expect_error(getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    standardConcept = "Something that doesn´t exist",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

   # expect error - no combination of standardConcept and conceptClassId
   expect_error(getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    standardConcept = "Non-standard",
    conceptClassId = "Clinical Finding",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

expect_error(getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    standardConcept="Classification", #not in our mock db
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  ))


  DBI::dbDisconnect(db)
})
