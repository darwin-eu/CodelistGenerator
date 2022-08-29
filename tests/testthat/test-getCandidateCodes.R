test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab()

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
    searchViaSynonyms = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 4 &
    all(codes$concept_id %in% c(3, 4, 5, 7)) &
    all(!codes$concept_id %in% c(1, 2, 6))))


  # test searchInSynonyms
  codes <- getCandidateCodes(
    keywords = "osteoarthrosis",
    domains = "Condition",
    searchInSynonyms = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(codes$concept_name %in% "Arthritis"))

  # test searchViaSynonyms
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    searchViaSynonyms = TRUE,
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

  # test non-standard
  codes <- getCandidateCodes(
    keywords = c("Musculoskeletal", "Degenerative arthropathy"),
    searchNonStandard = TRUE,
    includeDescendants = FALSE,
    domains = "Condition",
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(any(codes$concept_name %in% "Osteoarthrosis"))

  codes <- getCandidateCodes(
    keywords = c("Degenerative arthropathy"),
    searchNonStandard = TRUE,
    fuzzyMatch = TRUE,
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
  expect_true(nrow(codes)==0)

  # all options used with exact
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = "Condition",
    conceptClassId = "Clinical Finding",
    standardConcept = "Standard",
    searchInSynonyms = TRUE,
    searchViaSynonyms = TRUE,
    searchNonStandard = TRUE,
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
    searchInSynonyms = TRUE,
    searchViaSynonyms = TRUE,
    searchNonStandard = TRUE,
    fuzzyMatch = TRUE,
    maxDistanceCost = 0.1,
    includeDescendants = TRUE,
    includeAncestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes) >= 1)



  ## Edge cases
  # keywords that don´t exist
  codes <- getCandidateCodes(
    keywords = c("Musculoskeletal disorder","XXXXX"),
    standardConcept = c("Standard"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true("1" %in% codes$concept_id)

  codes <- getCandidateCodes(
    keywords = "XXXXX",
    standardConcept = c("Standard"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes)==0)

  # conceptClassId that doesn´t exist
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    conceptClassId = c("clinical finding", "Something that doesn´t exist"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true("1" %in% codes$concept_id)

  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    conceptClassId = "Something that doesn´t exist",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes)==0)

  # domain that doesn´t exist
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = c("Condition", "Some other table"),
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes)>0)

  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = c("Some other table"),
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(nrow(codes)==0)


  ## Expected errors
  #keyword should be a character
  expect_error(getCandidateCodes(
    keywords = 35,
    standardConcept = c("Standard"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  ))

  expect_error(getCandidateCodes(
    keywords = "a",
    searchViaSynonyms = TRUE,
    fuzzyMatch = TRUE,
    exclude = NULL,
    includeDescendants = TRUE,
    includeAncestor = FALSE,
    db = "chr",
    vocabularyDatabaseSchema = "main"
  ))

  # standardConcept that doesn´t exist
  expect_error(getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    standardConcept = c("Standard", "Something that doesn´t exist"),
    includeDescendants = FALSE,
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


  DBI::dbDisconnect(db)


  # use duckdb instead of SQLite
  # where there is no vocabulary schema name
  # mock db
  db <- mockVocab(dbType = "duckdb")

  # tests
  # test keywords search - exact
  codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = NULL
  )
  DBI::dbDisconnect(db)
})

test_that("tests with mock arrow", {
  library(DBI)
  library(arrow)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab()

  dOut <- tempdir()
  downloadVocab(
    db = db,
    vocabularyDatabaseSchema = "main",
    dirOut = dOut,
    errorIfExists = FALSE,
    verbose = TRUE
  )

    codes <- getCandidateCodes(
    keywords = "Musculoskeletal disorder",
    domains = "Condition",
    includeDescendants = FALSE,
    arrowDirectory=dOut
  )
  expect_true((nrow(codes) == 1 &
    codes$concept_name[1] == "Musculoskeletal disorder"))


})

test_that("tests with mock db - multiple domains", {
  library(DBI)
  library(RSQLite)
  library(dbplyr)
  library(dplyr)

  # mock db
  db <- mockVocab()

  # tests
  # test keywords search - exact
  codes <- getCandidateCodes(
    keywords = "arthritis",
    domains = c("Condition","Observation"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true((nrow(codes) == 4 &
                 all(codes$concept_id %in% c(3:5, 8)) &
                 all(!codes$concept_id %in% c(1,2,6, 7))))

  codes <- getCandidateCodes(
    keywords = "H/O osteoarthritis",
    domains = c("Condition","Observation"),
    includeDescendants = FALSE,
    db = db,
    vocabularyDatabaseSchema = "main"
  )
  expect_true(all(nrow(codes) == 1 &
                 codes$concept_id == 8))

  DBI::dbDisconnect(db)

})


# test_that("tests with synthetic db", {
#   library(DBI)
#   library(odbc)
#   library(dbplyr)
#   library(dplyr)
# db <-DBI::dbConnect(odbc::odbc(),
#                       Driver   = "ODBC Driver 11 for SQL Server",
#                       Server   = Sys.getenv("darwinDbDatabaseServer"),
#                       Database = Sys.getenv("darwinDbDatabase"),
#                       UID      = Sys.getenv("darwinDbUser"),
#                       PWD      = Sys.getenv("darwinDbPassword"),
#                       Port     = Sys.getenv("darwinDbDatabasePort"))
#
# codes<-getCandidateCodes(
#     keywords = "Musculoskeletal disorder",
#     domains = "Condition",
#     includeDescendants = FALSE,
#     db = db,
#     vocabularyDatabaseSchema = Sys.getenv("darwinDbCdmSchema")
#   )
# expect_true(nrow(codes)>1)
#
# dbDisconnect(db)
# })
