test_that("db via DBI::dbConnect()", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"),
               open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(),
                       paste0(tempdir(), "\\cdm.sqlite"))
  expect_error(
    get_candidate_codes(
      keywords = "a",
      search_synonyms = TRUE,
      fuzzy_match = TRUE,
      exclude = NULL,
      include_descendants = TRUE,
      include_ancestor = FALSE,
      db = "a",
      vocabulary_database_schema = "main"
    )
  )
    DBI::dbDisconnect(db)
})

test_that("concept table names", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"), open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
  vocabulary_database_schema <- "main"
  concept_db <- dplyr::tbl(db,
                           dplyr::sql(
                             glue::glue(
                               "SELECT * FROM {vocabulary_database_schema}.
                               concept")))
  concept_ancestor_db <- dplyr::tbl(db,
                                    dplyr::sql(
                                      glue::glue(
                               "SELECT * FROM {vocabulary_database_schema}.
                                        concept_ancestor")))
  concept_synonym_db <- dplyr::tbl(db,
                                   dplyr::sql(
                                     glue::glue(
                                "SELECT * FROM {vocabulary_database_schema}.
                                       concept_synonym")))

  concept <- concept_db %>%
    dplyr::collect() %>%
    dplyr::rename("CONCEPT_PAT_ID" = "CONCEPT_ID")
  concept_ancestor <- concept_ancestor_db %>%
    dplyr::collect() %>%
    dplyr::rename("ANCESTOR_CONCEPT_PAT_ID" = "ANCESTOR_CONCEPT_ID")
  concept_synonym <- concept_synonym_db %>%
    dplyr::collect() %>%
    dplyr::rename("CONCEPT_PAT_ID" = "CONCEPT_ID")
  db_wrong_variable_names <- dbConnect(RSQLite::SQLite())
  DBI::dbWriteTable(db_wrong_variable_names, "concept", concept)
  DBI::dbWriteTable(db_wrong_variable_names, "concept_ancestor",
                    concept_ancestor)
  DBI::dbWriteTable(db_wrong_variable_names, "concept_synonym",
                    concept_synonym)
  expect_error(
    get_candidate_codes(
      keywords = "a",
      search_synonyms = TRUE,
      fuzzy_match = TRUE,
      exclude = NULL,
      include_descendants = TRUE,
      include_ancestor = FALSE,
      db = db_wrong_variable_names,
      vocabulary_database_schema = "main"
    )
  )
  DBI::dbDisconnect(db)
})



test_that("candidate codes variable names", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"),
               open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(),
                       paste0(tempdir(), "\\cdm.sqlite"))
  codes <- get_candidate_codes(
    keywords = "a",
    search_synonyms = TRUE,
    fuzzy_match = TRUE,
    exclude = NULL,
    include_descendants = TRUE,
    include_ancestor = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  # variable names
  expect_true(all(c(
    "concept_id", "concept_name", "domain_id",
    "vocabulary_id", "concept_id"
  ) %in%
    names(codes)))
    DBI::dbDisconnect(db)
})


test_that("check options", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"), open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(),
                       paste0(tempdir(), "\\cdm.sqlite"))
  # check search_synonyms
  codes1 <- get_candidate_codes(
    keywords = "asthma",
    search_synonyms = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  codes2 <- get_candidate_codes(
    keywords = "asthma",
    search_synonyms = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(codes2) >= nrow(codes1))

  # check exclusion of Childhood asthma
  codes3 <- get_candidate_codes(
    keywords = "asthma",
    search_synonyms = FALSE,
    db = db,
    vocabulary_database_schema = "main"
  )
  codes4 <- get_candidate_codes(
    keywords = "asthma",
    exclude = "Childhood asthma",
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(codes4) < nrow(codes3))

  # check empty candidate set
  codes5 <- get_candidate_codes(
    keywords = "Childhood asthmaX",
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_null(codes5)

  # check domains all exist
  expect_error(get_candidate_codes(
    keywords = "asthma",
    exclude = "Childhood asthma",
    domains = c("Condition", "Some other table"),
    db = db,
    vocabulary_database_schema = "main"
  ))
  # check verbose
  expect_message(get_candidate_codes(
    keywords = "asthma",
    exclude = "Childhood asthma",
    verbose = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  ))

  # all options used
  codes5 <- get_candidate_codes(
    keywords = "asthma",
    domains = c("Condition"),
    search_synonyms = TRUE,
    fuzzy_match = TRUE,
    max_distance_substitutions = 0.1,
    max_distance_insertions = 0.1,
    max_distance_deletions = 0.1,
    exclude = "Childhood asthma",
    include_descendants = TRUE,
    include_ancestor = TRUE,
    verbose = TRUE,
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(codes5) >= 1)
    DBI::dbDisconnect(db)
})
