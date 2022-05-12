test_that("mappings", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
                           package = "Eunomia"),
               open = "rb"),
    exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
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

  mappings <- show_mappings(
    candidate_codelist = codes,
    source_vocabularies = c("ATC"),
    db = db,
    vocabulary_database_schema = "main"
  )
  mappings2 <- show_mappings(
    candidate_codelist = codes,
    source_vocabularies = c(
      "ATC", "ICD10CM", "ICD10PCS",
      "ICD9CM", "ICD9Proc",
      "LOINC", "OPCS4", "Read",
      "RxNorm", "RxNorm Extension",
      "SNOMED"
    ),
    db = db,
    vocabulary_database_schema = "main"
  )
  expect_true(nrow(mappings2) >= nrow(mappings))

  # expect error if not dbi connection
  expect_error(show_mappings(
    candidate_codelist = codes,
    source_vocabularies = c(
      "ATC", "ICD10CM", "ICD10PCS",
      "ICD9CM", "ICD9Proc",
      "LOINC", "OPCS4", "Read",
      "RxNorm", "RxNorm Extension",
      "SNOMED"
    ),
    db = "a",
    vocabulary_database_schema = "main"
  ))
  DBI::dbDisconnect(db)
})
