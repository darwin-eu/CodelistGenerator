#' Show mappings from source vocabularies to standard
#'
#' @param candidateCodelist Dataframe
#' @param sourceVocabularies Character vector
#' @param db Database connection via DBI::dbConnect()
#' @param vocabularyDatabaseSchema Name of database schema with vocab tables
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here ")
#' vocabularyDatabaseSchema <- " Your vocabulary schema here "
#' asthma_codes <- get_candidate_codes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabularyDatabaseSchema = " Your vocabulary schema here "
#' )
#' showMappings(
#'   candidateCodelist = asthma_codes,
#'   db = db,
#'   vocabularyDatabaseSchema = " Your vocabulary schema here "
#' )
#' }
showMappings <- function(candidateCodelist,
                         sourceVocabularies = c(
                           "ATC", "ICD10CM", "ICD10PCS",
                           "ICD9CM", "ICD9Proc",
                           "LOINC", "OPCS4", "Read",
                           "RxNorm", "RxNorm Extension",
                           "SNOMED"
                         ),
                         db,
                         vocabularyDatabaseSchema) {

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(sourceVocabularies, add = errorMessage)
  checkmate::assertDataFrame(candidateCodelist, add = errorMessage)
  dbInherits <- inherits(db, "DBIConnection")
  if (!isTRUE(dbInherits)) {
    errorMessage$push("db must be a database connection via DBI::dbConnect()")
  }
  checkmate::reportAssertions(collection = errorMessage)

  conceptDb <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".concept"
  )))
  conceptRelationshipDb <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".concept_relationship"
  )))
  # lowercase names
  conceptDb <- dplyr::rename_with(conceptDb, tolower)
  conceptRelationshipDb <- dplyr::rename_with(
    conceptRelationshipDb,
    tolower
  )

  # vocabs to lower case
  sourceVocabularies<-stringr::str_to_upper(sourceVocabularies)
  conceptDb<-conceptDb %>%
  mutate(vocabulary_id=stringr::str_to_upper(.data$vocabulary_id))

  # check sourceVocabularies exist
  errorMessage <- checkmate::makeAssertCollection()
  sourceVocabulariesInDb <- conceptDb %>%
    dplyr::select(.data$vocabulary_id) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()
  for (i in seq_along(sourceVocabularies)) {
    sourceVocabulariesCheck <- sourceVocabularies[i] %in% sourceVocabulariesInDb
    checkmate::assertTRUE(sourceVocabulariesCheck, add = errorMessage)
    if (!isTRUE(sourceVocabulariesCheck)) {
      errorMessage$push(
        glue::glue("- Vocabulary {sourceVocabularies[i]} not found in concept table")
      )
    }
  }
  checkmate::reportAssertions(collection = errorMessage)


  mappedCodes <- conceptDb %>%
    dplyr::inner_join(conceptRelationshipDb %>%
      dplyr::filter(.data$relationship_id == "Mapped from") %>%
      dplyr::filter(.data$concept_id_1 %in% !!candidateCodelist$concept_id) %>%
      dplyr::select("concept_id_1", "concept_id_2") %>%
      dplyr::rename("concept_id" = "concept_id_2"),
    by = c("concept_id")
    ) %>%
    dplyr::filter(.data$vocabulary_id %in% sourceVocabularies) %>%
    dplyr::distinct() %>%
    dplyr::collect()

  mappedCodes <- mappedCodes %>%
    dplyr::select(
      "concept_id_1", "concept_id",
      "concept_name", "concept_code",
      "vocabulary_id"
    )

  mappedCodes <- mappedCodes %>%
    dplyr::select("concept_id_1") %>%
    dplyr::rename("concept_id" = "concept_id_1") %>%
    dplyr::left_join(conceptDb %>%
      dplyr::filter(.data$concept_id %in% !!mappedCodes$concept_id_1) %>%
      dplyr::collect(),
    by = c("concept_id")
    ) %>%
    dplyr::select("concept_id", "concept_name", "vocabulary_id") %>%
    dplyr::rename("standard_vocabulary_id" = "vocabulary_id") %>%
    dplyr::rename("concept_id_1" = "concept_id") %>%
    dplyr::rename("standard_concept_name" = "concept_name") %>%
    dplyr::full_join(mappedCodes,
      by = "concept_id_1"
    ) %>%
    dplyr::rename("standard_concept_id" = "concept_id_1") %>%
    dplyr::rename("source_concept_id" = "concept_id") %>%
    dplyr::rename("source_concept_code" = "concept_code") %>%
    dplyr::rename("source_concept_name" = "concept_name") %>%
    dplyr::rename("source_vocabulary_id" = "vocabulary_id")

  mappedCodes %>%
    dplyr::distinct()
}
