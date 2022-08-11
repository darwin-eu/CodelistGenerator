

#' Create a list of references to remote OMOP vocab tables
#'
#' @param con A database connection
#' @param schema The schema where the vocab tables are located. Defaults to NULL.
#'
#' @return A list of dplyr database table references pointing to 5 vocabulary tables
#' @export
vocabRefFromDatabase <- function(con, schema = NULL) {

  checkmate::assertClass(con, "DBIConnection")
  checkmate::assertCharacter(schema, null.ok = TRUE)

  vocabTableNames <- c("concept", "concept_ancestor", "concept_synonym", "concept_relationship", "vocabulary")

  if (!is.null(schema)) {
    vocab <- purrr::map(vocabTableNames, ~dplyr::tbl(con, dbplyr::in_schema(schema, .)))
  } else {
    vocab <- purrr::map(vocabTableNames, ~dplyr::tbl(con, .))
  }

  vocab %>%
    magrittr::set_names(vocabTableNames) %>%
    magrittr::set_class("VocabReference") %>%
    assertVocabColumnNames()
}

#' Create a list of vocab tables from a directory of parquet files
#'
#' @param path Directory containing  5 vocabulary parquet files:
#' "concept", "concept_ancestor", "concept_synonym", "concept_relationship", "vocabulary" all with
#' the .parquet extension
#'
#' @return A list of 5 Arrow Tables
#' @export
vocabRefFromFiles <- function(path) {
  checkmate::assertCharacter(path, len = 1)

  vocabTableNames <- c("concept", "concept_ancestor", "concept_synonym", "concept_relationship", "vocabulary")
  vocabPaths <- file.path(path, paste0(vocabTableNames, ".parquet"))

  checkmate::assertTRUE(file.exists(path))
  checkmate::assertTRUE(all(purrr::map_lgl(vocabPaths, file.exists)))

  purrr::map(vocabPaths, arrow::read_parquet, as_data_frame = FALSE) %>%
    magrittr::set_names(vocabTableNames) %>%
    magrittr::set_class("VocabReference") %>%
    assertVocabColumnNames()
}

assertVocabColumnNames <- function(vocabReference) {

  checkmate::assertSetEqual(names(vocabReference),
    c("concept", "concept_ancestor", "concept_synonym", "concept_relationship", "vocabulary"))

  checkmate::assertSetEqual(names(vocabReference$concept),
    c("concept_id", "concept_name", "domain_id", "vocabulary_id", "standard_concept"))

  checkmate::assertSetEqual(names(vocabReference$concept_ancestor),
    c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"))

  checkmate::assertSetEqual(names(vocabReference$concept_synonym),
    c("concept_id", "concept_synonym_name"))

  checkmate::assertSetEqual(names(vocabReference$concept_relationship),
    c("concept_id_1", "concept_id_2", "relationship_id"))

  return(vocabReference)
}
