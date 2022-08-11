# Copyright 2022 DARWIN EU (C)
#
# This file is part of CodelistGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Download OMOP vocabulary tables
#'
#' Download the OMOP vocabulary tables from a remote database
#' connection and save them as parquet files in the specified folder.
#'
#' @param conn Database connection via DBI::dbConnect()
#' @param schema Name of database schema containing vocab tables
#' @param dirOut Directory where output files will be saved
#' @param errorIfExist Either TRUE or FALSE.
#' If TRUE, an error will be thrown if vocab parquet files already exist.
#' If FALSE, existing vocab parquet files will be overwritten.
#' @param verbose Either TRUE or FALSE. If TRUE, progress messages will be reported.
#'
#' @return Invisibly returns TRUE. This function is called for its side effect
#' of creating parquet files in the specified directory.
#' @export
#'
#' @examples
#' \dontrun{
#' library(CodelistGenerator)
#' conn <- generateMockVocabDb()
#' dOut <- here()
#'
#' downloadVocab(
#'   conn = conn,
#'   schema = "main",
#'   dirOut = dOut
#' )
#' }
#'
downloadVocab <- function(conn,
                          schema,
                          dirOut,
                          errorIfExists = TRUE,
                          verbose = FALSE) {
  if (verbose) {
    start <- Sys.time()
    message("Checking inputs")
  }

  #TODO add a progress bar and download in batches? Downloads take a while.

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(inherits(conn, "DBIConnection"), add = errorMessage)
  checkmate::assertCharacter(schema, len = 1, add = errorMessage)
  checkmate::assertTRUE(file.exists(dirOut), add = errorMessage)
  checkmate::assert_logical(errorIfExists, add = errorMessage)
  checkmate::assert_logical(verbose, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # handle case where files already exist
  vocabTableNames <- c("concept", "concept_ancestor", "concept_synonym", "concept_relationship", "vocabulary")
  vocabPaths <- file.path(dirOut, paste0(vocabTableNames, ".parquet"))

  if (any(purrr::map_lgl(vocabPaths, file.exists))) {
    if (errorIfExists) {
      rlang::abort("- parquet output files already exists\n- to overwrite existing files set errorIfExists=FALSE")
    } else {
      purrr::walk(vocabPaths, unlink)
    }
  }

  if (verbose) message("Downloading concept table")

  concept <- DBI::dbGetQuery(conn, glue::glue("
    SELECT
      concept_id,
      concept_name,
      domain_id,
      vocabulary_id,
      standard_concept,
      concept_class_id,
      concept_code
    FROM {schema}.concept"))

  concept <- arrow::arrow_table(concept,
                                schema = arrow::schema(concept_id = double(),
                                                       concept_name = arrow::string(),
                                                       domain_id = arrow::string(),
                                                       vocabulary_id = arrow::string(),
                                                       standard_concept = arrow::string(),
                                                       concept_class_id = arrow::string(),
                                                       concept_code = arrow::string()))

  arrow::write_parquet(concept, file.path(dirOut, "concept.parquet"))
  rm(concept)

  if (verbose) message("Downloading concept_relationship table")

  conceptRelationship <- DBI::dbGetQuery(conn, glue::glue("
    SELECT
      concept_id_1,
      concept_id_2,
      relationship_id
    FROM {schema}.concept_relationship"))

  conceptRelationship <- arrow::arrow_table(conceptRelationship,
                                            schema = arrow::schema(concept_id_1 = double(),
                                                                   concept_id_2 = double(),
                                                                   relationship_id = arrow::string()))

  arrow::write_parquet(conceptRelationship,  file.path(dirOut, "concept_relationship.parquet"))
  rm(conceptRelationship)

  if (verbose) message("Downloading concept_ancestor table")

  conceptAncestor <- DBI::dbGetQuery(conn, glue::glue("
    SELECT
      ancestor_concept_id,
      descendant_concept_id,
      min_levels_of_separation,
      max_levels_of_separation
    FROM {schema}.concept_ancestor"))

  conceptAncestor <- arrow::arrow_table(conceptAncestor,
                                        schema = arrow::schema(ancestor_concept_id = double(),
                                                               descendant_concept_id = double(),
                                                               min_levels_of_separation = double(),
                                                               max_levels_of_separation = double()))

  arrow::write_parquet(conceptAncestor,  file.path(dirOut, "concept_ancestor.parquet"))
  rm(conceptAncestor)

  if (verbose) message("Downloading concept_synonym table")

  conceptSynonym <- DBI::dbGetQuery(conn, glue::glue("
    SELECT concept_id, concept_synonym_name
    FROM {schema}.concept_synonym"))

  conceptSynonym <- arrow::arrow_table(conceptSynonym,
                                       schema = arrow::schema(concept_id = double(),
                                                              concept_synonym_name = arrow::string()))

  arrow::write_parquet(conceptSynonym, file.path(dirOut, "concept_synonym.parquet"))
  rm(conceptSynonym)

  if (verbose) message("Downloading vocabulary table")

  vocabulary <- DBI::dbGetQuery(conn, glue::glue("
    SELECT
      vocabulary_id,
      vocabulary_name,
      vocabulary_reference,
      vocabulary_version,
      vocabulary_concept_id
    FROM {schema}.vocabulary"))

  vocabulary <- arrow::arrow_table(vocabulary,
                                   schema = arrow::schema(vocabulary_id = arrow::string(),
                                                          vocabulary_name = arrow::string(),
                                                          vocabulary_reference = arrow::string(),
                                                          vocabulary_version = arrow::string(),
                                                          vocabulary_concept_id=double()))

  arrow::write_parquet(vocabulary, file.path(dirOut, "vocabulary.parquet"))
  rm(vocabulary)

  if (verbose) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
  invisible(TRUE)
}
