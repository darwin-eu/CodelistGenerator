# Copyright 2022 DARWIN EU (C)
#
# This file is part of IncidencePrevalence
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


#' Import vocabulary tables
#'
#' @param db Database connection via DBI::dbConnect()
#' @param vocabularyDatabaseSchema Name of database
#' schema with vocab tables
#' @param dirOut Directory where output files will
#' be saved
#' @param errorIfExists Either TRUE or FALSE.
#' If TRUE, error thrown if output database already exists.
#' If FALSE, output database overwritten if it exists.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return Apache arrow parquet files
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(Arrow)
#' library(CodelistGenerator)
#' library(here)
#' db <- generateMockVocabDb()
#' dOut <- here()
#'
#' importVocabAsArrow(
#'   db = db,
#'   vocabularyDatabaseSchema = "main",
#'   dirOut = dOut
#' )
#' }
#'
importVocab <- function(db,
                               vocabularyDatabaseSchema,
                               dirOut,
                               errorIfExists = TRUE,
                               verbose = FALSE) {
  if (verbose == TRUE) {
    start <- Sys.time()
    message("Checking inputs")
  }

  errorMessage <- checkmate::makeAssertCollection()
  dbInheritsCheck <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(dbInheritsCheck,
    add = errorMessage
  )
  checkmate::assertVector(vocabularyDatabaseSchema,
    add = errorMessage
  )
  dirOutCheck <- file.exists(dirOut)
  checkmate::assertTRUE(dirOutCheck,
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  # throw error if files already exists
  if (errorIfExists == TRUE) {
    errorMessage <- checkmate::makeAssertCollection()
    parquetPresenceCheck <- any(
      file.exists(glue::glue("{dirOut}/concept.parquet")),
      file.exists(glue::glue("{dirOut}/concept_ancestor.parquet")),
      file.exists(glue::glue("{dirOut}/concept_relationship.parquet")),
      file.exists(glue::glue("{dirOut}/concept_synonym.parquet")),
      file.exists(glue::glue("{dirOut}/vocabulary.parquet"))
    )
    checkmate::assertFALSE(parquetPresenceCheck,
      add = errorMessage
    )
    if (isTRUE(parquetPresenceCheck)) {
      errorMessage$push(
        "- parquet output files already exists"
      )
      errorMessage$push(
        "- to overwrite existing files set errorIfExists=FALSE"
      )
    }
    checkmate::reportAssertions(collection = errorMessage)
  }

  if (errorIfExists == FALSE) {
    if (any(
      file.exists(glue::glue("{dirOut}/concept.parquet")),
      file.exists(glue::glue("{dirOut}/concept_ancestor.parquet")),
      file.exists(glue::glue("{dirOut}/concept_synonym.parquet")),
      file.exists(glue::glue("{dirOut}/concept_relationship.parquet")),
      file.exists(glue::glue("{dirOut}/vocabulary.parquet"))
    ) == TRUE) {
      unlink(glue::glue("{dirOut}/concept.parquet"))
      unlink(glue::glue("{dirOut}/concept_ancestor.parquet"))
      unlink(glue::glue("{dirOut}/concept_relationship.parquet"))
      unlink(glue::glue("{dirOut}/concept_synonym.parquet"))
      unlink(glue::glue("{dirOut}/vocabulary.parquet"))
    }
  }

  # start
  if (verbose == TRUE) {
   message("Getting concept table")
  }
  concept <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept"
  ))) %>%
    dplyr::select(dplyr::all_of(c(
      "concept_id", "concept_name",
      "domain_id", "vocabulary_id", "standard_concept",
      "concept_class_id","concept_code"))) %>%
    dplyr::collect()
  concept <- arrow::arrow_table(concept,
              schema=arrow::schema(
  concept_id = double(),
  concept_name = arrow::string(),
  domain_id = arrow::string(),
  vocabulary_id = arrow::string(),
  standard_concept = arrow::string(),
  concept_class_id = arrow::string(),
  concept_code=arrow::string()))
  arrow::write_parquet(concept, glue::glue("{dirOut}/concept.parquet"))
  rm(concept)

  if (verbose == TRUE) {
    message("Getting concept relationship table")
  }
  conceptRelationship <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_relationship"
  ))) %>%
    dplyr::select(dplyr::all_of(c(
      "concept_id_1", "concept_id_2",
      "relationship_id"))) %>%
    dplyr::collect()
  conceptRelationship <- arrow::arrow_table(conceptRelationship,
              schema=arrow::schema(
  concept_id_1 = double(),
  concept_id_2 = double(),
  relationship_id = arrow::string()))
  arrow::write_parquet(conceptRelationship,  glue::glue("{dirOut}/concept_relationship.parquet"))
  rm(conceptRelationship)

  if (verbose == TRUE) {
    message("Getting concept ancestor table")
  }
  conceptAncestor <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_ancestor"
  ))) %>%
    dplyr::select(dplyr::all_of(c(
      "ancestor_concept_id", "descendant_concept_id",
      "min_levels_of_separation",
      "max_levels_of_separation"))) %>%
    dplyr::collect()
  conceptAncestor <- arrow::arrow_table(conceptAncestor,
              schema=arrow::schema(
  ancestor_concept_id = double(),
  descendant_concept_id = double(),
  min_levels_of_separation = double(),
  max_levels_of_separation = double()))
  arrow::write_parquet(conceptAncestor,  glue::glue("{dirOut}/concept_ancestor.parquet"))
  rm(conceptAncestor)

  if (verbose == TRUE) {
    message("Getting concept synonym table")
  }
  conceptSynonym <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_synonym"
  ))) %>%
    dplyr::select(dplyr::all_of(c(
      "concept_id",
      "concept_synonym_name"))) %>%
    dplyr::collect()
  conceptSynonym <- arrow::arrow_table(conceptSynonym,
              schema=arrow::schema(
  concept_id = double(),
  concept_synonym_name = arrow::string()))
  arrow::write_parquet(conceptSynonym,  glue::glue("{dirOut}/concept_synonym.parquet"))
  rm(conceptSynonym)

  if (verbose == TRUE) {
    message("Getting vocabulary table")
  }
  vocabulary <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.vocabulary"
  ))) %>%
    dplyr::select(dplyr::all_of(c(
      "vocabulary_id", "vocabulary_name" ,
      "vocabulary_reference" , "vocabulary_version",
      "vocabulary_concept_id"))) %>%
    dplyr::collect()
  vocabulary <- arrow::arrow_table(vocabulary,
              schema=arrow::schema(
  vocabulary_id = arrow::string(),
  vocabulary_name = arrow::string(),
  vocabulary_reference = arrow::string(),
  vocabulary_version = arrow::string(),
  vocabulary_concept_id=double()))
  arrow::write_parquet(vocabulary,  glue::glue("{dirOut}/vocabulary.parquet"))
  rm(vocabulary)

  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
}
