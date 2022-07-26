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
#' @param dirOut Directory where output database will
#' be saved
#' @param errorIfExists Either TRUE or FALSE.
#' If TRUE, error thrown if output database already exists.
#' If FALSE, output database overwritten if it exists.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return SQLite database
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(RSQLite)
#' library(CodelistGenerator)
#' library(here)
#' db <- generateMockVocabDb()
#' dOut <- here()
#'
#' importVocab(
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
  }


  if (verbose == TRUE) {
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

  # throw error if db already exists
  if (errorIfExists == TRUE) {
    errorMessage <- checkmate::makeAssertCollection()
    dbPresenceCheck <- !file.exists(paste0(dirOut, "/vocab.sqlite"))
    checkmate::assertTRUE(dbPresenceCheck,
      add = errorMessage
    )
    if (!isTRUE(dbPresenceCheck)) {
      errorMessage$push(
        "- local database already exists"
      )
      errorMessage$push(
        "- to overwrite existing local database set errorIfExists=FALSE"
      )
    }
    checkmate::reportAssertions(collection = errorMessage)
  }

  if (errorIfExists != TRUE) {
    if (file.exists(paste0(dirOut, "/vocab.sqlite")) == TRUE) {
      unlink(paste0(dirOut, "/vocab.sqlite"))
    }
  }

  # start
  conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    paste0(dirOut, "/vocab.sqlite")
  )

  if (verbose == TRUE) {
  }
  concept <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept"
  ))) %>%
    dplyr::collect()
  DBI::dbWriteTable(conn, "concept", concept, overwrite = TRUE)
  rm(concept)

  if (verbose == TRUE) {
    message("Getting concept relationship table")
  }
  conceptRelationship <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_relationship"
  ))) %>%
    dplyr::collect()
  DBI::dbWriteTable(conn, "concept_relationship", conceptRelationship, overwrite = TRUE)
  rm(conceptRelationship)

  if (verbose == TRUE) {
    message("Getting concept ancestor table")
  }
  conceptAncestor <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_ancestor"
  ))) %>%
    dplyr::collect()
  DBI::dbWriteTable(conn, "concept_ancestor", conceptAncestor, overwrite = TRUE)
  rm(conceptAncestor)

  if (verbose == TRUE) {
    message("Getting concept synonym table")
  }
  conceptSynonym <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_synonym"
  ))) %>%
    dplyr::collect()
  DBI::dbWriteTable(conn, "concept_synonym", conceptSynonym, overwrite = TRUE)
  rm(conceptSynonym)

  if (verbose == TRUE) {
    message("Getting vocabulary table")
  }
  vocabulary <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.vocabulary"
  ))) %>%
    dplyr::collect()
  DBI::dbWriteTable(conn, "vocabulary", vocabulary, overwrite = TRUE)
  rm(vocabulary)

  if (verbose == TRUE) {
    message("Adding indexes")
  }
  DBI::dbExecute(conn,
   "CREATE UNIQUE INDEX idx_concept_concept_id ON concept (concept_id)")
  DBI::dbExecute(conn,
   "CREATE INDEX idx_concept_relationship_id_1 ON concept_relationship (concept_id_1, concept_id_2)")
  DBI::dbExecute(conn,
   "CREATE INDEX idx_concept_ancestor_id_1 ON concept_ancestor (ancestor_concept_id)")
  DBI::dbExecute(conn,
   "CREATE INDEX idx_concept_ancestor_id_2 ON concept_ancestor (descendant_concept_id)")
  DBI::dbExecute(conn,
   "CREATE INDEX idx_concept_synonym_id ON concept_synonym (concept_id)")

  DBI::dbDisconnect(conn)

  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }
}
