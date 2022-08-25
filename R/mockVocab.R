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


#' Generate example vocabulary database
#'
#' @param dbType Character vector with type of in memory database.
#' Either 'SQL lite' or 'duckdb'
#' @return DBIConnection to SQLite database
#' with mock vocabulary
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(RSQLite)
#' library(CodelistGenerator)
#' db <- mockVocab()
#' }
mockVocab <- function(dbType = "SQLite") {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(dbType %in% c("SQLite", "duckdb"))
  checkmate::assertTRUE(length(dbType) == 1)
  checkmate::reportAssertions(collection = errorMessage)


  # tables
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
    concept_class_id = c(
      rep("Clinical Finding", 5),
      rep("Diagnosis", 2)
    ),
    concept_code = "1234"
  )
  conceptAncestor <- dplyr::bind_rows(
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
  conceptSynonym <- dplyr::bind_rows(
    data.frame(
    concept_id = 2,
    concept_synonym_name = "Arthritis"
  ),
  data.frame(
    concept_id = 3,
    concept_synonym_name = "Osteoarthrosis"
  )
  )
  conceptRelationship <- dplyr::bind_rows(
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
  vocabulary <- dplyr::bind_rows(
    data.frame(vocabulary_id = "SNOMED",
                           vocabulary_name = "SNOMED",
                           vocabulary_reference = "1",
                           vocabulary_version= "1",
                           vocabulary_concept_id=1),
    data.frame(vocabulary_id = "None",
               vocabulary_name = "OMOP Standardized Vocabularies",
               vocabulary_reference = "Omop generated",
               vocabulary_version= "v5.0 22-JUN-22",
               vocabulary_concept_id=44819096))

  # into in-memory databse
  if (dbType == "SQLite") {
    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  }
  if (dbType == "duckdb") {
    db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  }

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept",
      concept,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_ancestor",
      conceptAncestor,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_synonym",
      conceptSynonym,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_relationship",
      conceptRelationship,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "vocabulary",
      vocabulary,
      overwrite = TRUE
    )
  })

  return(db)
}
