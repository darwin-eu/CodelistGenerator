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
#' @param backend 'database' (duckdb), 'arrow' (parquet files), or 'data_frame'
#' @return cdm reference with mock vocabulary
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(CodelistGenerator)
#' db <- mockVocab("database")
#' }
mockVocabRef <- function(backend = "database") {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(backend %in% c("database", "arrow", "data_frame"))
  checkmate::assertTRUE(length(backend) == 1)
  checkmate::reportAssertions(collection = errorMessage)

  # tables
  concept <- data.frame(
    concept_id = 1:8,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Degenerative arthropathy",
      "Knee osteoarthritis",
      "H/O osteoarthritis"
    ),
    domain_id = c(rep("Condition",7), "Observation"),
    vocabulary_id = c(
      rep("SNOMED", 5),
      rep("Read", 2),
      "LOINC"
    ),
    standard_concept = c(
      rep("S", 5),
      rep(NA, 2),
      "S"
    ),
    concept_class_id = c(
      rep("Clinical Finding", 5),
      rep("Diagnosis", 2),
      "Observation"
    ),
    concept_code = "1234"
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  )
  conceptSynonym <- dplyr::bind_rows(
    data.frame(
    concept_id = 2L,
    concept_synonym_name = "Arthritis"
  ),
  data.frame(
    concept_id = 3L,
    concept_synonym_name = "Osteoarthrosis"
  )
  )
  conceptRelationship <- dplyr::bind_rows(
    data.frame(
      concept_id_1 = 2L,
      concept_id_2 = 6L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4L,
      concept_id_2 = 7L,
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

  # into in-memory duckdb
    db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

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

 cdm <- CDMConnector::cdm_from_con(db,
                             cdm_tables = tidyselect::all_of(c("concept",
                                                               "concept_relationship",
                                                               "concept_ancestor",
                                                               "concept_synonym",
                                                               "vocabulary")))
  if (backend == "database") {
    return(cdm)
  }

 if (backend %in%  c("arrow", "data_frame")) {
   dOut <- tempfile()
   dir.create(dOut)
   CDMConnector::stow(cdm, dOut)

   if (backend=="arrow") {
   cdm_arrow <- CDMConnector::cdm_from_files(path = dOut,
                                             cdm_tables = tidyselect::all_of(c("concept",
                                                                               "concept_relationship",
                                                                               "concept_ancestor",
                                                                               "concept_synonym",
                                                                               "vocabulary")),
                                             as_data_frame = FALSE)
   return(cdm_arrow)}

   if (backend=="data_frame") {
     cdm_data_frame <- CDMConnector::cdm_from_files(path = dOut,
                                               cdm_tables = tidyselect::all_of(c("concept",
                                                                                 "concept_relationship",
                                                                                 "concept_ancestor",
                                                                                 "concept_synonym",
                                                                                 "vocabulary")),
                                               as_data_frame = FALSE)
     return(cdm_data_frame)}

 }

}
