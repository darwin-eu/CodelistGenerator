# Copyright 2022 DARWIN EU®
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
#' cdm <- mockVocabRef()
#' cdm
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
mockVocabRef <- function(backend = "database") {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(backend %in% c("database", "arrow", "data_frame"))
  checkmate::assertTRUE(length(backend) == 1)
  checkmate::reportAssertions(collection = errorMessage)

  # tables
  concept <- data.frame(
    concept_id = 1:10,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Degenerative arthropathy",
      "Knee osteoarthritis",
      "H/O osteoarthritis",
      "Adalimumab",
      "Injection"
    ),
    domain_id = c(rep("Condition", 7), "Observation", "Drug", "Drug"),
    vocabulary_id = c(
      rep("SNOMED", 5),
      rep("Read", 2),
      "LOINC", "RxNorm", "OMOP"
    ),
    standard_concept = c(
      rep("S", 5),
      rep(NA, 2),
      "S", "S", NA
    ),
    concept_class_id = c(
      rep("Clinical Finding", 5),
      rep("Diagnosis", 2),
      "Observation", "Ingredient", "Dose Form"
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
    ),
    data.frame(
      concept_id_1 = 9L,
      concept_id_2 = 10L,
      relationship_id = "RxNorm has dose form"
    )
  )
  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 9L,
      ingredient_concept_id = 9L,
      amount_value = NA,
      amount_unit_concept_id = 8576,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587,
      box_size = NA
    )
  )

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

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_strength",
      drugStrength,
      overwrite = TRUE
    )
  })
  cdm <- CDMConnector::cdm_from_con(db,
    cdm_tables = tidyselect::all_of(c(
      "concept",
      "concept_relationship",
      "concept_ancestor",
      "concept_synonym",
      "vocabulary",
      "drug_strength"
    ))
  )
  if (backend == "database") {
    return(cdm)
  }

  if (backend %in% c("arrow", "data_frame")) {
    dOut <- tempfile()
    dir.create(dOut)
    CDMConnector::stow(cdm, dOut)

    if (backend == "arrow") {
      cdmArrow <- CDMConnector::cdm_from_files(
        path = dOut,
        as_data_frame = FALSE
      )
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
      return(cdmArrow)
    }

    if (backend == "data_frame") {
      cdmDF <- CDMConnector::cdm_from_files(
        path = dOut,
        as_data_frame = TRUE
      )
      DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
      return(cdmDF)
    }
  }
}
