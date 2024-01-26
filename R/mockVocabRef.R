# Copyright 2023 DARWIN EU®
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
#' @param backend 'database' (duckdb) or 'data_frame'
#' @return cdm reference with mock vocabulary
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef()
#' cdm
#' CDMConnector::cdmDisconnect(cdm)
#' }
mockVocabRef <- function(backend = "database") {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(backend %in% c("database", "data_frame"))
  checkmate::assertTRUE(length(backend) == 1)
  checkmate::reportAssertions(collection = errorMessage)

  # compulsory tables
  person <- dplyr::tibble(
    person_id = 1, gender_concept_id = 0, year_of_birth = 1990,
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1, person_id = 1,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2025-12-31"),
    period_type_concept_id = 0
  )

  # vocab tables
  concept <- data.frame(
    concept_id = 1:19,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Osteonecrosis",
      "Degenerative arthropathy",
      "Knee osteoarthritis",
      "H/O osteoarthritis",
      "Adalimumab",
      "Injection",
      "ALIMENTARY TRACT AND METABOLISM",
      "Descendant drug",
      "Injectable",
      "Diseases of the musculoskeletal system and connective tissue",
      "Arthropathies",
      "Arthritis",
      "OA",
      "Other ingredient"
    ),
    domain_id = c(rep("Condition", 8), "Observation", rep("Drug", 5),
                  rep("Condition", 4), "Drug"),
    vocabulary_id = c(
      rep("SNOMED", 6),
      rep("Read", 2),
      "LOINC", "RxNorm", "OMOP",
      "ATC",
      "RxNorm", "OMOP",
      "ICD10", "ICD10", "ICD10", "ICD10", "RxNorm"
    ),
    standard_concept = c(
      rep("S", 6),
      rep(NA, 2),
      "S", "S", NA,
      NA, "S", NA, NA, NA, NA, NA, "S"
    ),
    concept_class_id = c(
      rep("Clinical Finding", 6),
      rep("Diagnosis", 2),
      "Observation", "Ingredient", "Dose Form",
      "ATC 1st", "Drug", "Dose Form",
      "ICD10 Chapter", "ICD10 SubChapter",
      "ICD Code","ICD Code", "Ingredient"
    ),
    concept_code = "1234",
    valid_start_date = NA,
    valid_end_date = NA,
    invalid_reason =
      NA
  )
  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
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
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
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
  )%>%
    dplyr::mutate(language_concept_id  = NA)
  conceptRelationship <- dplyr::bind_rows(
    data.frame(
      concept_id_1 = 2L,
      concept_id_2 = 7L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4L,
      concept_id_2 = 8L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 10L,
      concept_id_2 = 11L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 3L,
      concept_id_2 = 6L,
      relationship_id = "Due to of"
    ),
    data.frame(
      concept_id_1 = 13L,
      concept_id_2 = 14L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 15L,
      concept_id_2 = 16L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 16L,
      concept_id_2 = 17L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 17L,
      concept_id_2 = 18L,
      relationship_id = "Subsumes"
    ),
    data.frame(
      concept_id_1 = 18L,
      concept_id_2 = 3L,
      relationship_id = "Maps to"
    )
  ) %>%
    dplyr::mutate(valid_start_date = NA,
                  valid_end_date = NA,
                  invalid_reason = NA)
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
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA,
      amount_unit_concept_id = 8576,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587,
      box_size = NA,
      valid_start_date = NA,
      valid_end_date = NA
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA,
      cdm_holder = NA,
      source_description = NA,
      source_documentation_reference = NA,
      cdm_etl_reference = NA,
      source_release_date = NA,
      cdm_release_date = NA,
      cdm_version = "5.3",
      vocabulary_version  = NA
    )
  )

  # achilles tables
  # count of 400 records for knee osteoarthritis
  achillesAnalysis <- dplyr::tibble(analysis_id = 1,
                                    analysis_name = 1)
  achillesResults <- dplyr::tibble(analysis_id = 401,
                             stratum_1 = 4,
                             stratum_2 = NA,
                             stratum_3 = NA,
                             count_value = 100)
  achillesResultsDist <- dplyr::tibble(analysis_id = 1,
                                  count_value = 5)

  # into in-memory duckdb
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
                      person,
                      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period",
                      observationPeriod,
                      overwrite = TRUE
    )
  })
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
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "cdm_source",
                      cdmSource,
                      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "achilles_analysis",
                      achillesAnalysis,
                      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "achilles_results",
                      achillesResults,
                      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "achilles_results_dist",
                      achillesResultsDist,
                      overwrite = TRUE
    )
  })

  cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = "main",
                                    write_schema = "main",
                                    cdm_name = "mock")

  cdm$achilles_analysis <- dplyr::tbl(db, "achilles_analysis")
  cdm$achilles_results <- dplyr::tbl(db, "achilles_results")
  cdm$achilles_results_dist <- dplyr::tbl(db, "achilles_results_dist")

  if (backend %in% c("data_frame")) {
    cdm <- cdm %>% dplyr::collect()
  }

  cdm
}
