# Copyright 2025 DARWIN EU®
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


#' Generate example vocabulary database
#'
#' @param backend 'database' (duckdb) or 'data_frame'.
#' @return cdm reference with mock vocabulary.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' cdm
#' }
mockVocabRef <- function(backend = "data_frame") {

  # initial checks
  omopgenerics::assertChoice(backend,
                             choices = c("database", "data_frame"),
                             length = 1)

  if(backend == "database"){
    rlang::check_installed("duckdb")
  }

  # compulsory tables
  person <- dplyr::tibble(
    person_id = c(1L:400L), gender_concept_id = 0L, year_of_birth = 1990L,
    race_concept_id = 0L, ethnicity_concept_id = 0L
  )
  observationPeriod <- dplyr::tibble(
    observation_period_id = 1L, person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2024-12-31"),
    period_type_concept_id = 0L
  )

  # condition accurrence table
  conditionOccurrence <- rbind(
    dplyr::tibble(
      condition_occurrence_id = c(1L:400L),
      condition_concept_id = 4L,
      person_id = c(1L:400L),
      condition_start_date = as.Date("2010-01-01"),
      condition_end_date = as.Date("2010-01-01"),
      condition_type_concept_id = NA_integer_,
      condition_source_concept_id = NA_integer_,
      condition_source_value = NA_character_
    ),
    dplyr::tibble(
      condition_occurrence_id = c(201L:400L),
      condition_concept_id = 5L,
      person_id = c(1L:200L),
      condition_start_date = as.Date("2010-01-01"),
      condition_end_date = as.Date("2010-01-01"),
      condition_type_concept_id = NA_integer_,
      condition_source_concept_id = NA_integer_,
      condition_source_value = NA_character_
    ),
    dplyr::tibble(
      condition_occurrence_id = c(401L:500L),
      condition_concept_id = 9L,
      person_id = c(1L:100L),
      condition_start_date = as.Date("2010-01-01"),
      condition_end_date = as.Date("2010-01-01"),
      condition_type_concept_id = NA_integer_,
      condition_source_concept_id = NA_integer_,
      condition_source_value = NA_character_
    )
  )

  # vocab tables
  concept <- data.frame(
    concept_id = c(1:10, 46234469, 12:13, 44784844, 15:21 , 35604877L, 35604394L, 22:24, 25:26) |>
      as.integer(),
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
      "Injectable Foam",
      "Diseases of the musculoskeletal system and connective tissue",
      "Arthropathies",
      "Arthritis",
      "OA",
      "Other ingredient",
      "glucagon Nasal Powder",
      "nitrogen Topical Liquefied Gas",
      "Nasal Powder",
      "Topical Liquefied Gas",
      "percent",
      "milligram",
      "Other musculoskeletal disorder",
      "Brand 1",
      "Brand 2"
    ),
    domain_id = c(rep("Condition", 8), "Observation", rep("Drug", 5),
                  rep("Condition", 4), rep("Drug",5), rep("Unit",2),
                  "Condition",
                  rep("Drug", 2)),
    vocabulary_id = c(
      rep("SNOMED", 6),
      rep("Read", 2),
      "LOINC", "RxNorm", "OMOP",
      "ATC",
      "RxNorm", "OMOP",
      rep("ICD10",4),
      rep("RxNorm",5),
      rep("UCUM",2),
      "SNOMED",
      rep("RxNorm Extension",2)
    ),
    standard_concept = c(
      rep("S", 6),
      rep(NA, 2),
      "S", "S", NA,
      NA, "S",
      rep(NA,5),
      rep("S",3),
      rep(NA,2),
      rep("S",2),
      rep(NA,3)
    ),
    concept_class_id = c(
      rep("Clinical Finding", 6),
      rep("Diagnosis", 2),
      "Observation", "Ingredient", "Dose Form",
      "ATC 1st", "Drug", "Dose Form",
      "ICD10 Chapter", "ICD10 SubChapter",
      "ICD Code","ICD Code", "Ingredient",
      rep("Clinical Drug Form",2),
      rep("Dose Form",2),
      rep("Unit",2),
      "Clinical Finding",
      rep("Brand Name",2)
    ),
    concept_code = "1234",
    valid_start_date = as.Date(NA),
    valid_end_date = as.Date(NA),
    invalid_reason = NA_character_
  )

  conceptAncestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 1L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 2L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 3L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 1L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 2L,
      max_levels_of_separation = 2L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 4L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 3L,
      descendant_concept_id = 5L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 10L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 10L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 12L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 12L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
    ),
    data.frame(
      ancestor_concept_id = 19L,
      descendant_concept_id = 13L,
      min_levels_of_separation = 1L,
      max_levels_of_separation = 1L
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
  )|>
    dplyr::mutate(language_concept_id  = NA_integer_)

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
      concept_id_2 = 46234469L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 3L,
      concept_id_2 = 6L,
      relationship_id = "Due to of"
    ),
    data.frame(
      concept_id_1 = 13L,
      concept_id_2 = 44784844L,
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
    ),
    data.frame(
      concept_id_1 = 20L,
      concept_id_2 = 35604877L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 21L,
      concept_id_2 = 35604394L,
      relationship_id = "RxNorm has dose form"
    ),
    data.frame(
      concept_id_1 = 1L,
      concept_id_2 = 24L,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 10L,
      concept_id_2 = 25L,
      relationship_id = "Has brand name"
    ),
    data.frame(
      concept_id_1 = 20L,
      concept_id_2 = 25L,
      relationship_id = "Has brand name"
    ),
    data.frame(
      concept_id_1 = 20L,
      concept_id_2 = 26L,
      relationship_id = "Has brand name"
    ),
    data.frame(
      concept_id_1 = 13L,
      concept_id_2 = 26L,
      relationship_id = "Has brand name"
    ),
    data.frame(
      concept_id_1 = 21L,
      concept_id_2 = 26L,
      relationship_id = "Has brand name"
    )
  ) |>
    dplyr::mutate(valid_start_date = as.Date(NA),
                  valid_end_date = as.Date(NA),
                  invalid_reason = NA_character_)

  vocabulary <- dplyr::bind_rows(
    data.frame(
      vocabulary_id = "SNOMED",
      vocabulary_name = "SNOMED",
      vocabulary_reference = "1",
      vocabulary_version = "1",
      vocabulary_concept_id = 1L
    ),
    data.frame(
      vocabulary_id = "None",
      vocabulary_name = "OMOP Standardized Vocabularies",
      vocabulary_reference = "Omop generated",
      vocabulary_version = "v5.0 22-JUN-22",
      vocabulary_concept_id = 44819096L
    )
  )

  drugStrength <- dplyr::bind_rows(
    data.frame(
      drug_concept_id = 10L,
      ingredient_concept_id = 10L,
      amount_value = NA_real_,
      amount_unit_concept_id = 8576L,
      numerator_value = 0.010,
      numerator_unit_concept_id = 8576L,
      denominator_value = 0.5,
      denominator_unit_concept_id = 8587L,
      box_size = NA_integer_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA)
    ),
    data.frame(
      drug_concept_id = 20L,
      ingredient_concept_id = NA_integer_,
      amount_value = NA_real_,
      amount_unit_concept_id = 22L,
      numerator_value = NA_real_,
      numerator_unit_concept_id = NA_integer_,
      denominator_value = 0.5,
      denominator_unit_concept_id = NA_integer_,
      box_size = NA_integer_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA)
    ),
    data.frame(
      drug_concept_id = 21L,
      ingredient_concept_id = NA_integer_,
      amount_value = NA_real_,
      amount_unit_concept_id = 23L,
      numerator_value = NA_real_,
      numerator_unit_concept_id = NA_integer_,
      denominator_value = 0.5,
      denominator_unit_concept_id = NA_integer_,
      box_size = NA_integer_,
      valid_start_date = as.Date(NA),
      valid_end_date = as.Date(NA)
    )
  )

  cdmSource <- dplyr::as_tibble(
    data.frame(
      cdm_source_name  = "mock",
      cdm_source_abbreviation = NA_character_,
      cdm_holder = NA_character_,
      source_description = NA_character_,
      source_documentation_reference = NA_character_,
      cdm_etl_reference = NA_character_,
      source_release_date = as.Date(NA),
      cdm_release_date = as.Date(NA),
      cdm_version = "5.3",
      vocabulary_version  = NA_character_
    )
  )

  cdm_df <- omopgenerics::cdmFromTables(tables = list(person = person,
                                                      concept = concept,
                                                      condition_occurrence = conditionOccurrence,
                                                      concept_ancestor = conceptAncestor,
                                                      concept_synonym = conceptSynonym,
                                                      concept_relationship = conceptRelationship,
                                                      vocabulary = vocabulary,
                                                      drug_strength = drugStrength,
                                                      observation_period = observationPeriod,
                                                      cdm_source = cdmSource),
                                        cdmName = "mock")

  cdm_df <- buildAchillesTables(cdm_df) |>
    suppressMessages()

  if (backend == "data_frame") {
    return(cdm_df)
  }

  # into db
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  cdm <- CDMConnector::copyCdmTo(con = db,
                                 cdm = cdm_df,
                                 schema = "main",
                                 overwrite = TRUE)
  attr(cdm, "write_schema") <- "main"
  cdm
}
