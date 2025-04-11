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

#' Get the names of all available drug ingredients
#'
#' @inheritParams cdmDoc
#'
#' @return A vector containing the concept names for all ingredient level codes
#' found in the concept table of cdm.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' availableIngredients(cdm)
#'}
availableIngredients <- function(cdm) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  ingredientConcepts <- cdm$concept |>
    dplyr::filter(.data$standard_concept == "S") |>
    dplyr::filter(.data$concept_class_id == "Ingredient") |>
    dplyr::pull("concept_name")

    return(ingredientConcepts)
}

#' Get the names of all available Anatomical Therapeutic Chemical (ATC) classification codes
#'
#' @inheritParams cdmDoc
#' @inheritParams levelATCDoc
#'
#' @return A vector containing the names of ATC codes for the chosen level(s)
#' found in the concept table of cdm.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' availableATC(cdm)
#'}
#'
availableATC <- function(cdm,
                         level = c("ATC 1st")) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, choices = c(
    "ATC 1st",
    "ATC 2nd",
    "ATC 3rd",
    "ATC 4th",
    "ATC 5th"
  ))

    atc_names <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ATC") |>
    dplyr::filter(.data$concept_class_id %in% .env$level) |>
    dplyr::pull("concept_name")

  return(atc_names)
}

#' Get the names of all International Classification of Diseases (ICD) 10 codes
#'
#' @inheritParams cdmDoc
#' @inheritParams levelICD10Doc
#'
#' @return A vector containing the names of all ICD-10 codes for the chosen
#' level(s) found in the concept table of cdm.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' availableICD10(cdm)
#'}
availableICD10 <- function(cdm,
                           level = c(
                             "ICD10 Chapter",
                             "ICD10 SubChapter"
                           )){

  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, choices = c(
    "ICD10 Chapter",
    "ICD10 SubChapter",
    "ICD10 Hierarchy",
    "ICD10 Code"
  ))

  if("ICD10 Code" %in% level){
    level <- c(level, "ICD10 code") # for compatability with older vocab versions
  }

  ICD10Concepts <- list()
  for(i in seq_along(level)){
  working_level <- level[i]
  ICD10Concepts[[i]] <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ICD10",
                   .data$concept_class_id %in%
                    .env$working_level) |>
    dplyr::pull("concept_name")
  }
  ICD10Concepts <- purrr::flatten_chr(ICD10Concepts)

  return(ICD10Concepts)
}
