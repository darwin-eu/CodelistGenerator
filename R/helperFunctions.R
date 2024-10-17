# Copyright 2024 DARWIN EU®
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

#' Get all ingredients codes from the cdm
#'
#' @param cdm cdm_reference via CDMConnector
#'
#' @return A vector list of all ingredient level codes found in the concept
#' table of cdm.
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' availableIngredients(cdm)
#'}
availableIngredients <- function(cdm) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  ingredientConcepts <- cdm$concept |>
    dplyr::filter(.data$standard_concept == "S") |>
    dplyr::filter(.data$concept_class_id == "Ingredient") |>
    dplyr::pull("concept_name")

    return(ingredientConcepts)
}

#' Get all ATC codes from the cdm
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level ATC level. Can be one or more of "ATC 1st", "ATC 2nd",
#' "ATC 3rd", "ATC 4th", and "ATC 5th"
#'
#' @return A vector list of all ATC codes for the chosen level(s) found in the
#' concept table of cdm.
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
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  levelCheck <- all(level %in%
                      c(
                        "ATC 1st",
                        "ATC 2nd",
                        "ATC 3rd",
                        "ATC 4th",
                        "ATC 5th"
                      ))
  if (!isTRUE(levelCheck)) {
    errorMessage$push(
      "- level can only be from: ATC 1st, ATC 2nd, ATC 3rd, ATC 4th, ATC 5th"
    )
  }
  checkmate::assertTRUE(levelCheck, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  atc_names <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ATC") |>
    dplyr::filter(.data$concept_class_id %in% .env$level) |>
    dplyr::pull("concept_name")

  return(atc_names)
}

#' Get all ICD codes from the cdm
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level Can be either "ICD10 Chapter" or "ICD10 SubChapter"
#'
#' @return A vector list of all ICD10 codes for the chosen level(s) found in the
#' concept table of cdm.
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

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  levelCheck <- all(level %in%
                      c(
                        "ICD10 Chapter",
                        "ICD10 SubChapter"
                      ))
  if (!isTRUE(levelCheck)) {
    errorMessage$push(
      "- level can only be from: ICD10 Chapter, ICD10 SubChapter "
    )
  }
  checkmate::assertTRUE(levelCheck, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  ICD10Concepts <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ICD10") |>
    dplyr::filter(.data$concept_class_id %in% .env$level) |>
    dplyr::pull("concept_name")

  return(ICD10Concepts)
}
