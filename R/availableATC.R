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
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get ATC 1st level classification codes
#' availableATC(cdm, level = "ATC 1st")
#'
#' # Get all ATC classification codes
#' availableATC(cdm, level = c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th"))
#'}
#'
availableATC <- function(cdm, level = c("ATC 1st")){
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, choices = supportedATC())

  atc_names <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ATC",
                  .data$concept_class_id %in% .env$level) |>
    dplyr::pull("concept_name")

  return(atc_names)
}

supportedATC <- function(){
  c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th")
}
