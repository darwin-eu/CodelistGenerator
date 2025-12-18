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

#' Add concepts to a codelist
#'
#' @inheritParams cdmDoc
#' @inheritParams xDoc
#' @param concepts Concepts_ID to add
#' @inheritParams codelistNameDoc
#'
#' @return A codelist
#' @export
#'
#' @examples
#' \donttest{
#' library(omock)
#' library(CDMConnector)
#'
#' # Creating CDM object
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Creating codelist
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    nameStyle = "{concept_name}")
#'
#' # Add a concept to all the codelists:
#' codelist$acetaminophen
#' codelist <- codelist |>
#'  addConcepts(cdm, concepts = c(1L))
#' codelist$acetaminophen
#'
#' # Add a concept to a specific codelist
#' codelist$amiodarone
#' codelist <- codelist |>
#'   addConcepts(cdm, concepts = c(2L), codelistName = "amiodarone")
#' codelist$amiodarone
#'
#' # See function: `excludeConcepts()` for details on how to remove specific concepts
#' # from a codelist
#' }
addConcepts <- function(x,
                        cdm,
                        concepts,
                        codelistName = NULL){

  # initial checks
  checkCodelist(x)
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(concepts, integerish = TRUE, min = 0)
  omopgenerics::assertCharacter(codelistName, null = TRUE)

  no_present <- setdiff(codelistName, names(x))
  if (length(no_present) > 0) {
    cli::cli_abort("{.val {no_present}} codelist{?s} {?does/do} not exist in {.obj x}.")
  }

  if(is.null(codelistName)){
    codelistName <- names(x)
  }

  # Unique concepts
  concepts <- unique(concepts)

  concepts <- list("new_codelist" = concepts) |>
    omopgenerics::newCodelist() |>
    suppressWarnings()

  if(inherits(x, "codelist_with_details")){
    concepts <- concepts |>
      CodelistGenerator::asCodelistWithDetails(cdm)
    x[codelistName] <- purrr::map(x[codelistName], ~ dplyr::bind_rows(.x, concepts$new_codelist))
    x[codelistName] <- purrr::map(x[codelistName], ~ dplyr::distinct(.x) %>% dplyr::arrange(concept_id))
  }

  if(inherits(x, "codelist")){
    x[codelistName] <- purrr::map(x[codelistName], ~ append(.x, concepts$new_codelist))
    x[codelistName] <- purrr::map(x[codelistName], ~ sort(unique(.x)))
  }

  return(x)
}
