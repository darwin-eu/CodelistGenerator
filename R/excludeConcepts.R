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

#' Exclude concepts from a codelist
#'
#' @inheritParams cdmDoc
#' @inheritParams xDoc
#' @param concepts Concepts_id to exclude
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
#' # downloadMockDataset(datasetName = "GiBleed")
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Creating codelist
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    nameStyle = "{concept_name}")
#'
#' # Exclude concept to all the codelists:
#' codelist$acetaminophen
#' codelist <- codelist |>
#'   excludeConcepts(cdm, concepts = c(1125315L))
#' codelist$acetaminophen

#' # Add a concept to a specific codelist
#' codelist$amiodarone
#' codelist <- codelist |>
#'   excludeConcepts(cdm, concepts = c(1310034L), codelistName = "amiodarone")
#' codelist$amiodarone
#'
#' # See function: `addConcepts()` for details on how to add specific concepts
#' # to a codelist
#'}
excludeConcepts <- function(x,
                            cdm,
                            concepts,
                            codelistName = NULL){

  # initial checks
  checkCodelist(x, allowConceptSetExpression = FALSE)
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

  names1 <- names(x)

  if(inherits(x, "codelist_with_details")){
    concepts <- dplyr::tibble("concept_id" = concepts)
    x[codelistName] <- purrr::map(x[codelistName],
                                  ~ .x |>
                                    dplyr::anti_join(concepts, by = "concept_id") |>
                                    dplyr::arrange(concept_id))
    n1 <- unlist(purrr::map(x, ~nrow(.x)))
    x <- x[names(x)[n1 > 0]]
    x <- x |> newCodelistWithDetails()
  }

  if(inherits(x, "codelist")){
    x[codelistName] <- purrr::map(x[codelistName],
                                  ~ sort(setdiff(.x, concepts)))
    x <- x[lengths(x) > 0]
  }

  no_present <- setdiff(names1, names(x))
  if (length(no_present) > 0) {
    cli::cli_warn("{.val {no_present}} codelist{?s} will be removed from the final
                   codelist, as there are no elements left after the exclusion
                   of the concepts specified.")
  }

  return(x)
}
