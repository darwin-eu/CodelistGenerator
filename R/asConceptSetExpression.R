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

#' Coerce to a concept set expression
#'
#' @param x Codelist or codelist with details
#' @param ... For extensibility
#'
#' @returns codelist
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
#' # Create concept_set_expression from a codelist
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    name = "acetaminophen",
#'                                    nameStyle = "{concept_name}",
#'                                    type = "codelist")
#'
#' asConceptSetExpression(codelist)
#'
#' # Create concept_set_expression from a codelist_with_details
#' codelist <- getDrugIngredientCodes(cdm,
#'                                    name = "acetaminophen",
#'                                   nameStyle = "{concept_name}",
#'                                    type = "codelist_with_details")
#'
#' asConceptSetExpression(codelist)
#' }
asConceptSetExpression <- function(x, ...){
  UseMethod("asConceptSetExpression")
}

#' @export
#' @rdname asConceptSetExpression
asConceptSetExpression.codelist <- function(x, ...){
  x |>
    purrr::map(\(x) {
      dplyr::tibble(
        concept_id = as.integer(x),
        excluded = FALSE,
        descendants = FALSE,
        mapped = FALSE
      )
    }) |>
    omopgenerics::newConceptSetExpression()
}

#' @export
#' @rdname asConceptSetExpression
asConceptSetExpression.codelist_with_details  <- function(x, ...){
  asCodelist(x) |>
    purrr::map(\(x) {
      dplyr::tibble(
        concept_id = as.integer(x),
        excluded = FALSE,
        descendants = FALSE,
        mapped = FALSE
      )
    }) |>
    omopgenerics::newConceptSetExpression()
}
