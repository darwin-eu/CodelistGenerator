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

#' Generate a codelist from the union of different codelists. The generated codelist
#' will come out in alphabetical order.
#'
#' @inheritParams xDoc
#' @inheritParams keepOriginalDoc
#'
#' @return A codelist
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getDrugIngredientCodes(cdm,
#'                         nameStyle = "{concept_name}") |>
#' unionCodelists()
#' }
unionCodelists <- function(x,
                           keepOriginal = FALSE) {

  checkCodelist(x)
  omopgenerics::assertLogical(keepOriginal, length = 1)

  xNames <- names(x)

  allCodes <- purrr::list_c(x) |>
    unique()
  allCodesName <- paste0(xNames, collapse = "_")

  newX <- list()
  newX[[allCodesName]] <- allCodes
  if(isTRUE(keepOriginal)){
    newX <- purrr::list_flatten(list(x, newX))
  }

  if(inherits(x, "codelist")){
    newX <- newX |> omopgenerics::newCodelist()
  }
  if(inherits(x, "codelist_with_details")){
    newX <- newX |> omopgenerics::newCodelistWithDetails()
  }
  if(inherits(x, "concept_set_expression")){
    newX <- newX |> omopgenerics::newConceptSetExpression()
  }

  return(newX)
}
