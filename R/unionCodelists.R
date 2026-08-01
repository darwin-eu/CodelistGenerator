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
#' @param newCodelistName Character vector with the name of the new codelist. If
#' NULL all codelists names will be combined.
#' @inheritParams keepOriginalDoc
#' @param codelistsToJoin Character vector with the names of the codelists to be
#' unioned.
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
                           newCodelistName = NULL,
                           keepOriginal = FALSE,
                           codelistsToJoin = names(x)) {

  checkCodelist(x)
  omopgenerics::assertLogical(keepOriginal, length = 1)
  omopgenerics::assertCharacter(newCodelistName, length = 1, null = TRUE)
  omopgenerics::assertChoice(codelistsToJoin, names(x), unique = TRUE)

  xNames <- codelistsToJoin

  allCodes <- purrr::list_c(x[codelistsToJoin]) |>
    unique()
  if (is.null(newCodelistName)) {
    newCodelistName <- paste0(xNames, collapse = "_")
  }

  newX <- list()
  newX[[newCodelistName]] <- allCodes
  if (isTRUE(keepOriginal)) {
    newX <- purrr::list_flatten(list(x, newX))
  }

  if (inherits(x, "codelist")) {
    newX <- newX |> omopgenerics::newCodelist()
  }
  if (inherits(x, "codelist_with_details")) {
    newX <- newX |> omopgenerics::newCodelistWithDetails()
  }
  if (inherits(x, "concept_set_expression")) {
    newX <- newX |> omopgenerics::newConceptSetExpression()
  }

  return(newX)
}
