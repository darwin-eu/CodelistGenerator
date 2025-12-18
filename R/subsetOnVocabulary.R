# Copyright 2024 DARWIN EU (C)
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

#' Subset a codelist to only those codes from a particular vocabulary.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param vocabulary Vocabulary to subset with (i.e., SNOMED)
#' @param negate If FALSE, only concepts with the vocabulary specified will be
#' returned. If TRUE, concepts with the vocabulary specified will be excluded.
#'
#' @return The codelist with only those concepts associated with the vocabulary
#' (if negate = FALSE) or the codelist without those concepts associated with
#' the vocabulary (if negate = TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#' codes <- subsetOnVocabulary(
#'               x = newCodelist(list("codes" = c(1L,13L,15L))),
#'               cdm = cdm,
#'               vocabulary = "SNOMED")
#' codes
#' }
subsetOnVocabulary <- function(x,
                               cdm,
                               vocabulary,
                               negate = FALSE){

  # Initial checks
  checkCodelist(x, allowConceptSetExpression = FALSE)
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertCharacter(vocabulary)
  omopgenerics::assertLogical(negate, length = 1)
  x_original <- x

  if(length(x) == 0){
    cli::cli_warn("{.val x} is empty. Returning an empty codelist.")
    return(x)
  }

  # Add vocabulary_id table if it's not already there
  if(inherits(x_original, "codelist")){
    x <- x |> addDetails(cdm, cols = "vocabulary_id")
  }

  if(inherits(x_original, "codelist_with_details")){
    if(!c("vocabulary_id" %in% colnames(x[[1]]))){
      x <- x |> addDetails(cdm, cols = "vocabulary_id")
      noVocab <- TRUE
    }else{
      noVocab <- FALSE
    }
  }

  # Filter vocabulary
  if(isTRUE(negate)){
    x <- purrr::map(x, ~ dplyr::filter(.x, !tolower(.data$vocabulary_id) %in% tolower(.env$vocabulary)))
  }else{
    x <- purrr::map(x, ~ dplyr::filter(.x, tolower(.data$vocabulary_id) %in% tolower(.env$vocabulary)))
  }

  if(inherits(x_original, "codelist")){
    x <- purrr::map(x, ~dplyr::pull(.x, "concept_id")) |>
      omopgenerics::newCodelist()
  }

  if(inherits(x_original, "codelist_with_details")){
    if(isTRUE(noVocab)){
      x <- purrr::map(x, ~dplyr::select(.x, -"vocabulary_id"))
    }
    x <- x |>
      omopgenerics::newCodelistWithDetails()
  }

  x <- dropEmptyCodelist(x_original, x)

  return(x)
}
