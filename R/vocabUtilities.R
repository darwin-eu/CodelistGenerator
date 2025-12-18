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

#' Get the available version of the vocabulary used in the cdm
#'
#' @inheritParams cdmDoc
#'
#' @return The vocabulary version being used in the cdm.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' vocabularyVersion(cdm = cdm)
#' }
vocabularyVersion <- function(cdm) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  version <- as.character(cdm$vocabulary |>
                            dplyr::rename_with(tolower) |>
                            dplyr::filter(.data$vocabulary_id == "None") |>
                            dplyr::select("vocabulary_version") |>
                            dplyr::collect())
  return(version)
}

addIngredientCount <- function(cdm, concepts) {
  ingredient_ancestor <- cdm$concept_ancestor |>
    dplyr::inner_join(cdm$concept |>
                        dplyr::filter(.data$concept_class_id == "Ingredient",
                                      .data$standard_concept == "S") |>
                        dplyr::select("concept_id"),
                      by = c("ancestor_concept_id" = "concept_id"))

  ingredient_count <- concepts |>
    dplyr::distinct(.data$concept_id) |>
    dplyr::left_join(ingredient_ancestor,
                     by = c("concept_id" = "descendant_concept_id")) |>
    dplyr::count(
      .data$concept_id,
      wt = dplyr::if_else(is.na(.data$ancestor_concept_id), 0L, 1L),
      name = "ingredient_count"
    )

  concepts <- concepts |>
    dplyr::left_join(ingredient_count,
                     by = "concept_id")

  if(!is.null(attr(cdm, "dbcon"))){
    concepts <- concepts |>
      dplyr::compute()}

  return(concepts)
}

