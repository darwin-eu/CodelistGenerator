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

#' Get descendant codes for a given concept
#'
#' @inheritParams cdmDoc
#' @param conceptId concept_id to search
#' @param withAncestor If TRUE, return column with ancestor. In case of multiple
#' ancestors, concepts will be separated by ";".
#'
#' @return The descendants of a given concept id.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getDescendants(cdm = cdm, conceptId = 1)
#' }

getDescendants <- function(cdm,
                           conceptId,
                           withAncestor = FALSE) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(conceptId, integerish = TRUE)
  omopgenerics::assertLogical(withAncestor)

  if(isFALSE(withAncestor)){
    descendants <- getDescendantsOnly(cdm, conceptId)}

  if(isTRUE(withAncestor)){
    descendants <- getDescendantsAndAncestor(cdm, conceptId)}

  return(descendants)
}

getDescendantsOnly <- function(cdm, conceptId) {
  descendants <- cdm$concept_ancestor |>
    dplyr::filter(.data$ancestor_concept_id %in% .env$conceptId) |>
    dplyr::select("descendant_concept_id") |>
    dplyr::distinct() |>
    dplyr::rename("concept_id" = "descendant_concept_id") |>
    dplyr::left_join(cdm$concept,
                     by = "concept_id")

  descendants <- descendants |>
    dplyr::collect()

  return(descendants)
}

getDescendantsAndAncestor <- function(cdm, conceptId) {
  conceptIdDbTable <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = conceptIdDbTable,
                                   table = dplyr::tibble(ancestor_concept_id = as.integer(conceptId)),
                                   overwrite = TRUE)

  descendants <- cdm$concept_ancestor |>
    dplyr::inner_join(cdm[[conceptIdDbTable]],
                      by = "ancestor_concept_id") |>
    dplyr::rename("concept_id" = "descendant_concept_id") |>
    dplyr::left_join(cdm$concept,
                     by = "concept_id") |>
    dplyr::compute()

  descendants <- descendants |>
    dplyr::collect() |>
    dplyr::mutate(name = paste0("concept_", .data$ancestor_concept_id))

  if(nrow(descendants)>0){
    descendants <- descendants |>
      tidyr::pivot_wider(names_from = "name",
                         values_from = "ancestor_concept_id")

    # one row per concept, with ancestor (of which there may be multiple)
    working_cols <- stringr::str_subset(string = colnames(descendants),
                                        pattern = paste(c(colnames(cdm$concept),
                                                          colnames(cdm$concept_ancestor)),
                                                        collapse = "|"),
                                        negate = TRUE)

    descendants <- descendants |>
      tidyr::unite(col="ancestor_concept_id",
                   dplyr::all_of(working_cols), sep=";")
    # quicker to replace NAs afterwards rather than inside unite
    # (especially when there are many columns)
    descendants$ancestor_concept_id <- stringr::str_replace_all(
      string = descendants$ancestor_concept_id,
      pattern = ";NA|NA;",
      replacement = ""
    )
  }

  omopgenerics::dropSourceTable(cdm, conceptIdDbTable)

  # nb conceptId will also be a descendant of itself
  return(descendants)

}

