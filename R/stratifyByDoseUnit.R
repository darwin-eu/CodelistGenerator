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


#' Stratify a codelist by dose unit.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{dose_unit}` to include the dose unit name.
#' @inheritParams keepOriginalDoc
#'
#' @return The codelist with the required stratifications, as different elements
#' of the list.
#' @export
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#'
#' cdm <- mockVocabRef()
#'
#' codes <- newCodelist(list("concepts" = c(20L, 21L)))
#' new_codes <- stratifyByDoseUnit(x = codes,
#'                                 cdm = cdm,
#'                                 keepOriginal = TRUE)
#' new_codes
#' }
stratifyByDoseUnit <- function(x,
                               cdm,
                               nameStyle = "{codelist_name}_{dose_unit}",
                               keepOriginal = FALSE){
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "dose_unit",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addDoseUnit <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add amount unit and numerator unit
    dplyr::left_join(
      cdm$drug_strength |>
        dplyr::left_join(
          cdm$concept |>
            dplyr::select(
              "amount_unit_concept_id" = "concept_id",
              "amount_unit" = "concept_name"
            ),
          by = "amount_unit_concept_id"
        ) |>
        dplyr::left_join(
          cdm$concept |>
            dplyr::select(
              "numerator_unit_concept_id" = "concept_id",
              "numerator_unit" = "concept_name"
            ),
          by = "numerator_unit_concept_id"
        ) |>
        dplyr::select("concept_id" = "drug_concept_id", "amount_unit", "numerator_unit"),
      by = "concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(dose_unit = dplyr::coalesce(.data$amount_unit, .data$numerator_unit)) |>
    dplyr::select(!c("amount_unit", "numerator_unit")) |>
    dplyr::distinct()
}
