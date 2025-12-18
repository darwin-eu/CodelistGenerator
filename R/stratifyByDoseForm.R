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
#
#' Stratify a codelist by dose form.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{dose_form}` to include the dose form name.
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
#' codes <- newCodelist(list("codes" = c(10L, 20L, 21L)))
#' new_codes <- stratifyByDoseForm(x = codes,
#'                                 cdm = cdm,
#'                                 keepOriginal = TRUE)
#' new_codes
#' }
stratifyByDoseForm <- function(x,
                               cdm,
                               nameStyle = "{codelist_name}_{dose_form}",
                               keepOriginal = FALSE) {
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "dose_form",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addDoseFormId <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add dose_form
    dplyr::left_join(
      cdm$concept_relationship |>
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
        dplyr::select(
          "concept_id" = "concept_id_1",
          "dose_form_concept_id" = "concept_id_2"
        ),
      by = "concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::distinct()
}
addDoseForm <- function(x) {
  x |>
    # add dose_form_concept_id
    addDoseFormId() |>
    # add dose_form
    dplyr::left_join(
      CodelistGenerator::doseFormToRoute |>
        dplyr::select(
          "dose_form_concept_id",
          "dose_form" = "dose_form_concept_name"
        ),
      by = "dose_form_concept_id"
    ) |>
    dplyr::select(!"dose_form_concept_id") |>
    dplyr::distinct()
}
