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
  st <- searchStrategyAttr(
    function_name = "stratifyByDoseForm",
    cdm = "cdm"
  )
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "dose_form",
    nameStyle = nameStyle,
    st = st,
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
    dplyr::mutate(dose_form_concept_id = as.integer(.data$dose_form_concept_id)) |>
    dplyr::distinct()
}

addDoseForm <- function(x, keepDiscordant = FALSE) {
  cdm <- omopgenerics::cdmReference(table = x)
  x_dose_forms <- x |>
    addDoseFormId() |>
    dplyr::collect() |>
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

  # can also try get dose forms by going via constitutes relationship
  # only keep if they have 1 associateddose form
  x_dose_forms_via_constitutes <-  x |>
    addDoseFormId() |>
    dplyr::select("codelist_name",
                  "concept_id_1" = "concept_id") |>
    dplyr::distinct() |>
    dplyr::inner_join(cdm$concept_relationship |>
                        dplyr::filter(.data$relationship_id == "Constitutes"),
                      by = "concept_id_1") |>
    dplyr::select("codelist_name",
                  "x_concept_id" = "concept_id_1",
                  "concept_id" = "concept_id_2") |>
    addDoseFormId() |>
    dplyr::collect()  |>
    dplyr::left_join(
      CodelistGenerator::doseFormToRoute |>
        dplyr::select(
          "dose_form_concept_id",
          "dose_form" = "dose_form_concept_name"
        ),
      by = "dose_form_concept_id"
    ) |>
    dplyr::select("codelist_name",
                  "concept_id" = "x_concept_id",
                  "dose_form") |>
    dplyr::distinct()


  # if multiple values and some are missing, keep only non-missing
  # ie so do not get considered to have unclassified dose form
  dose_forms <- x_dose_forms |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::filter(!is.na(.data$dose_form) | all(is.na(.data$dose_form))) |>
    dplyr::ungroup()

  # for those without dose form, try and fill in from the constitutes relationship
  dose_forms <- dplyr::bind_rows(
    dose_forms |>
      dplyr::filter(!is.na(.data$dose_form)),
    dose_forms |>
      dplyr::filter(is.na(.data$dose_form)) |>
      dplyr::select("codelist_name",
                    "concept_id") |>
      dplyr::left_join(x_dose_forms_via_constitutes,
                       by = c("codelist_name",
                              "concept_id")))

  # if multiple discordant route, set to unknown
  if(isFALSE(keepDiscordant)){
    dose_forms <- dose_forms |>
      dplyr::group_by(.data$codelist_name, .data$concept_id) |>
      dplyr::mutate(discordant = dplyr::if_else(dplyr::n() == 1, 1L, 0L)) |>
      dplyr::mutate(dose_form = dplyr::if_else(.data$discordant == 1,
                                               .data$dose_form,
                                               NA)) |>
      dplyr::ungroup() |>
      dplyr::select(!"discordant") |>
      dplyr::distinct()
  }

  dose_forms

}
