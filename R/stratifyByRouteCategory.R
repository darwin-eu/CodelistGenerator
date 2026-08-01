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


#' Stratify a codelist by route category.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{route_category}` to include the route
#' category name.
#' @inheritParams keepOriginalDoc
#'
#' @return The codelist with the required stratifications, as different elements
#' of the list.
#' @export
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#' codes <- newCodelist(list("concepts" = c(20,21,22)))
#' new_codes <- stratifyByRouteCategory(x = codes,
#'                                      cdm = cdm,
#'                                      keepOriginal = TRUE)
#' new_codes
#'}
stratifyByRouteCategory <- function(x,
                                    cdm,
                                    nameStyle = "{codelist_name}_{route_category}",
                                    keepOriginal = FALSE) {
  st <- searchStrategyAttr(
    function_name = "stratifyByRouteCategory",
    cdm = "cdm"
  )
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "route_category",
    nameStyle = nameStyle,
    st = st,
    keepOriginal = keepOriginal
  )
}

addRouteCategory <- function(x) {

  x_route <- x |>
    addDoseForm(keepDiscordant = TRUE) |>
    dplyr::left_join(
      CodelistGenerator::doseFormToRoute |>
        dplyr::select("dose_form_concept_name", "route_category"),
      by = c("dose_form" = "dose_form_concept_name")
    ) |>
    dplyr::select(!"dose_form") |>
    dplyr::distinct()

  # if multiple values and some are missing, keep only non-missing
  # ie so do not get considered to have unclassified route
  x_route <- x_route |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::filter(!is.na(.data$route_category) | all(is.na(.data$route_category))) |>
    dplyr::ungroup()


  # if multiple discordant route, set to unknown
  x_route <- x_route |>
    dplyr::group_by(.data$codelist_name, .data$concept_id) |>
    dplyr::mutate(discordant = dplyr::if_else(dplyr::n() == 1, 1L, 0L)) |>
    dplyr::mutate(route_category = dplyr::if_else(.data$discordant == 1,
                                                  .data$route_category,
                                                  NA)) |>
    dplyr::ungroup() |>
    dplyr::select(!"discordant") |>
    dplyr::distinct()

  x_route

}
