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
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "route_category",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addRouteCategory <- function(x) {
  x |>
    # add dose_form_concept_id
    addDoseFormId() |>
    # add route_category
    dplyr::left_join(
      CodelistGenerator::doseFormToRoute |>
        dplyr::select("dose_form_concept_id", "route_category"),
      by = "dose_form_concept_id"
    ) |>
    dplyr::select(!"dose_form_concept_id") |>
    dplyr::distinct()
}
