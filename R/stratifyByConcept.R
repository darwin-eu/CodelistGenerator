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


#' Stratify a codelist by the concepts included within it.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{concept}` to include the concept name.
#' @inheritParams keepOriginalDoc
#'
#' @return The codelist or a codelist with details with the required
#' stratifications, as different elements of the list.
#' @export
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#'
#' cdm <- mockVocabRef()
#'
#' codes <- newCodelist(list("concepts" = c(20L, 21L)))
#'
#' new_codes <- stratifyByConcept(x = codes,
#'                                cdm = cdm,
#'                                keepOriginal = TRUE)
#'
#' new_codes
#' }
stratifyByConcept <- function(x,
                              cdm,
                              nameStyle = "{codelist_name}_{concept}",
                              keepOriginal = FALSE) {
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "concept",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addConcept <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add concept
    dplyr::left_join(
      cdm$concept |>
        dplyr::select("concept_id", "concept" = "concept_name"),
      by = "concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::distinct()
}
