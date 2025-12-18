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


#' Stratify a codelist by domain category.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{domain}` to include the domain name.
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
#' codes <- newCodelist(list("concepts_1" = c(20L,21L,22L),
#'                          "concepts_2" = c(10L,13L,21L)))
#' new_codes <- stratifyByDomain(x = codes,
#'                              cdm = cdm,
#'                              keepOriginal = TRUE)
#' new_codes
#'}
#'
stratifyByDomain <- function(x,
                             cdm,
                             nameStyle = "{codelist_name}_{domain}",
                             keepOriginal = FALSE) {
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "domain",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addDomain <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add domain
    dplyr::left_join(
      cdm$concept |>
        dplyr::select("concept_id", "domain" = "domain_id"),
      by = "concept_id"
    ) |>
    dplyr::select("codelist_name", "concept_id", "domain") |>
    dplyr::collect() |>
    dplyr::distinct()
}
