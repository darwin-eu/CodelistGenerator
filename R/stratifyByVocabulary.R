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

#' Subset a codelist to only those codes from a particular domain.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{vocabulary}` to include the vocabulary name.
#' @inheritParams keepOriginalDoc
#'
#' @return The codelist with the required stratifications, as different elements
#' of the list.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#'
#' cdm <- mockVocabRef()
#'
#' codes <- stratifyByVocabulary(
#'   x = newCodelist(list("codes" = c(10L, 13L, 15L))),
#'   cdm = cdm,
#'   keepOriginal = TRUE
#' )
#'
#' codes
#' }
#'
stratifyByVocabulary <- function(x,
                                 cdm,
                                 nameStyle = "{codelist_name}_{vocabulary}",
                                 keepOriginal = FALSE) {
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "vocabulary",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addVocabulary <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add vocabulary
    dplyr::left_join(
      cdm$concept |>
        dplyr::select("concept_id", "vocabulary" = "vocabulary_id"),
      by = "concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::distinct()
}
