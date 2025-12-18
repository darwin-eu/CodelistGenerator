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


#' Stratify a codelist by brand category.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param nameStyle Naming of the new codelists, use `{codelist_name}` to
#' include the codelist name and `{brand}` to include the brand name.
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
#' codes <- newCodelist(list(
#'   concepts_1 = c(20L, 21L, 22L),
#'   concepts_2 = c(10L, 13L, 21L)
#' ))
#'
#' new_codes <- stratifyByBrand(x = codes,
#'                              cdm = cdm,
#'                              keepOriginal = TRUE)
#' new_codes
#'}
stratifyByBrand <- function(x,
                            cdm,
                            nameStyle = "{codelist_name}_{brand}",
                            keepOriginal = FALSE) {
  stratifyCodelistBy(
    x = x,
    cdm = cdm,
    by = "brand",
    nameStyle = nameStyle,
    keepOriginal = keepOriginal
  )
}

addBrand <- function(x) {
  cdm <- omopgenerics::cdmReference(table = x)
  x |>
    # add domain and concept_class_id
    dplyr::left_join(
      cdm$concept |>
        dplyr::select("concept_id", "domain_id", "concept_class_id"),
      by = "concept_id"
    ) |>
    # remove missing domains
    dplyr::filter(!is.na(.data$domain_id)) |>
    # add brand concept id
    dplyr::left_join(
      cdm[["concept_relationship"]] |>
        dplyr::filter(.data$relationship_id == "Has brand name") |>
        dplyr::rename(
          concept_id = "concept_id_1",
          brand_concept_id = "concept_id_2"
        ),
      by = "concept_id"
    ) |>
    # add brand name
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          brand_concept_id = "concept_id",
          brand = "concept_name"
        ),
      by = "brand_concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::mutate(brand = dplyr::if_else(
      .data$concept_class_id == "Ingredient",
      NA_character_,
      .data$brand
    )) |>
    dplyr::select("codelist_name", "concept_id", "brand") |>
    dplyr::distinct()
}
