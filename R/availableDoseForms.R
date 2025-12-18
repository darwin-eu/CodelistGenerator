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

#' Get the dose forms for drug concepts
#'
#' @inheritParams cdmDoc
#'
#' @return The dose forms available for drug concepts.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get all domains available in the CDM
#' availableDoseForms(cdm = cdm)
#' }
availableDoseForms <- function(cdm) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  # all dose forms in the cdm
    doseForms <- cdm[["concept_relationship"]] |>
      dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
      dplyr::select("dose_form_concept_id" = "concept_id_2") |>
      dplyr::distinct() |>
      dplyr::collect()

    doseForms <- doseForms |>
      dplyr::left_join(
        CodelistGenerator::doseFormToRoute,
        by = "dose_form_concept_id"
      ) |>
      dplyr::pull("dose_form_concept_name") |>
      unique() |>
      sort()

    return(doseForms)

}

#' Get the dose forms associated with drug concepts in a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#'
#' @return The dose forms available for drug concepts.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get all dose forms available in a codelist
#' codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
#'                              "codes2" = c(44022939L)))
#' associatedDoseForms(x = codelist, cdm = cdm)
#' }
associatedDoseForms <- function(x,
                                cdm) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  checkCodelist(x, allowConceptSetExpression = FALSE)

  original <- x

  # codelist table
  nm <- omopgenerics::uniqueTableName()
  x <- dplyr::as_tibble(x) |>
    dplyr::rename(codelist_name = dplyr::any_of("codelist_with_details_name")) |>
    dplyr::select("codelist_name", "concept_id")

  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = nm,
                                   table = x)
  on.exit(omopgenerics::dropSourceTable(cdm = cdm, name = nm))

  x <- addDoseForm(x = cdm[[nm]])

  # correct missing
  x <- correctMissingValue(x, "dose_form")

  # stratify codelist
  x <- stratifyCodelist(
    x = x,
    by = c("codelist_name"),
    nameStyle = "{codelist_name}"
  )

  x <- purrr::map(x,
                  \(x) dplyr::pull(x, "dose_form") |> unique() |> sort())

  return(x)
}

