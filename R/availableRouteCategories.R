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

#' Get available drug routes
#'
#' @description
#' Get the dose form categories available in the database (see
#' https://doi.org/10.1002/pds.5809) for more details on how routes
#' were classified).
#'
#'
#' @inheritParams cdmDoc
#' @inheritParams xDoc
#'
#' @return A character vector with all available routes.
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
#' availableRouteCategories(cdm = cdm)
#' }
availableRouteCategories <- function(cdm) {
  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

    routeCategories <- cdm[["concept_relationship"]] |>
      dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
      dplyr::select("dose_form_concept_id" = "concept_id_2") |>
      dplyr::distinct() |>
      dplyr::collect()

    routeCategories <- routeCategories |>
      dplyr::left_join(
        CodelistGenerator::doseFormToRoute,
        by = "dose_form_concept_id"
      ) |>
      dplyr::pull("route_category") |>
      unique() |>
      sort()

  return(routeCategories)
}

#' Get drug routes associated with a codelist
#'
#' @description
#' Get the dose form categories available in the database (see
#' https://doi.org/10.1002/pds.5809) for more details on how routes
#' were classified).
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#'
#' @return A character vector with all available routes.
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
#' }
associatedRouteCategories <- function(x,
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

  x <- addRouteCategory(x = cdm[[nm]])

  # correct missing
  x <- correctMissingValue(x, "route_category")

  # stratify codelist
  x <- stratifyCodelist(
    x = x,
    by = c("codelist_name"),
    nameStyle = "{codelist_name}"
  )

  routeCategories <- purrr::map(x,
                                \(x) dplyr::pull(x, "route_category") |> unique() |> sort())

  return(routeCategories)
}
