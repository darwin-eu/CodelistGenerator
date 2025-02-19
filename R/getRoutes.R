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

#' Get available routes in a cdm reference.
#'
#' @description
#' Get the dose form categories available in the database (see
#' https://doi.org/10.1002/pds.5809) for more details on how routes
#' were classified).
#'
#'
#' @param cdm A cdm reference.
#'
#' @return A character vector with available routes
#' @export
#'
getRouteCategories <- function(cdm) {

    # relate does form concept id with the classification established by
    # doseFormToRoute
    doseRouteData <- get("doseFormToRoute")

    routeCategory <- cdm$concept_relationship |>
      # get dose form available in the cdm
      dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
      dplyr::select("concept_id_2") |>
      dplyr::rename("concept_id" = "concept_id_2") |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::left_join(
        doseRouteData, by = c("concept_id" = "dose_form_concept_id")
      ) |>
      dplyr::mutate(route_category = dplyr::if_else(
        is.na(.data$route_category),
        "unclassified_route",
        .data$route_category
      )) |>
      dplyr::select("route_category") |>
      dplyr::distinct() |>
      dplyr::pull()

  # sort alphabetically the result
  routeCategory <- sort(routeCategory)

  return(routeCategory)
}

