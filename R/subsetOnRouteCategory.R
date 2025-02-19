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

#' Subset a codelist to only those with a particular route category
#'
#' @param x Codelist
#' @param cdm A cdm reference
#' @param routeCategory Route category. Use getRoutes() to find the available
#' route categories for a cdm
#'
#' @return The codelist with only those concepts associated with the
#' specified route categories
#' @export
#'
subsetOnRouteCategory <- function(x, cdm, routeCategory){

  if(inherits(x, "codelist_with_details")){
    x_original <- x
    withDetails <- TRUE
    x <- codelistFromCodelistWithDetails(x)
  } else {
    withDetails <- FALSE
  }

  x <- omopgenerics::newCodelist(x)

  if(isFALSE(inherits(cdm, "cdm_reference"))){
    cli::cli_abort("cdm must be a cdm reference")
  }
  if(isFALSE(is.character(routeCategory))){
    cli::cli_abort("routeCategory must be a character vector")
  }

  doseRouteData <- get("doseFormToRoute")

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    x[[i]] <- cdm[[tableCodelist]] |>
      dplyr::inner_join(cdm$concept_relationship |>
      dplyr::filter(.data$relationship_id == "RxNorm has dose form"),
      by = c("concept_id" = "concept_id_1")
      ) |>
      dplyr::select("concept_id",
                    "concept_id_2") |>
      dplyr::collect() |>
      dplyr::left_join(
        doseRouteData, by = c("concept_id_2" = "dose_form_concept_id")
      ) |>
      dplyr::mutate(route_category = dplyr::if_else(
        is.na(.data$route_category),
        "unclassified_route",
        .data$route_category
      )) |>
      dplyr::filter(.data$route_category %in% .env$routeCategory) |>
      dplyr::select("concept_id") |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    x[[i]] <- sort(x[[i]])

    if(isTRUE(withDetails)){
      x[[i]] <- x_original[[i]] |>
        dplyr::filter(.data$concept_id %in% x[[i]])
    }
  }

   x <- x |>
    vctrs::list_drop_empty()

   CDMConnector::dropTable(cdm = cdm, name = tableCodelist)

   x

}
