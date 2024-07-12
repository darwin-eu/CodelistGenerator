# Copyright 2024 DARWIN EU®
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


#' Stratify a codelist by route category
#'
#' @param x A codelist
#' @param cdm A cdm reference
#' @param keepOriginal Whether to keep the original codelist and append the
#' stratify (if TRUE) or just return the stratified codelist (if FALSE).
#'
#' @return A codelist
#' @export
#'
stratifyByRouteCategory <- function(x, cdm, keepOriginal = FALSE){

  if(inherits(x, "codelist_with_details")){
    x_original <- x
    withDetails <- TRUE
    x <- codelistFromCodelistWithDetails(x)
  } else {
    omopgenerics::newCodelist(x)
    withDetails <- FALSE
  }

  x <- omopgenerics::newCodelist(x)

  if(isFALSE(inherits(cdm, "cdm_reference"))){
    cli::cli_abort("cdm must be a cdm reference")
  }
  checkmate::assertLogical(keepOriginal, len = 1)

  doseRouteData <- get0("doseFormToRoute", envir = asNamespace("CodelistGenerator"))

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  result <- list()

  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    workingName <- names(x)[i]

    workingCodesWithRoute <- cdm[[tableCodelist]] |>
      dplyr::left_join(cdm$concept |>
                         dplyr::select("concept_id", "domain_id"),
                       by = "concept_id") |>
      dplyr::left_join(cdm$concept_relationship |>
                          dplyr::filter(.data$relationship_id == "RxNorm has dose form"),
                        by = c("concept_id" = "concept_id_1")
      ) |>
      dplyr::select("concept_id",
                    "concept_id_2",
                    "domain_id") |>
      dplyr::collect() |>
      dplyr::left_join(
        doseRouteData, by = c("concept_id_2" = "dose_form_concept_id")
      ) |>
      dplyr::mutate(route_category = dplyr::if_else(
        is.na(.data$route_category) & (tolower(.data$domain_id) == "drug"),
        "unclassified_route",
        .data$route_category
      )) |>
      dplyr::select("concept_id", "route_category") |>
      dplyr::distinct() |>
      dplyr::filter(!is.na(.data$route_category)) |>
      dplyr::collect()

    if(isTRUE(withDetails)){
      workingCodesWithRoute <-  x_original[[i]] |>
          dplyr::inner_join(workingCodesWithRoute,
                            by = "concept_id")
    }

    workingCodesWithRoute <- split(
      workingCodesWithRoute,
      workingCodesWithRoute[, c("route_category")]
    )

    if(length(workingCodesWithRoute) > 0){
    names(workingCodesWithRoute) <- paste0(workingName, "_",
                                           names(workingCodesWithRoute))
    }

    if(isFALSE(withDetails)){
    for(j in seq_along(workingCodesWithRoute)){
      workingCodesWithRoute[[j]] <- sort(workingCodesWithRoute[[j]] |>
                                           dplyr::pull("concept_id"))

    }}

    result[[i]] <- workingCodesWithRoute

  }

  result <- purrr::list_flatten(result) |>
    vctrs::list_drop_empty()

  if(isTRUE(keepOriginal)){
    result <- purrr::list_flatten(list(x, result))
  }

  CDMConnector::dropTable(cdm = cdm, name = tableCodelist)

  result <- result[order(names(result))]

  if(isFALSE(withDetails)){
    result <- omopgenerics::newCodelist(result)
  } else{
    result <- omopgenerics::newCodelistWithDetails(result)
  }


  result

}
