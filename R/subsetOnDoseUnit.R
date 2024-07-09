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

#' Subset a codelist to only those with a particular dose unit
#'
#' @param x Codelist
#' @param cdm A cdm reference
#' @param doseUnit Dose unit. Use getDoseUnit() to find the available
#' dose units in a cdm
#'
#' @return The codelist with only those concepts associated with the
#' dose unit
#' @export
#'
subsetOnDoseUnit <- function(x, cdm, doseUnit){

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
  if(isFALSE(is.character(doseUnit))){
    cli::cli_abort("doseUnit must be a character vector")
  }

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())


  drugStrengthNamed <- cdm$drug_strength |>
    dplyr::left_join(cdm$concept |>
                       dplyr::select("concept_id",
                                     "concept_name"),
                     by= c("amount_unit_concept_id"= "concept_id")) |>
    dplyr::rename("amount_concept_name" = "concept_name") |>
    dplyr::left_join(cdm$concept |>
                        dplyr::select("concept_id",
                                      "concept_name"),
                      by= c("numerator_unit_concept_id"= "concept_id")) |>
    dplyr::rename("numerator_concept_name" = "concept_name")

  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    x[[i]] <- cdm[[tableCodelist]] |>
      dplyr::inner_join(drugStrengthNamed,
                        by = c("concept_id" = "drug_concept_id")
      ) |>
      dplyr::select("concept_id",
                    "amount_concept_name",
                    "numerator_concept_name") |>
      dplyr::distinct() |>
      dplyr::collect()

    x[[i]] <- x[[i]] |>
      dplyr::filter(tolower(.data$amount_concept_name) %in% tolower(.env$doseUnit) |
                    tolower(.data$numerator_concept_name) %in% tolower(.env$doseUnit)) |>
      dplyr::pull("concept_id")

    x[[i]] <- sort(unique(x[[i]]))

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
