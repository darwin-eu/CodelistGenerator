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
getDoseUnit <- function(cdm){

  if(isFALSE(inherits(cdm, "cdm_reference"))){
    cli::cli_abort("{.arg cdm} is not a cdm reference")
  }

  sort(unique(c(cdm$drug_strength |>
    dplyr::select("amount_unit_concept_id") |>
    dplyr::filter(!is.na(.data$amount_unit_concept_id)) |>
    dplyr::distinct() |>
    dplyr::left_join(cdm$concept,
                     by = c("amount_unit_concept_id" = "concept_id")) |>
    dplyr::pull("concept_name"),
  cdm$drug_strength |>
    dplyr::select("numerator_unit_concept_id") |>
    dplyr::filter(!is.na(.data$numerator_unit_concept_id)) |>
    dplyr::distinct() |>
    dplyr::left_join(cdm$concept,
                     by = c("numerator_unit_concept_id" = "concept_id")) |>
    dplyr::pull("concept_name"))))

}
