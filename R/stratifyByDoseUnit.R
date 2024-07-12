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


#' Stratify a codelist by dose unit
#'
#' @param x A codelist
#' @param cdm A cdm reference
#' @param keepOriginal Whether to keep the original codelist and append the
#' stratify (if TRUE) or just return the stratified codelist (if FALSE).
#'
#' @return A codelist
#' @export
#'
stratifyByDoseUnit <- function(x, cdm, keepOriginal = FALSE){

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
  checkmate::assertLogical(keepOriginal, len = 1)

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  result <- list()

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

    workingName <- names(x)[i]

    workingCodesWithDoseUnit <- cdm[[tableCodelist]] |>
      dplyr::left_join(cdm$concept,
                       by = "concept_id")|>
      dplyr::left_join(drugStrengthNamed,
                        by = c("concept_id" = "drug_concept_id")
      ) |>
      dplyr::select("concept_id", "domain_id",
                    "amount_concept_name",
                    "numerator_concept_name") |>
      dplyr::distinct() |>
      dplyr::collect()

    workingCodesWithDoseUnit <- workingCodesWithDoseUnit |>
      dplyr::mutate(
        unit_group = dplyr::case_when(
          !is.na(.data$amount_concept_name) ~ omopgenerics::toSnakeCase(.data$amount_concept_name),
          !is.na(.data$numerator_concept_name) ~ omopgenerics::toSnakeCase(.data$numerator_concept_name),
          tolower(.data$domain_id) == "drug" ~ "unkown_dose_unit"
        )
      ) |>
      dplyr::filter(!is.na(.data$unit_group))

    if(isTRUE(withDetails)){
      workingCodesWithDoseUnit <-  x_original[[i]] |>
        dplyr::inner_join(workingCodesWithDoseUnit,
                          by = "concept_id")
    }

    workingCodesWithDoseUnit <- split(
      workingCodesWithDoseUnit,
      workingCodesWithDoseUnit[, c("unit_group")]
    )

    if(length(workingCodesWithDoseUnit)>0){
    names(workingCodesWithDoseUnit) <- paste0(workingName, "_",
                                           names(workingCodesWithDoseUnit))
    }

    if(isFALSE(withDetails)){
      for(j in seq_along(workingCodesWithDoseUnit)){
        workingCodesWithDoseUnit[[j]] <- sort(workingCodesWithDoseUnit[[j]] |>
                                             dplyr::pull("concept_id"))

      }}

    result[[i]] <- workingCodesWithDoseUnit

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
