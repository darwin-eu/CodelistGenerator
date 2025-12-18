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

#' Get available dose units
#'
#' @description
#' Get the available dose units
#'
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A character vector with available routes.
#' @export
#'
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get all dose units available in the CDM
#' availableDoseUnits(cdm = cdm)
#' }
availableDoseUnits <- function(cdm, standardConcept = "Standard"){

  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  standardConcept <- assertStandardConcept(standardConcept)

  # If no codelist is provided, return all concept class Ids in the cdm
    doseUnits <- doseUnitsInternal(cdm,
                                   newOmopTable = "drug_strength",
                                   standardConcept = standardConcept,
                                   xx = NULL)

  return(doseUnits)
}

#' Get available dose units
#'
#' @description
#' Get the dose units associated with a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A character vector with available routes.
#' @export
#'
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
#'                              "codes2" = c(44022939L, 1830282L)))
#' associatedDoseUnits(cdm = cdm,
#'                    x = codelist)
#' }
associatedDoseUnits <- function(x,
                                cdm,
                                standardConcept = "Standard"){

  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  standardConcept <- assertStandardConcept(standardConcept)
  checkCodelist(x, allowConceptSetExpression = FALSE)

  if(inherits(x, "codelist_with_details")){
    x <- asCodelist(x)
  }

  if(inherits(x, "codelist")){
    newOmopTable <- paste0(omopgenerics::uniqueTableName(),
                           omopgenerics::uniqueId())
    codesTable <- paste0(omopgenerics::uniqueTableName(),
                         omopgenerics::uniqueId())
    cdm <- subsetOmopTable(cdm, newOmopTable, "drug_strength", x)

    doseUnits <- list()
    for( i in seq_along(x) ){
      doseUnits[[names(x)[[i]]]] <- doseUnitsInternal(cdm,
                                                      newOmopTable = newOmopTable,
                                                      codesTable = codesTable,
                                                      standardConcept = standardConcept,
                                                      xx = x[[i]])
    }
    omopgenerics::dropSourceTable(cdm, name = c(newOmopTable, codesTable))
  }

  return(doseUnits)
}


doseUnitsInternal <- function(cdm,
                              newOmopTable,
                              codesTable,
                              standardConcept,
                              xx){

  doseUnits <- cdm[[newOmopTable]]

  if(!is.null(xx)){
    cdm <- omopgenerics::insertTable(
      cdm, name = codesTable, dplyr::tibble(drug_concept_id = xx))
    doseUnits <- doseUnits |>
      dplyr::rename("drug_concept_id" = "concept_id") |>
      dplyr::inner_join(cdm[[codesTable]], by = "drug_concept_id")
  }

  doseUnits <- doseUnits |>
    dplyr::select("amount_unit_concept_id", "numerator_unit_concept_id") |>
    tidyr::pivot_longer(cols = c("amount_unit_concept_id", "numerator_unit_concept_id"),
                        values_to = "concept_id") |>
    dplyr::select(-"name") |>
    dplyr::filter(!is.na(.data$concept_id)) |>
    dplyr::distinct() |>
    dplyr::left_join(
      cdm[["concept"]] |>
        dplyr::select("concept_id", "concept_name", "standard_concept"),
      by = c("concept_id")) |>
    dplyr::mutate(standard_concept = dplyr::case_when(
      is.na(.data$standard_concept) ~ "non-standard",
      .data$standard_concept == "C" ~ "classification",
      .data$standard_concept == "S" ~ "standard"
    )) |>
    dplyr::filter(.data$standard_concept %in% .env$standardConcept) |>
    dplyr::pull("concept_name") |>
    unique() |>
    sort()

  return(doseUnits)
}
