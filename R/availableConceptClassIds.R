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

#' Get the available concept classes used in a given set of domains
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#' @inheritParams domainDoc
#'
#' @return The concept classes
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Get all available concept_class_ids in the CDM
#' availableConceptClassIds(cdm,
#'                          standardConcept = "Standard")
#'
#' # Get all available concept_class_ids in the CDM for a specific domain
#' availableConceptClassIds(cdm,
#'                          standardConcept = "Standard",
#'                          domain = "Condition")
#'
#' # Notice that this corresponds to the information provided by `concept_class_id`
#' # column in the `concept` table
#' }
availableConceptClassIds <- function(cdm,
                            standardConcept = "Standard",
                            domain = NULL) {
  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  domain <- assertDomain(domain, cdm)
  standardConcept <- assertStandardConcept(standardConcept)

    conceptClassIds <- conceptClassIdsInternal(cdm,
                                               newOmopTable = "concept",
                                               domain = domain,
                                               standardConcept = standardConcept,
                                               xx = NULL)

  return(conceptClassIds)
}

#' Get the concept classes associated with a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#' @inheritParams domainDoc
#'
#' @return The concept classes
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmFromDataset(datasetName = "GiBleed")
#'
#' # Get concept_class_ids in a codelist
#' x <- newCodelist(list("codes1" = c(1118088L, 40213201L, 35208414L),
#'                       "codes2" = c(1557272L, 4336464L, 4295880L)))
#' associatedConceptClassIds(x, cdm,
#'                          standardConcept = "Standard")
#'
#' # Notice that this corresponds to the information provided by `concept_class_id`
#' # column in the `concept` table
#' }
associatedConceptClassIds <- function(x,
                                      cdm,
                                     standardConcept = "Standard",
                                     domain = NULL) {
  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  domain <- assertDomain(domain, cdm)
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
    cdm <- subsetOmopTable(cdm, newOmopTable, "concept", x)

    conceptClassIds <- list()
    for( i in seq_along(x) ){
      conceptClassIds[[names(x)[[i]]]] <- conceptClassIdsInternal(cdm,
                                                                  newOmopTable = newOmopTable,
                                                                  codesTable = codesTable,
                                                                  domain = domain,
                                                                  standardConcept = standardConcept,
                                                                  xx = x[[i]])
    }
    omopgenerics::dropSourceTable(cdm, name = c(newOmopTable, codesTable))
  }

  return(conceptClassIds)
}

conceptClassIdsInternal <- function(cdm, newOmopTable, codesTable, domain, standardConcept, xx){

  conceptDb <- cdm[[newOmopTable]]

  if(!is.null(xx)){
    cdm <- omopgenerics::insertTable(
      cdm, name = codesTable, dplyr::tibble(concept_id = xx))
    conceptDb <- conceptDb |>
      dplyr::inner_join(cdm[[codesTable]], by = "concept_id")
  }

  conceptDb <- conceptDb |>
    dplyr::filter(tolower(.data$domain_id) %in% .env$domain) |>
    dplyr::mutate(standard_concept = dplyr::case_when(
      is.na(.data$standard_concept) ~ "non-standard",
      .data$standard_concept == "C" ~ "classification",
      .data$standard_concept == "S" ~ "standard"
    )) |>
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  conceptClassId <- conceptDb |>
    dplyr::select("concept_class_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull()

  conceptClassId <- sort(conceptClassId)

  return(conceptClassId)
}
