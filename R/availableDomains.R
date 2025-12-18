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

#' Get the domains available in the cdm
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A vector with the domains of the cdm.
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
#' # Get all domains available in the CDM for standard concepts
#' availableDomains(cdm = cdm, standardConcept = "Standard")
#' }
availableDomains <- function(cdm,
                    standardConcept = "Standard") {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  standardConcept <- assertStandardConcept(standardConcept)

    newOmopTable <- "concept"
    domains <- domainsInternal(cdm,
                               newOmopTable = newOmopTable,
                               standardConcept = standardConcept,
                               xx = NULL)

  return(domains)
}

#' Get the domains associated with a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A vector with the domains of the cdm.
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
#' # Get all domains available in a codelist
#' codelist <- newCodelist(list("codes1" = c(194152L, 1830279L, 40558872L),
#'                              "codes2" = c(44022939L)))
#' associatedDomains(x = codelist, cdm = cdm,
#'                  standardConcept = c("Non-standard", "Standard"))
#' }
associatedDomains <- function(x,
                              cdm,
                              standardConcept = "Standard") {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  standardConcept <- assertStandardConcept(standardConcept)
  checkCodelist(x, allowConceptSetExpression = FALSE)

  # If no codelist is provided, return all concept class Ids in the cdm
  if(is.null(x)){
    newOmopTable <- "concept"
    domains <- domainsInternal(cdm,
                               newOmopTable = newOmopTable,
                               standardConcept = standardConcept,
                               xx = NULL)
  }

  if(inherits(x, "codelist_with_details")){
    x <- asCodelist(x)
  }

  if(inherits(x, "codelist")){
    newOmopTable <- paste0(omopgenerics::uniqueTableName(),
                           omopgenerics::uniqueId())
    codesTable <- paste0(omopgenerics::uniqueTableName(),
                         omopgenerics::uniqueId())
    cdm <- subsetOmopTable(cdm, newOmopTable, "concept", x)

    domains <- list()
    for( i in seq_along(x) ){
      domains[[names(x)[[i]]]] <- domainsInternal(cdm,
                                                  newOmopTable = newOmopTable,
                                                  codesTable = codesTable,
                                                  standardConcept = standardConcept,
                                                  xx = x[[i]])
    }
    omopgenerics::dropSourceTable(cdm, name = c(newOmopTable, codesTable))
  }

  return(domains)
}

domainsInternal <- function(cdm, newOmopTable, codesTable, standardConcept, xx){
  conceptDb <- cdm[[newOmopTable]]

  if(!is.null(xx)){
    cdm <- omopgenerics::insertTable(
      cdm, name = codesTable, dplyr::tibble(concept_id = xx))
    conceptDb <- conceptDb |>
      dplyr::inner_join(cdm[[codesTable]], by = "concept_id")
  }

  conceptDb <- conceptDb |>
    dplyr::mutate(standard_concept = dplyr::case_when(
      is.na(.data$standard_concept) ~ "non-standard",
      .data$standard_concept == "C" ~ "classification",
      .data$standard_concept == "S" ~ "standard"
    )) |>
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  domains <- conceptDb |>
    dplyr::select("domain_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull() |>
    sort()

  return(domains)
}
