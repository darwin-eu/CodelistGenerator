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

#' Get available relationships between concepts
#'
#' @inheritParams cdmDoc
#' @param standardConcept1  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param standardConcept2  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param domains1 Character vector with one or more of the OMOP CDM domain. If NULL, all domains are considered.
#' @param domains2 Character vector with one or more of the OMOP CDM domain. If NULL, all domains are considered.
#'
#' @return A character vector with unique concept relationship values.
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
#' # Get all relationship ids in the CDM between `Condition` and `Standard` concepts.
#' availableRelationshipIds(cdm = cdm,
#'                          standardConcept1 = "Standard",
#'                          standardConcept2 = "Standard",
#'                          domains1 = "Condition",
#'                          domains2 = "Condition")
#'
#' }
availableRelationshipIds <- function(cdm,
                            standardConcept1 = "Standard",
                            standardConcept2 = "Standard",
                            domains1 = "Condition",
                            domains2 = "Condition") {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  domains1 <- assertDomain(domains1, cdm)
  standardConcept1 <- assertStandardConcept(standardConcept1)
  domains2 <- assertDomain(domains2, cdm)
  standardConcept2 <- assertStandardConcept(standardConcept2)

  relationshipIds <- relationshipIdsInternal(cdm,
                                               newOmopTable = "concept_relationship",
                                               standardConcept1 = standardConcept1,
                                               standardConcept2 = standardConcept2,
                                               domains1 = domains1,
                                               domains2 = domains2,
                                               xx = NULL)

  return(relationshipIds)
}

#' Get available relationships with concepts in a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @param standardConcept1  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param standardConcept2  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param domains1 Character vector with one or more of the OMOP CDM domain. If NULL, all domains are considered.
#' @param domains2 Character vector with one or more of the OMOP CDM domain. If NULL, all domains are considered.
#'
#' @return A character vector with unique concept relationship values.
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
#'
# Get all relationships within a codelist
#' codelist <- newCodelist(list("codes1" = c(8479L, 4117795L),
#'                              "codes2" = c(8480L, 8600L, 8481L, 4189167L)))
#' associatedRelationshipIds(x = codelist, cdm = cdm,
#'                          standardConcept1 = c("Standard", "Non-standard", "Classification"),
#'                          standardConcept2 = c("Standard", "Non-standard", "Classification"),
#'                          domains1 = NULL,
#'                          domains2 = NULL)
#' }
associatedRelationshipIds <- function(x,
                                      cdm,
                                      standardConcept1 = "Standard",
                                      standardConcept2 = "Standard",
                                      domains1 = "Condition",
                                      domains2 = "Condition") {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  checkCodelist(x, allowConceptSetExpression = FALSE)
  domains1 <- assertDomain(domains1, cdm)
  standardConcept1 <- assertStandardConcept(standardConcept1)
  domains2 <- assertDomain(domains2, cdm)
  standardConcept2 <- assertStandardConcept(standardConcept2)

  if(inherits(x, "codelist_with_details")){
    x <- asCodelist(x)
  }

  if(inherits(x, "codelist")){
    newOmopTable <- paste0(omopgenerics::uniqueTableName(),
                           omopgenerics::uniqueId())
    codesTable <- paste0(omopgenerics::uniqueTableName(),
                         omopgenerics::uniqueId())
    cdm <- subsetOmopTable(cdm, newOmopTable, "concept_relationship", x)

    relationshipIds <- list()
    for( i in seq_along(x) ){
      relationshipIds[[names(x)[[i]]]] <- relationshipIdsInternal(cdm,
                                                                  newOmopTable = newOmopTable,
                                                                  codesTable = codesTable,
                                                                  standardConcept1 = standardConcept1,
                                                                  standardConcept2 = standardConcept2,
                                                                  domains1 = domains1,
                                                                  domains2 = domains2,
                                                                  xx = x[[i]])
    }
    omopgenerics::dropSourceTable(cdm, name = c(newOmopTable))
  }

  return(relationshipIds)
}

relationshipIdsInternal <- function(cdm, newOmopTable, codesTable, standardConcept1, standardConcept2,
                                    domains1, domains2, xx){

  relationshipIds <- cdm[[newOmopTable]]

  if(!is.null(xx)){
    cdm <- omopgenerics::insertTable(
      cdm, name = codesTable, dplyr::tibble(concept_id_1 = xx))
    relationshipIds <- relationshipIds |>
      dplyr::rename("concept_id_1" = "concept_id")  |>
      dplyr::inner_join(cdm[[codesTable]], by = "concept_id_1")
  }

  relationshipIds <- relationshipIds |>
    dplyr::left_join(
      cdm[["concept"]] |>
        dplyr::select(
          "concept_id_1" = "concept_id",
          "domain_id_1" = "domain_id",
          "standard_concept_1" = "standard_concept"
        ),
      by = c("concept_id_1")
    ) |>
    dplyr::left_join(
      cdm[["concept"]] |>
        dplyr::select(
          "concept_id_2" = "concept_id",
          "domain_id_2" = "domain_id",
          "standard_concept_2" = "standard_concept"
        ),
      by = c("concept_id_2")
    ) |>
    dplyr::mutate_at(c("domain_id_1", "domain_id_2"),
                     ~ tolower(.)) |>
    dplyr::mutate_at(c("standard_concept_1", "standard_concept_2"),
                     ~ dplyr::case_when(
                       is.na(.) ~ "non-standard",
                       . == "C" ~ "classification",
                       . == "S" ~ "standard"
                     )) |>
    dplyr::filter(.data$standard_concept_1 %in% .env$standardConcept1,
                  .data$standard_concept_2 %in% .env$standardConcept2,
                  tolower(.data$domain_id_1) %in% .env$domains1,
                  tolower(.data$domain_id_2) %in% .env$domains2)

  relationshipIds <- relationshipIds |>
    dplyr::select("relationship_id") |>
    dplyr::distinct() |>
    dplyr::pull() |>
    sort()

  return(relationshipIds)
}
