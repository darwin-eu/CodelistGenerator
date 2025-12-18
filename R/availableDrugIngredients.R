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

#' Get the names of all available drug ingredients
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A vector containing the concept names for all ingredient level codes
#' found in the concept table of cdm.
#'
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
#' # Get all drug ingredients available in the CDM for standard concepts
#' availableDrugIngredients(cdm = cdm)
#'}
availableDrugIngredients <- function(cdm, standardConcept = "Standard") {

  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  standardConcept <- assertStandardConcept(standardConcept)

  drugIngredients <- drugIngredientsInternal(cdm,
                                             newOmopTable = "concept",
                                             standardConcept = standardConcept,
                                             xx = NULL)

  return(drugIngredients)
}

#' Get the names of drug ingredients associated with codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A vector containing the concept names for all ingredient level codes
#' found in the concept table of cdm.
#'
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
#' # Get all drug ingredients associated with a codelist
#' codelist <- newCodelist(list("codes1" = c(37498042L),
#'                              "codes2" = c( 42899580L, 35741956L)))
#' associatedDrugIngredients(x = codelist, cdm = cdm,
#'                          standardConcept = c("Standard", "Non-standard"))
#'}
associatedDrugIngredients <- function(x, cdm, standardConcept = "Standard") {

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
    cdm <- subsetOmopTable(cdm, newOmopTable, "concept", x)

    drugIngredients <- list()
    for( i in seq_along(x) ){
      drugIngredients[[names(x)[[i]]]] <- drugIngredientsInternal(cdm,
                                                                  newOmopTable = newOmopTable,
                                                                  codesTable = codesTable,
                                                                  standardConcept = standardConcept,
                                                                  xx = x[[i]])
    }
    omopgenerics::dropSourceTable(cdm, name = c(newOmopTable, codesTable))
  }

  return(drugIngredients)
}


drugIngredientsInternal <- function(cdm,
                                    newOmopTable,
                                    codesTable,
                                    standardConcept,
                                    xx){

  ingredientConcepts <- cdm[[newOmopTable]]

  if(!is.null(xx)){
    cdm <- omopgenerics::insertTable(
      cdm, name = codesTable, dplyr::tibble(descendant_concept_id = xx))

    ingredientConcepts <- cdm$concept_ancestor |>
      dplyr::select("ancestor_concept_id", "descendant_concept_id") |>
      dplyr::inner_join(cdm[[codesTable]], by = "descendant_concept_id") |>
      dplyr::select("concept_id" = "ancestor_concept_id") |>
      dplyr::left_join(cdm$concept |>
                         dplyr::select("concept_id",
                                       "concept_name",
                                       "concept_class_id",
                                       "standard_concept"),
                       by = "concept_id") |>
      dplyr::mutate(standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard"
      )) |>
      dplyr::filter(.data$concept_class_id == "Ingredient",
                    .data$standard_concept %in% .env$standardConcept) |>
      dplyr::select("concept_name") |>
      dplyr::distinct() |>
      dplyr::pull("concept_name") |>
      sort()
  } else {
    ingredientConcepts <- ingredientConcepts |>
      dplyr::filter(.data$concept_class_id == "Ingredient") |>
      dplyr::mutate(standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard"
      )) |>
      dplyr::filter(.data$standard_concept %in% .env$standardConcept) |>
      dplyr::pull("concept_name")|>
      sort()
  }


}
