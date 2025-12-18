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

#' Subset a codelist to only those codes with a range of number of ingredients
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams ingredientRangeDoc
#' @param negate If FALSE, only concepts with the ingredient range specified will be
#' returned (both limits included). If TRUE, concepts with number of ingredients outside
#' the range will be returned.
#'
#' @return The codelist with only those concepts associated with the domain
#' (if negate = FALSE) or the codelist without those concepts associated with
#' the domain (if negate = TRUE).
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef()
#' codes <- subsetOnIngredientRange(
#'               x = newCodelist(list("codes" = c(10L, 13L))),
#'               cdm = cdm,
#'               ingredientRange = c(2, 10))
#' codes
#' }
subsetOnIngredientRange <- function(x,
                                    cdm,
                                    ingredientRange,
                                    negate = FALSE){

  # Initial checks
  checkCodelist(x, allowConceptSetExpression = FALSE)
  omopgenerics::validateCdmArgument(cdm)
  omopgenerics::assertLogical(negate, length = 1)
  omopgenerics::assertNumeric(ingredientRange, length = 2, min = 0)
  omopgenerics::assertTrue(ingredientRange[1] <= ingredientRange[2])

  if(ingredientRange[2] == Inf){
    ingredientRange[2] <- 9999999
  }

  x_original <- x
  if(inherits(x_original, "codelist_with_details")){
    x <- asCodelist(x)
    newX <- omopgenerics::emptyCodelistWithDetails()
  }
  if(inherits(x_original, "codelist")){
    newX <- omopgenerics::emptyCodelist()
  }

  result <- list()
  newOmopTable <- paste0(omopgenerics::uniqueTableName(),
                         omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cdm <- CDMConnector::insertTable(cdm,
                                     name = newOmopTable,
                                     table = dplyr::tibble("concept_id" = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    if(isTRUE(negate)){
      result[[i]] <- addIngredientCount(cdm = cdm, concepts = cdm[[newOmopTable]]) |>
        dplyr::filter(.data$ingredient_count < !!ingredientRange[1] |
                        .data$ingredient_count > !!ingredientRange[2]) |>
        dplyr::select(!c("ingredient_count")) |>
        dplyr::collect()
    }else{
      result[[i]] <- addIngredientCount(cdm = cdm, concepts = cdm[[newOmopTable]]) |>
        dplyr::filter(.data$ingredient_count >= !!ingredientRange[1],
                      .data$ingredient_count <= !!ingredientRange[2]) |>
        dplyr::select(!c("ingredient_count")) |>
        dplyr::collect()
    }

    if(inherits(x_original, "codelist_with_details")){
      newX[[names(x_original)[[i]]]] <- x_original[[i]] |>
        dplyr::inner_join(
          result[[i]], by = "concept_id"
        ) |>
        dplyr::arrange(.data$concept_id)
    }
    if(inherits(x_original, "codelist")){
      newX[[names(x_original)[[i]]]] <- result[[i]] |>
        dplyr::pull("concept_id") |>
        sort()
    }
  }

  newX <- dropEmptyCodelist(x_original, newX)

  omopgenerics::dropSourceTable(cdm = cdm, name = newOmopTable)

  return(newX)
}
