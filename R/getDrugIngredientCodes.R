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

#' Get descendant codes of drug ingredients
#'
#' @inheritParams cdmDoc
#' @param name Names of ingredients of interest. For example, c("acetaminophen",
#' "codeine"), would result in a list of length two with the descendant
#' concepts for these two particular drug ingredients. Users can also specify the
#' concept ID instead of the name (e.g., c(1125315, 42948451)) using a numeric vector.
#' @inheritParams nameStyleDoc
#' @inheritParams doseFormDoc
#' @inheritParams doseUnitDoc
#' @inheritParams routeCategoryDoc
#' @inheritParams ingredientRangeDoc
#' @inheritParams typeNarrowDoc
#'
#' @return Concepts with their format based on the type argument.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getDrugIngredientCodes(cdm = cdm,
#'                        name = "Adalimumab",
#'                        nameStyle = "{concept_name}")
#'}
getDrugIngredientCodes <- function(cdm,
                                   name = NULL,
                                   nameStyle = "{concept_code}_{concept_name}",
                                   doseForm = NULL,
                                   doseUnit = NULL,
                                   routeCategory = NULL,
                                   ingredientRange = c(1, Inf),
                                   type = "codelist") {

  # Initial checks
  omopgenerics::validateCdmArgument(cdm)
  nameStyle <- checkNameStyle(nameStyle)
  omopgenerics::assertChoice(type, length = 1,
                             choices = c("codelist",
                                         "concept_set_expression",
                                         "codelist_with_details"))
  omopgenerics::assertNumeric(ingredientRange, length = 2, min = 0)
  omopgenerics::assertTrue(ingredientRange[1] <= ingredientRange[2])
  omopgenerics::assertCharacter(doseForm, null = TRUE)
  omopgenerics::assertCharacter(doseUnit, null = TRUE)
  omopgenerics::assertCharacter(routeCategory, null = TRUE)

  if(type == "concept_set_expression"){
    if(!is.null(doseForm)){
      cli::cli_abort("Dose forms not yet supported for concept set expressions")
    }
    if(!is.null(doseUnit)){
      cli::cli_abort("Dose units not yet supported for concept set expressions")
    }
    if(!is.null(routeCategory)){
      cli::cli_abort("Route categorises not yet supported for concept set expressions")
    }
  }

  if(!is.null(name)){
    if(is.character(name)){
      omopgenerics::assertCharacter(name)
    }else if(is.numeric(name)){
      omopgenerics::assertNumeric(name)
    }else{
      cli::cli_abort("Argument `name` must be either a character vector with the ingredients' names, a
                                numerical vector with the ingredients' concepts ID or NULL")
    }
  }
  ingredientConcepts <- cdm$concept |>
    dplyr::filter(.data$standard_concept == "S",
                  .data$concept_class_id == "Ingredient") |>
    dplyr::select("concept_id", "concept_name", "concept_code") |>
    dplyr::collect()

  if (!is.null(name)){
    ingredientConcepts <- ingredientConcepts |>
      filterIngredientConcepts(name)

    if(nrow(ingredientConcepts) == 0) {
      if(type == "codelist"){
        return(omopgenerics::emptyCodelist())
      } else if(type == "codelist_with_details"){
        return(omopgenerics::emptyCodelistWithDetails())
      } else {
        # note, currently no empty concept set expression function
        return(omopgenerics::newConceptSetExpression(list()))
      }
    }
  }

  # simple case can just return concept set expression with descendants
  if(all(type == "concept_set_expression",
         is.null(doseForm),
         is.null(doseUnit),
         is.null(routeCategory),
         ingredientRange == c(1, Inf))){
    cse <- ingredientConcepts |>
      dplyr::mutate(name = glue::glue(.env$nameStyle)) |>
      dplyr::select("concept_id",
                    "name") |>
      dplyr::mutate("excluded" = FALSE,
                    "descendants" = TRUE,
                    "mapped" = FALSE)
    cse <- split(
      x = cse |>
        dplyr::select(!"name"),
      f = as.factor(cse$name),
      drop = TRUE
    ) |>
      omopgenerics::newConceptSetExpression()
    return(cse)
  }

  # to avoid potential memory problems will batch
  if (nrow(ingredientConcepts) > 0) {
    ingredientCodes <- fetchBatchedDescendants(cdm,
                                               codes = ingredientConcepts$concept_id,
                                               ingredientRange = ingredientRange,
                                               batchSize = 500,
                                               doseForm = doseForm
    ) |>
      dplyr::filter(.data$standard_concept == "S")
  }

  if (nrow(ingredientCodes) == 0) {
    cli::cli_warn("No descendant codes found")
    return(invisible(list()))
  }


  ingredientCodes <- ingredientCodes  |>
    dplyr::select("concept_id", "concept_name",
                  "domain_id", "vocabulary_id",
                  "standard_concept",
                  "ancestor_concept_id") |>
    # split different ancestors into multiple cols
    tidyr::separate_wider_delim(
      cols = "ancestor_concept_id",
      delim = ";",
      names_sep = "",
      too_few = "align_start"
    )

  ingredientCodes <- ingredientCodes |>
    # one row per concept + ancestor
    tidyr::pivot_longer(cols = !c("concept_id", "concept_name",
                                  "domain_id", "vocabulary_id",
                                  "standard_concept"),
                        names_to = NULL,
                        values_to = "ancestor_concept_id",
                        values_drop_na = TRUE
    )

  ingredientCodes <- ingredientCodes |>
    dplyr::left_join(
      ingredientConcepts |>
        dplyr::mutate("concept_name" = omopgenerics::toSnakeCase(.data$concept_name)) |>
        dplyr::mutate("concept_id" = as.character(.data$concept_id)) |>
        dplyr::mutate("name" = glue::glue(.env$nameStyle)) |>
        dplyr::select("concept_id", "name"),
      by = c("ancestor_concept_id" = "concept_id")
    )

  if(type == "codelist"){
    ingredientCodes <- split(
      x = ingredientCodes$concept_id,
      f = as.factor(ingredientCodes$name),
      drop = TRUE
    ) |>
      omopgenerics::newCodelist()
  }else if(type == "codelist_with_details"){
    ingredientCodes <- split(
      x = ingredientCodes,
      f = as.factor(ingredientCodes$name),
      drop = TRUE
    ) |>
      purrr::map(~dplyr::select(., -"name")) |>
      omopgenerics::newCodelistWithDetails()
  } else {
    ingredientCodes <- split(
      x = ingredientCodes|>
        dplyr::select("concept_id") |>
        dplyr::mutate("excluded" = FALSE,
                      "descendants" = FALSE,
                      "mapped" = FALSE),
      f = as.factor(ingredientCodes$name),
      drop = TRUE
    )|>
      omopgenerics::newConceptSetExpression()
  }

  if(!is.null(routeCategory)){
    ingredientCodes <- subsetOnRouteCategory(ingredientCodes,
                                             cdm = cdm,
                                             routeCategory = routeCategory)
  }

  if(!is.null(doseUnit)){
    ingredientCodes <- subsetOnDoseUnit(ingredientCodes,
                                        cdm = cdm,
                                        doseUnit = doseUnit)
  }

  return(ingredientCodes)
}

filterIngredientConcepts <- function(ingredientConcepts, name){
  if(is.character(name)){
    ingredientConcepts <- ingredientConcepts |>
      dplyr::filter(tidyWords(.data$concept_name, message = FALSE) %in%
                      tidyWords(.env$name, message = FALSE))
  }else if(is.numeric(name)){
    ingredientConcepts <- ingredientConcepts |>
      dplyr::filter(.data$concept_id %in% .env$name)
  }
  if(nrow(ingredientConcepts) == 0){
    cli::cli_warn("- No matching Ingredient codes found")
  } else if(nrow(ingredientConcepts) < length(name)){
    missingIngredientConcepts <-  dplyr::tibble(concept_name = tidyWords(.env$name)) |>
      dplyr::anti_join(ingredientConcepts |>
                         dplyr::mutate(concept_name = tidyWords(.data$concept_name)),
                       by = "concept_name") |>
      dplyr::pull("concept_name")
    cli::cli_warn("- No matching Ingredient codes found for {missingIngredientConcepts}")
  }
  ingredientConcepts
}

fetchBatchedDescendants <- function(cdm,
                                    codes,
                                    batchSize,
                                    ingredientRange = c(0, Inf),
                                    doseForm) {
  codeBatches <- split(
    codes,
    ceiling(seq_along(codes) / batchSize)
  )

  descendants <- vector("list", length(codeBatches))

  cli::cli_progress_bar(
    total = length(descendants),
    format = " -- getting descendants {cli::pb_bar} {cli::pb_current} of {cli::pb_total} batched groups"
  )

  for (i in seq_along(descendants)) {
    cli::cli_progress_update()
    x <- getDescendants(
      cdm = cdm,
      conceptId = codeBatches[[i]],
      withAncestor = TRUE
    )

    x <- list("codes" = x) |> newCodelistWithDetails()

    if(ingredientRange[1] != 1 | ingredientRange[2]!= Inf){
    x <- subsetOnIngredientRange(x,
                                 cdm = cdm,
                                 ingredientRange = ingredientRange,
                                 negate = FALSE)
    }

    if(!is.null(doseForm)){
      x <- subsetOnDoseForm(x,
                            cdm = cdm,
                            doseForm = doseForm,
                            negate = FALSE)
    }

    if(length(x$codes) == 0){
      x$codes <- dplyr::tibble(
        "ancestor_concept_id" = character(),
        "concept_id" = integer(),
        "concept_name" = character(),
        "domain_id" = character(),
        "vocabulary_id" = character(),
        "standard_concept" = character()
      )
    }
    descendants[[i]] <- x$codes
  }

  cli::cli_progress_done()
  descendants <- dplyr::bind_rows(descendants)

  return(descendants)
}

checkNameStyle <- function(nameStyle){
  x <- nameStyle %in% c("{concept_code}", "{concept_id}", "{concept_name}",
                        "{concept_code}_{concept_id}", "{concept_code}_{concept_name}", "{concept_id}_{concept_code}",
                        "{concept_id}_{concept_name}", "{concept_name}_{concept_code}", "{concept_name}_{concept_id}",
                        "{concept_code}_{concept_id}_{concept_name}", "{concept_code}_{concept_name}_{concept_id}",
                        "{concept_id}_{concept_code}_{concept_name}", "{concept_id}_{concept_name}_{concept_code}",
                        "{concept_name}_{concept_code}_{concept_id}", "{concept_name}_{concept_id}_{concept_code}")
  if(isFALSE(x)){
    cli::cli_abort("nameStyle {nameStyle} is not supported. nameStyle argument must be a choice between {{concept_name}}, {{concept_id}}, or {{concept_code}},
                   or any combination of the three separated by `_` (i.e., {{concept_id}}_{{concept_name}}.")}

  return(nameStyle)
}
