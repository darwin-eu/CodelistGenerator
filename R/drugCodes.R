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

#' Get the descendant codes of Anatomical Therapeutic Chemical (ATC) classification codes
#'
#' @inheritParams cdmDoc
#' @inheritParams levelATCDoc
#' @param name ATC name of interest. For example, c("Dermatologicals",
#' "Nervous System"), would result in a list of length two with the descendant
#' concepts for these two particular ATC groups.
#' @inheritParams nameStyleDoc
#' @inheritParams doseFormDoc
#' @inheritParams doseUnitDoc
#' @inheritParams routeCategoryDoc
#' @inheritParams typeNarrowDoc
#'
#' @return Concepts with their format based on the type argument
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getATCCodes(cdm = cdm, level = "ATC 1st")
#' }
getATCCodes <- function(cdm,
                        level = c("ATC 1st"),
                        name = NULL,
                        nameStyle = "{concept_code}_{concept_name}",
                        doseForm = NULL,
                        doseUnit = NULL,
                        routeCategory = NULL,
                        type = "codelist") {

  # initial checks
  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(level, c("ATC 1st", "ATC 2nd", "ATC 3rd", "ATC 4th", "ATC 5th"))
  omopgenerics::assertCharacter(name, null = T)
  nameStyle <- checkNameStyle(nameStyle)
  omopgenerics::assertCharacter(doseForm, null = T)
  omopgenerics::assertCharacter(doseUnit, null = T)
  omopgenerics::assertCharacter(routeCategory, null = T)
  omopgenerics::assertCharacter(type, len = 1)

  atc_groups <- cdm$concept |>
    dplyr::filter(.data$vocabulary_id == "ATC") |>
    dplyr::filter(.data$concept_class_id %in% .env$level) |>
    dplyr::select("concept_id", "concept_name", "concept_code") |>
    dplyr::collect()

  if (!is.null(name)) {
    atc_groups <- atc_groups |>
      dplyr::filter(tidyWords(.data$concept_name) %in% tidyWords(.env$name))
  }

  if (nrow(atc_groups) > 0) {
    # to avoid potential memory problems will batch
    atc_descendants <- fetchBatchedDescendants(cdm = cdm,
                                               codes = atc_groups$concept_id,
                                               batchSize = 500,
                                               doseForm = doseForm)
  }else{
    cli::cli_abort(
      "- No matching ATC codes found"
    )
  }

  if (nrow(atc_descendants) > 0) {
    atc_descendants <- atc_descendants |>
      dplyr::select("concept_id", "concept_name",
                    "domain_id", "vocabulary_id",
                    "ancestor_concept_id") |>
      # split different ancestors into multiple cols
      tidyr::separate_wider_delim(
        cols = "ancestor_concept_id",
        delim = ";",
        names_sep = "",
        too_few = "align_start"
      )

    atc_descendants <- atc_descendants |>
      # one row per concept + ancestor
      tidyr::pivot_longer(cols = !c("concept_id", "concept_name",
                                    "domain_id", "vocabulary_id"),
                          names_to = NULL,
                          values_to = "ancestor_concept_id",
                          values_drop_na = TRUE
      )

    atc_descendants <- atc_descendants |>
      dplyr::left_join(
        atc_groups |>
          dplyr::mutate("concept_name" = omopgenerics::toSnakeCase(.data$concept_name),
                        "concept_id" = as.character(.data$concept_id)) |>
          dplyr::mutate("name" = glue::glue(.env$nameStyle)) |>
          dplyr::select("concept_id", "name"),
        by = c("ancestor_concept_id" = "concept_id")
      )

    if(type == "codelist"){
      atc_descendants <- split(
        x = atc_descendants$concept_id,
        f = as.factor(atc_descendants$name),
        drop = TRUE
      ) |>
        omopgenerics::newCodelist()
    }else if(type == "codelist_with_details"){
      atc_descendants <- split(
        x = atc_descendants,
        f = as.factor(atc_descendants$name),
        drop = TRUE
      ) |>
        purrr::map(~dplyr::select(., -"name")) |>
        omopgenerics::newCodelistWithDetails()
    }
  }


  if(!is.null(routeCategory)){
    atc_descendants <- subsetOnRouteCategory(atc_descendants,
                                             cdm = cdm,
                                             routeCategory = routeCategory)
  }

  if(!is.null(doseUnit)){
    atc_descendants <- subsetOnDoseUnit(atc_descendants,
                                        cdm = cdm,
                                        doseUnit = doseUnit)
  }

  return(atc_descendants)
}

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
#' cdm <- mockVocabRef()
#' getDrugIngredientCodes(cdm = cdm, name = "Adalimumab",
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

  # initial checks
  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }

  omopgenerics::assertClass(cdm, "cdm_reference")
  nameStyle <- checkNameStyle(nameStyle)
  omopgenerics::assertChoice(type, length = 1, choices = c("codelist", "codelist_with_details"))
  omopgenerics::assertNumeric(ingredientRange, length = 2, min = 0)
  omopgenerics::assertTrue(ingredientRange[1] <= ingredientRange[2])
  omopgenerics::assertCharacter(doseForm, null = TRUE)
  omopgenerics::assertCharacter(doseUnit, null = TRUE)
  omopgenerics::assertCharacter(routeCategory, null = TRUE)

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
    dplyr::filter(.data$standard_concept == "S") |>
    dplyr::filter(.data$concept_class_id == "Ingredient") |>
    dplyr::select("concept_id", "concept_name", "concept_code") |>
    dplyr::collect()

  if (!is.null(name)){
    ingredientConcepts <- ingredientConcepts |>
      filterIngredientConcepts(name)
  }

  omopgenerics::assertTrue(nrow(ingredientConcepts) > 0,
                           msg = "- No matching Ingredient codes found")

  # to avoid potential memory problems will batch
  if (nrow(ingredientConcepts) > 0) {
    ingredientCodes <- fetchBatchedDescendants(cdm,
                                               codes = ingredientConcepts$concept_id,
                                               ingredientRange = ingredientRange,
                                               batchSize = 500,
                                               doseForm = doseForm
    )
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
    ingredientConcepts |>
      dplyr::filter(tidyWords(.data$concept_name) %in% tidyWords(.env$name))
  }else if(is.numeric(name)){
    ingredientConcepts |>
      dplyr::filter(.data$concept_id %in% .env$name)
  }
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
    descendants[[i]] <- getDescendants(
      cdm = cdm,
      conceptId = codeBatches[[i]],
      withAncestor = TRUE,
      ingredientRange = ingredientRange,
      doseForm = doseForm
    )
  }
  cli::cli_progress_done()
  descendants <- dplyr::bind_rows(descendants)

  return(descendants)
}

