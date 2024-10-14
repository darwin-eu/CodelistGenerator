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

#' Get descendant codes for ATC levels
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level ATC level. Can be one or more of "ATC 1st", "ATC 2nd",
#' "ATC 3rd", "ATC 4th", and "ATC 5th"
#' @param name ATC name of interest. For example, c("Dermatologicals",
#' "Nervous System"), would result in a list of length two with the descendant
#' concepts for these two particular ATC groups.
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param doseUnit Only descendants codes with the specified dose unit
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose unit
#' @param routeCategory Only descendants codes with the specified route
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param type Can be "codelist", "codelist_with_details", or
#' "concept_set_expression"
#'
#' @return Concepts with their format based on the type argument.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef()
#' getATCCodes(cdm = cdm, level = "ATC 1st")
#' CDMConnector::cdmDisconnect(cdm)
#' }
getATCCodes <- function(cdm,
                        level = c("ATC 1st"),
                        name = NULL,
                        doseForm = NULL,
                        doseUnit = NULL,
                        routeCategory = NULL,
                        type = "codelist") {


  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  levelCheck <- all(level %in%
    c(
      "ATC 1st",
      "ATC 2nd",
      "ATC 3rd",
      "ATC 4th",
      "ATC 5th"
    ))
  if (!isTRUE(levelCheck)) {
    errorMessage$push(
      "- level can only be from: ATC 1st, ATC 2nd, ATC 3rd, ATC 4th, ATC 5th"
    )
  }
  checkmate::assertTRUE(levelCheck, add = errorMessage)
  checkmate::assertVector(name,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertCharacter(type, len = 1)
  checkmate::reportAssertions(collection = errorMessage)

  atc_groups <- cdm$concept %>%
    dplyr::filter(.data$vocabulary_id == "ATC") %>%
    dplyr::filter(.data$concept_class_id %in% .env$level) %>%
    dplyr::select("concept_id", "concept_name", "concept_code") %>%
    dplyr::collect()

  if (!is.null(name)) {
    atc_groups <- atc_groups %>%
      dplyr::filter(tidyWords(.data$concept_name) %in% tidyWords(.env$name))
  }

  errorMessage <- checkmate::makeAssertCollection()
  atcCheck <- nrow(atc_groups) > 0
  if (!isTRUE(atcCheck)) {
    errorMessage$push(
      "- No matching ATC codes found"
    )
  }
  checkmate::assertTRUE(atcCheck, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # to avoid potential memory problems will batch
  if (nrow(atc_groups) > 0) {
    atc_descendants <- fetchBatchedDescendants(cdm = cdm,
                                  codes = atc_groups$concept_id,
                                  batchSize = 500,
                                  doseForm = doseForm)
}

  if (nrow(atc_descendants) > 0) {
    atc_descendants <- atc_descendants %>%
      dplyr::select("concept_id", "concept_name",
                      "domain_id", "vocabulary_id",
                      "ancestor_concept_id") %>%
      # split different ancestors into multiple cols
      tidyr::separate_wider_delim(
        cols = "ancestor_concept_id",
        delim = ";",
        names_sep = "",
        too_few = "align_start"
      )

    atc_descendants <- atc_descendants %>%
      # one row per concept + ancestor
      tidyr::pivot_longer(cols = !c("concept_id", "concept_name",
                             "domain_id", "vocabulary_id"),
        names_to = NULL,
        values_to = "ancestor_concept_id",
        values_drop_na = TRUE
      )

    # to list
    # one item per concept
    atc_descendants <- split(
      x = atc_descendants,
      f = as.factor(atc_descendants$ancestor_concept_id),
      drop = TRUE
    )

    names(atc_descendants) <- dplyr::tibble(concept_id = names(atc_descendants)) |>
      dplyr::mutate(seq = dplyr::row_number()) |>
      dplyr::left_join(atc_groups |>
                         dplyr::mutate(concept_id = as.character(.data$concept_id)),
                       by= "concept_id") |>
      dplyr::mutate(new_name = paste0(.data$concept_code, "_",
                                      omopgenerics::toSnakeCase(.data$concept_name))) |>
      dplyr::arrange(seq) |>
      dplyr::pull("new_name")


    # for each item in the list - pull out concepts and name
    for (i in seq_along(atc_descendants)) {
      if(type == "codelist"){
      atc_descendants[[i]] <- atc_descendants[[i]] %>%
        dplyr::select("concept_id") %>%
        dplyr::distinct() %>%
        dplyr::pull()

      } else {
        atc_descendants[[i]] <- atc_descendants[[i]] %>%
          dplyr::select(!"ancestor_concept_id")
      }
    }
  }

  if(type == "codelist"){
    atc_descendants <- omopgenerics::newCodelist(atc_descendants)
  } else {
    atc_descendants <- omopgenerics::newCodelistWithDetails(atc_descendants)
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

#' Get descendant codes for drug ingredients
#'
#' @param cdm cdm_reference via CDMConnector
#' @param name Names of ingredients of interest. For example, c("acetaminophen",
#' "codeine"), would result in a list of length two with the descendant
#' concepts for these two particular drug ingredients.
#' @param nameStyle Name style to apply to returned list. Can be one of
#' "{concept_code}_{concept_name}", "{concept_code}", or "{concept_name}".
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param doseUnit Only descendants codes with the specified dose unit
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose unit
#' @param routeCategory Only descendants codes with the specified route
#' will be returned. If NULL, descendant codes will be returned regardless
#' of route category.
#' @param ingredientRange Used to restrict descendant codes to those
#' associated with a specific number of ingredients. Must be a vector of length
#' two with the first element the minimum number of ingredients allowed and
#' the second the maximum. A value of c(2, 2) would restrict to only concepts
#' associated with two ingredients.
#' @param type Can be "codelist", "codelist_with_details", or
#' "concept_set_expression"
#'
#' @return Concepts with their format based on the type argument.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef()
#' getDrugIngredientCodes(cdm = cdm, name = "Adalimumab",
#'                        nameStyle = "{concept_name}")
#' CDMConnector::cdmDisconnect(cdm)
#'}
getDrugIngredientCodes <- function(cdm,
                                   name = NULL,
                                   nameStyle = "{concept_code}_{concept_name}",
                                   doseForm = NULL,
                                   doseUnit = NULL,
                                   routeCategory = NULL,
                                   ingredientRange = c(1, Inf),
                                   type = "codelist") {


  if(type == "concept_set_expression"){
    cli::cli_abort("concept_set_expression not yet supported")
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertVector(name,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assert_choice(x = nameStyle,
                           choices = c("{concept_code}_{concept_name}",
                                       "{concept_code}",
                                       "{concept_name}"))
  checkmate::assertCharacter(type, len = 1)
  checkmate::reportAssertions(collection = errorMessage)

  ingredientConcepts <- cdm$concept %>%
    dplyr::filter(.data$standard_concept == "S") %>%
    dplyr::filter(.data$concept_class_id == "Ingredient") %>%
    dplyr::select("concept_id", "concept_name", "concept_code") %>%
    dplyr::collect()

  if (!is.null(name)) {
    ingredientConcepts <- ingredientConcepts %>%
      dplyr::filter(tidyWords(.data$concept_name) %in% tidyWords(.env$name))
  }

  errorMessage <- checkmate::makeAssertCollection()
  ingredientCheck <- nrow(ingredientConcepts) > 0
  if (!isTRUE(ingredientCheck)) {
    errorMessage$push(
      "- No matching Ingredient codes found"
    )
  }
  checkmate::assertTRUE(ingredientCheck, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

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
      ingredientCodes <- ingredientCodes  %>%
        dplyr::select("concept_id", "concept_name",
                      "domain_id", "vocabulary_id",
                      "standard_concept",
                      "ancestor_concept_id") %>%
      # split different ancestors into multiple cols
      tidyr::separate_wider_delim(
        cols = "ancestor_concept_id",
        delim = ";",
        names_sep = "",
        too_few = "align_start"
      )

    ingredientCodes <- ingredientCodes %>%
      # one row per concept + ancestor
      tidyr::pivot_longer(cols = !c("concept_id", "concept_name",
                                    "domain_id", "vocabulary_id",
                                    "standard_concept"),
        names_to = NULL,
        values_to = "ancestor_concept_id",
        values_drop_na = TRUE
      )

    # to list
    # one item per concept
    ingredientCodes <- split(
      x = ingredientCodes,
      f = as.factor(ingredientCodes$ancestor_concept_id),
      drop = TRUE
    )
    names(ingredientCodes) <- dplyr::tibble(concept_id = names(ingredientCodes)) |>
    dplyr::mutate(seq = dplyr::row_number()) |>
      dplyr::left_join(ingredientConcepts |>
                          dplyr::mutate(concept_id = as.character(.data$concept_id)),
                       by= "concept_id") |>
      dplyr::mutate(concept_name = paste0(omopgenerics::toSnakeCase(.data$concept_name)),
                    new_name = glue::glue(nameStyle)) |>
      dplyr::arrange(seq) |>
      dplyr::pull("new_name")

    # for each item in the list - pull out concepts and name
    for (i in seq_along(ingredientCodes)) {
      if(type == "codelist"){
        ingredientCodes[[i]] <- ingredientCodes[[i]] %>%
          dplyr::select("concept_id") %>%
          dplyr::distinct() %>%
          dplyr::pull()

      } else {
        ingredientCodes[[i]] <- ingredientCodes[[i]] %>%
          dplyr::select(!"ancestor_concept_id")
      }
    }

    if(type == "codelist"){
    ingredientCodes <- omopgenerics::newCodelist(ingredientCodes)
    } else {
    ingredientCodes <- omopgenerics::newCodelistWithDetails(ingredientCodes)
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

