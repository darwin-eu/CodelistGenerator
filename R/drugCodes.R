# Copyright 2023 DARWIN EU®
#
# This file is part of IncidencePrevalence
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
#' @param withConceptDetails If FALSE, each item in the list of results (one per
#' ATC group) will contain a vector of concept IDs for each ingredient. If
#' TRUE each item in the list of results will contain a tibble with additional
#' information on the identified concepts.
#'
#' @return A named list, with each item containing a vector of descendant
#' concepts of an ATC group (if withConceptDetails was set as FALSE) or a
#' tibble with the descendant concepts along with additional details about them
#' (if withConceptDetails was set as TRUE).
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getATCCodes(cdm = cdm, level = "ATC 1st")
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getATCCodes <- function(cdm,
                        level = c("ATC 1st"),
                        name = NULL,
                        doseForm = NULL,
                        withConceptDetails = FALSE) {
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
  checkmate::assertLogical(withConceptDetails, len = 1)
  checkmate::reportAssertions(collection = errorMessage)

  atc_groups <- cdm$concept %>%
    dplyr::filter(.data$vocabulary_id == "ATC") %>%
    dplyr::filter(.data$concept_class_id %in% .env$level) %>%
    dplyr::collect()

  if (!is.null(name)) {
    atc_groups <- atc_groups %>%
      dplyr::filter(tolower(.data$concept_name) %in% tolower(.env$name))
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

    # for each item in the list - pull out concepts and name
    for (i in seq_along(atc_descendants)) {
      workingLevel <- atc_groups %>%
        dplyr::filter(.data$concept_id == names(atc_descendants)[i]) %>%
        dplyr::pull("concept_class_id")
      workingName <- atc_groups %>%
        dplyr::filter(.data$concept_id == names(atc_descendants)[i]) %>%
        dplyr::pull("concept_name")
      workingName <- stringr::str_to_lower(workingName)
      workingName <- stringr::str_replace_all(workingName, " ", "_")

      if(isFALSE(withConceptDetails)){
      atc_descendants[[i]] <- atc_descendants[[i]] %>%
        dplyr::select("concept_id") %>%
        dplyr::distinct() %>%
        dplyr::pull()
      } else {
        atc_descendants[[i]] <- atc_descendants[[i]] %>%
          dplyr::select(!"ancestor_concept_id")
      }

      names(atc_descendants)[i] <- workingName
    }
  }
  return(atc_descendants)
}

#' Get descendant codes for drug ingredients
#'
#' @param cdm cdm_reference via CDMConnector
#' @param name Names of ingredients of interest. For example, c("acetaminophen",
#' "codeine"), would result in a list of length two with the descendant
#' concepts for these two particular drug ingredients.
#' @param doseForm Only descendants codes with the specified dose form
#' will be returned. If NULL, descendant codes will be returned regardless
#' of dose form.
#' @param withConceptDetails If FALSE, each item in the list of results (one per
#' ingredient) will contain a vector of concept IDs for each ingredient. If
#' TRUE each item in the list of results will contain a tibble with additional
#' information on the identified concepts.
#'
#' @return A named list, with each item containing a vector of descendant
#' concepts of an ingredient (if withConceptDetails was set as FALSE) or a
#' tibble with the descendant concepts along with additional details about them
#' (if withConceptDetails was set as TRUE).
#' @export
#'
#' @examples
#'cdm <- mockVocabRef()
#'getDrugIngredientCodes(cdm = cdm, name = "Adalimumab")
#'DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getDrugIngredientCodes <- function(cdm,
                                   name = NULL,
                                   doseForm = NULL,
                                   withConceptDetails = FALSE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertVector(name,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertLogical(withConceptDetails, len = 1)
  checkmate::reportAssertions(collection = errorMessage)

  ingredientConcepts <- cdm$concept %>%
    dplyr::filter(.data$standard_concept == "S") %>%
    dplyr::filter(.data$concept_class_id == "Ingredient") %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::collect()

  if (!is.null(name)) {
    ingredientConcepts <- ingredientConcepts %>%
      dplyr::filter(tolower(.data$concept_name) %in% tolower(.env$name))
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
      batchSize = 500,
      doseForm = doseForm
    )
  }

      ingredientCodes <- ingredientCodes  %>%
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

    ingredientCodes <- ingredientCodes %>%
      # one row per concept + ancestor
      tidyr::pivot_longer(cols = !c("concept_id", "concept_name",
                                    "domain_id", "vocabulary_id"),
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

    # for each item in the list - pull out concepts and name
    for (i in seq_along(ingredientCodes)) {
      workingName <- ingredientConcepts %>%
        dplyr::filter(.data$concept_id == names(ingredientCodes)[[i]]) %>%
        dplyr::pull("concept_name")
      workingName <- stringr::str_to_lower(workingName)
      workingName <- stringr::str_replace_all(workingName, " ", "_")


      if(isFALSE(withConceptDetails)){
        ingredientCodes[[i]] <- ingredientCodes[[i]] %>%
          dplyr::select("concept_id") %>%
          dplyr::distinct() %>%
          dplyr::pull()
      } else {
        ingredientCodes[[i]] <- ingredientCodes[[i]] %>%
          dplyr::select(!"ancestor_concept_id")
      }

      names(ingredientCodes)[[i]] <- workingName
    }
    return(ingredientCodes)
}

fetchBatchedDescendants <- function(cdm, codes, batchSize, doseForm) {
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
      doseForm = doseForm
    )
  }
  cli::cli_progress_done()
  descendants <- dplyr::bind_rows(descendants)

  return(descendants)
}
