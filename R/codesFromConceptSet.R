# Copyright 2023 DARWIN EU (C)
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

#' Get concept ids from a provided path to json files
#'
#' @param path Path to a file or folder containing JSONs of concept sets
#' @param cdm A cdm reference created with CDMConnector
#' @param withConceptDetails If FALSE a vector of concept IDs will be returned
#' for each concept set. If TRUE a tibble will be returned with additional
#' information on the identified concepts.
#'
#' @return Named list with concept_ids for each concept set
#' @export
#'
#' @examples
#'
codesFromConceptSet <- function(path, cdm, withConceptDetails = FALSE) {
  # initial checks
  checkInputs(path = path, cdm = cdm)

  if (dir.exists(path)) {
    conceptSets <- dplyr::tibble(concept_set_path = list.files(
      path = path,
      full.names = TRUE
    ))
  } else {
    conceptSets <- dplyr::tibble(concept_set_path = .env$path)
  }
  conceptSets <- conceptSets %>%
    dplyr::filter(tools::file_ext(.data$concept_set_path) == "json") %>%
    dplyr::mutate(
      concept_set_name =
        tools::file_path_sans_ext(basename(.data$concept_set_path))
    ) %>%
    dplyr::mutate(cohort_definition_id = dplyr::row_number())
  if (conceptSets %>% nrow() == 0) {
    cli::cli_abort(glue::glue("No 'json' file found in {path}"))
  }

  # first part: read jsons
  tryCatch(
    expr = conceptList <- readConceptSet(conceptSets),
    error = function(e) {
      cli::cli_abort(
        "The json file is not a properly formated OMOP concept set."
      )
    }
  )

  if(any(conceptList$include_mapped == TRUE)){
    exc <- paste(conceptList %>%
                   dplyr::filter(.data$include_mapped == TRUE) %>%
                   dplyr::pull("cohort_name"), collapse = "; ")
    cli::cli_abort(
      glue::glue("Mapped as TRUE not supported (found in {exc})"))
  }

  # second part: produce output list
  conceptFinalList <- formatConceptList(conceptList, cdm)

  if(isTRUE(withConceptDetails)){
    conceptFinalList <- addDetails(conceptList = conceptFinalList,
               cdm = cdm)
  }

  # return list
  return(conceptFinalList)
}



addDetails <- function(conceptList, cdm){

  for(i in seq_along(conceptList)){
    conceptList[[i]] <- dplyr::tibble(concept_id = conceptList[[i]],
                                      concept_set = names(conceptList)[i])
  }

  conceptList <- dplyr::bind_rows(conceptList) %>%
    dplyr::left_join(cdm[["concept"]] %>%
      dplyr::select("concept_id", "concept_name",
                "domain_id", "vocabulary_id"),
                     by = "concept_id",
                     copy = TRUE)

   conceptList <- split(
    x = conceptList %>% dplyr::select(!"concept_set"),
    f = as.factor(conceptList$concept_set)
  )

   return(conceptList)

}





#' Put concept ids from all cohorts of interest in the required list format
#'
#' @param conceptList table with all the concept ids read, with their respective
#' cohort, exclusion and descendant information
#' @param cdm A cdm reference created with CDMConnector
#'
#' @return list of concept_ids and respective cohort_definition_ids of interest
#' @noRd
formatConceptList <- function(conceptList, cdm) {
  conceptList <- conceptList %>%
    dplyr::filter(.data$include_descendants == FALSE) %>%
    dplyr::union(
      cdm[["concept_ancestor"]] %>%
        dplyr::select(
          "concept_id" = "ancestor_concept_id",
          "descendant_concept_id"
        ) %>%
        dplyr::inner_join(
          conceptList %>%
            dplyr::filter(.data$include_descendants == TRUE),
          copy = TRUE,
          by = "concept_id"
        ) %>%
        dplyr::select(-"concept_id") %>%
        dplyr::rename("concept_id" = "descendant_concept_id") %>%
        dplyr::collect()
    ) %>%
    dplyr::select(-"include_descendants") %>%
    dplyr::rename("drug_concept_id" = "concept_id")
  # eliminate the ones that is_excluded = TRUE
  conceptList <- conceptList %>%
    dplyr::filter(.data$is_excluded == FALSE) %>%
    dplyr::select("cohort_name", "drug_concept_id") %>%
    dplyr::anti_join(
      conceptList %>%
        dplyr::filter(.data$is_excluded == TRUE),
      by = c("cohort_name","drug_concept_id")
    )
  conceptFinalList <- list()
  for(n in conceptList[["cohort_name"]] %>% unique()) {
    conceptFinalList[[n]] <- conceptList %>%
      dplyr::filter(.data$cohort_name == n) %>%
      dplyr::select("drug_concept_id") %>%
      dplyr::pull()
  }
  return(conceptFinalList)
}

#' Get concept ids and information from a list of json files provided
#'
#' @param conceptSets paths and names of all jsons to be read
#'
#' @return raw table with all the concept_ids read, and their information
#' @noRd
readConceptSet <- function(conceptSets) {
  names <- c("CONCEPT_CLASS_ID",
             "CONCEPT_CODE",
             "CONCEPT_ID",
             "CONCEPT_NAME",
             "DOMAIN_ID",
             "INVALID_REASON",
             "INVALID_REASON_CAPTION",
             "STANDARD_CONCEPT",
             "STANDARD_CONCEPT_CAPTION",
             "VOCABULARY_ID",
             "isExcluded",
             "includeMapped",
             "includeDescendants")

  for (k in 1:nrow(conceptSets)) {
    conceptSetName <- conceptSets$concept_set_name[k]
    conceptSet <- RJSONIO::fromJSON(conceptSets$concept_set_path[k])
    conceptSet <- lapply(conceptSet$items, function(x) {
      x <- append(x, x[["concept"]])
      x[["concept"]] <- NULL
      return(x)
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(
        cohort_name = conceptSetName
      )
    # Add columns missing from the read file with default values
    conceptSet[setdiff(names, names(conceptSet))] <- as.character(NA)
    conceptSet <- conceptSet %>%
      dplyr::mutate(
        isExcluded = ifelse(is.na(.data$isExcluded), FALSE, .data$isExcluded),
        includeMapped = ifelse(
          is.na(.data$includeMapped), FALSE, .data$includeMapped
        ),
        includeDescendants = ifelse(
          is.na(.data$includeDescendants), FALSE, .data$includeDescendants
        )
      )

    if (k == 1) {
      conceptList <- conceptSet
    } else {
      conceptList <- dplyr::bind_rows(conceptList, conceptSet)
    }
  }
  conceptList <- conceptList %>%
    dplyr::select(
      "cohort_name",
      "concept_id" = "CONCEPT_ID",
      "is_excluded" = "isExcluded",
      "include_descendants" = "includeDescendants",
      "include_mapped" = "includeMapped"
    )

  return(conceptList)
}
