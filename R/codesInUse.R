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

#' Use achilles counts to filter a codelist to keep only the codes
#' used in the database
#'
#' @param x A codelist
#' @param cdm cdm_reference via CDMConnector
#' @param minimumCount Any codes with a frequency under this will be removed.
#' @param table cdm table
#'
#' @return Use achilles counts to filter codelist to only the codes used in the database
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#'                            keywords = "arthritis",
#'                            domains = "Condition",
#'                            includeDescendants = FALSE)
#' x <- restrictToCodesInUse(list("cs1" = codes$concept_id,
#'                                "cs2" = 999),
#'                                 cdm = cdm)
#'
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
restrictToCodesInUse <- function(x,
                               cdm,
                               minimumCount = 0L,
                               table = c("condition_occurrence",
                                         "device_exposure",
                                         "drug_exposure",
                                         "measurement",
                                         "observation",
                                         "procedure_occurrence",
                                         "visit_occurrence")){


  if(is.null(cdm[["achilles_results"]])){
    cli::cli_abort("Achilles results must be in the cdm reference")
  }

  dbCodes <- codesInUse(cdm = cdm,
                        minimumCount = minimumCount,
                        table = table)

  if(is.null(dbCodes)){
  for(i in seq_along(x)){
    cli::cli_inform("No codes from any codelist found in the database")
    return(invisible(omopgenerics::emptyCodelist()))
  }
} else {
  for(i in seq_along(x)){
    x[[i]] <- intersect(x[[i]], dbCodes)
    if(!length(x[[i]]) >= 1){
    cli::cli_inform("No codes from codelist {names(x)[i]} found in the database")
    }
  }
}

x <- vctrs::list_drop_empty(x)

if(length(x) == 0){
  return(invisible(omopgenerics::emptyCodelist()))
}

x

}

#' Use achilles counts to get codes used in the database
#'
#' @param cdm cdm_reference via CDMConnector
#' @param minimumCount Any codes with a frequency under this will be removed.
#' @param table cdm table
#'
#' @return A list of integers indicating codes being used in the database.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef("database")
#' x <- codesInUse(cdm = cdm)
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
codesInUse <- function(cdm,
                       minimumCount = 0,
                       table = c("condition_occurrence",
                                 "device_exposure",
                                 "drug_exposure",
                                 "measurement",
                                 "observation",
                                 "procedure_occurrence",
                                 "visit_occurrence")){

  if(is.null(cdm[["achilles_results"]])){
    cli::cli_abort("Achilles results must be in the cdm reference")
  }

  codes <- fetchAchillesCodesInUse(cdm, minimumCount = minimumCount)

  codes
}

#' Use achilles counts to get source codes used in the database
#'
#' @param cdm cdm_reference via CDMConnector
#' @param table cdm table
#'
#' @return A list of source codes used in the database.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef("database")
#' x <- sourceCodesInUse(cdm = cdm)
#' x
#' CDMConnector::cdmDisconnect(cdm)
#' }
sourceCodesInUse <- function(cdm,
                                 table = c("condition_occurrence",
                                           "device_exposure",
                                           "drug_exposure",
                                           "measurement",
                                           "observation",
                                           "procedure_occurrence",
                                           "visit_occurrence")){

    if(is.null(cdm[["achilles_results"]])){
    cli::cli_abort("Achilles results must be in the cdm reference")
    }
    codes <- fetchAchillesSourceCodesInUse(cdm)

  codes
}

unmappedSourceCodesInUse <- function(cdm,
                                         table = c("condition_occurrence",
                                                   "device_exposure",
                                                   "drug_exposure",
                                                   "measurement",
                                                   "observation",
                                                   "procedure_occurrence",
                                                   "visit_occurrence")){

  # note, no achilles query for this so will have to query the cdm

  codes <- list()
  for(i in seq_along(table)){
    workingTable <- table[i]
    standardConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_concept_id",
      workingTable == "device_exposure" ~ "device_concept_id",
      workingTable == "drug_exposure" ~ "drug_concept_id",
      workingTable == "measurement" ~ "measurement_concept_id",
      workingTable == "observation" ~ "observation_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_concept_id",
      workingTable == "visit_occurrence" ~ "visit_concept_id"
    )

    workingConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_source_concept_id",
      workingTable == "device_exposure" ~ "device_source_concept_id",
      workingTable == "drug_exposure" ~ "drug_source_concept_id",
      workingTable == "measurement" ~ "measurement_source_concept_id",
      workingTable == "observation" ~ "observation_source_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_source_concept_id",
      workingTable == "visit_occurrence" ~ "visit_source_concept_id"
    )

    # keep unmapped codes
    codes[[i]] <- as.integer(cdm[[workingTable]] %>%
      dplyr::filter(!!rlang::sym(standardConcept) == 0) %>%
      dplyr::select(dplyr::all_of(workingConcept)) %>%
      dplyr::distinct() %>%
      dplyr::pull())
    codes[[i]] <- stats::na.omit(codes[[i]])
  }

  codes <- unlist(codes)

  codes
}

fetchAchillesCodesInUse <- function(cdm, minimumCount = 0, collect = TRUE){
 codes <- cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id %in%
      c(
        401, # condition occurrence
        701, # drug_exposure
        801, # observation
        1801, # measurement
        201, # visit_occurrence
        601, # procedure_occurrence
        2101 # device_exposure
      )) %>%
    dplyr::filter(.data$count_value >= .env$minimumCount) %>%
    dplyr::select("concept_id" = "stratum_1") %>%
    dplyr::distinct() %>%
    dplyr::mutate(concept_id = as.integer(.data$concept_id))

 if(isTRUE(collect)){
  codes <- codes %>%
    dplyr::pull("concept_id")
 }

 codes

}

fetchAchillesSourceCodesInUse <- function(cdm, minimumCount = 0){
  cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id %in%
                    c(
                      425, # condition occurrence
                      725, # drug_exposure
                      825, # observation
                      1825, # measurement
                      225, # visit_occurrence
                      625, # procedure_occurrence
                      2125 # device_exposure
                    )) %>%
    dplyr::filter(.data$count_value >= .env$minimumCount) %>%
    dplyr::select("stratum_1") %>%
    dplyr::distinct() %>%
    dplyr::mutate(stratum_1 = as.integer(.data$stratum_1)) %>%
    dplyr::pull("stratum_1")
}
