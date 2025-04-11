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


#' Compare overlap between two sets of codes
#'
#' @param codelist1 Output of getCandidateCodes or a codelist
#' @param codelist2 Output of getCandidateCodes.
#'
#' @return Tibble with information on the overlap of codes in both codelists.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' codes1 <- getCandidateCodes(
#'  cdm = cdm,
#'  keywords = "Arthritis",
#'  domains = "Condition",
#'  includeDescendants = TRUE
#' )
#' codes2 <- getCandidateCodes(
#'  cdm = cdm,
#'  keywords = c("knee osteoarthritis", "arthrosis"),
#'  domains = "Condition",
#'  includeDescendants = TRUE
#')
#' compareCodelists(
#'  codelist1 = codes1,
#'  codelist2 = codes2
#' )
#' }
compareCodelists <- function(codelist1,
                             codelist2) {

  if(is.list(codelist1) && !is.data.frame(codelist1)){
    if(length(codelist1)>1){
    cli::cli_abort("Codelist must be singular")
    }
    codelist1 <- omopgenerics::newCodelist(codelist1)
    codelist1 <- dplyr::tibble(concept_id = codelist1[[1]],
                               concept_name = NA_character_)
  }

  if(is.list(codelist2) && !is.data.frame(codelist2)){
    if(length(codelist2)>1){
      cli::cli_abort("Codelist must be singular")
    }
    codelist2 <- omopgenerics::newCodelist(codelist2)
    codelist2 <- dplyr::tibble(concept_id = codelist2[[1]],
                               concept_name = NA_character_)
  }

  # initial checks
  omopgenerics::assertTable(codelist1, columns = c("concept_id", "concept_name"))
  omopgenerics::assertTable(codelist2, columns = c("concept_id", "concept_name"))

  codes <- dplyr::full_join(codelist1 |>
                            dplyr::select("concept_id", "concept_name") |>
                            dplyr::mutate(codelist_1 = 1) |>
                            dplyr::distinct(),
                          codelist2 |>
                            dplyr::select("concept_id", "concept_name") |>
                            dplyr::mutate(codelist_2 = 1) |>
                            dplyr::distinct(),
                          by = c("concept_id", "concept_name"))

  codes <- codes |>
    dplyr::mutate(
      codelist =
        dplyr::case_when(
          !is.na(codelist_1) & is.na(codelist_2) ~ "Only codelist 1",
          is.na(codelist_1) & !is.na(codelist_2) ~ "Only codelist 2",
          !is.na(codelist_1) & !is.na(codelist_2) ~ "Both"
        ))

  return(codes)
}
