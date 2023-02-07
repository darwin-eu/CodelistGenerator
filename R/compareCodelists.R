# Copyright 2022 DARWIN EU®
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


#' Compare two codelists
#'
#' @param codelist1 Output of getCandidateCodes
#' @param codelist2 Output of getCandidateCodes
#'
#' @return tibble
#' @export
#'
#' @examples
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
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
compareCodelists <- function(codelist1,
                              codelist2) {

  ## checks for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(codelist1,
    add = errorMessage
  )
  checkmate::assert_tibble(codelist2,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist1)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist2)),
    add = errorMessage
  )
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)


  all <- dplyr::bind_rows(codelist1, codelist2) %>%
    dplyr::select(c("concept_id", "concept_name"))
  duplicates <- all[duplicated(all), ]
  unique <- unique(all)

  # function to return new column which
  # indicate which codelist the concept came from
  # If returns "Both" it means the concept contain
  # in both codelists
  unique$codelist <- dplyr::if_else(is.na(
    match(
      glue::glue("{unique$concept_id};{unique$concept_name}"),
      glue::glue("{duplicates$concept_id};{duplicates$concept_name}")
    )
  ), dplyr::if_else(is.na(
    match(
      glue::glue("{unique$concept_id};{unique$concept_name}"),
      glue::glue("{codelist1$concept_id};{codelist1$concept_name}")
    )
  ),
  "Only codelist 2",
  "Only codelist 1"),
  "Both")

  return(unique)
}
