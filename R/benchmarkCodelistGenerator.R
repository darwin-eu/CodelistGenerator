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

#' Run benchmark of codelistGenerator analyses
#'
#' @param cdm a CDM reference object
#'
#' @return a tibble with time taken for different analysis
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#'
#' cdm <- mockVocabRef()
#'
#' timings <- benchmarkCodelistGenerator(cdm)
#' }
#'

benchmarkCodelistGenerator <- function(cdm){

  # Initial checks ----
  omopgenerics::validateCdmArgument(cdm)

  timings <- list()

  # Create vocabulary based codelists ----
  ## getDrugIngredientCodes ----
  tictoc::tic()
  x <- getDrugIngredientCodes(cdm = cdm,
                              name = c("acetaminophen", "codein", "adalimumab"),
                              nameStyle = "{concept_code}_{concept_name}",
                              doseForm  = NULL,
                              doseUnit  = NULL,
                              routeCategory = NULL,
                              ingredientRange = c(1,Inf),
                              type = "codelist")
  t <- tictoc::toc(quiet = TRUE)

  timings[["getDrugIngredientCodes"]] <- dplyr::tibble(
    task = "getting drug ingredient codes (acetaminophen, codein, adalimumab)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  ## getATCCodes ----
  tictoc::tic()
  x <- getATCCodes(cdm = cdm,
                   level = c("ATC 1st"),
                   name = c("Alimentary tract and metabolism"),
                   nameStyle = "{concept_code}_{concept_name}",
                   doseForm = NULL,
                   doseUnit = NULL,
                   routeCategory = NULL,
                   type = "codelist")
  t <- tictoc::toc(quiet = TRUE)

  timings[["getATCCodes"]] <- dplyr::tibble(
    task = "getting ATC codes (ATC 1st level, 1 name of interest)",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  ## getCandidateCodes ----
  tictoc::tic()
  x <- getCandidateCodes(
    cdm,
    keywords = "dementia",
    exclude = c("child"),
    domains = NULL,
    standardConcept = "Standard",
    searchInSynonyms = TRUE,
    searchNonStandard = TRUE,
    includeDescendants = TRUE,
    includeAncestor = TRUE
  )
  t <- tictoc::toc(quiet = TRUE)

  timings[["getCandidateCodes"]] <- dplyr::tibble(
    task = "getting candidate codes for dementia, excluding `child`, within all domains, and all options set to TRUE",
    time_taken_secs = as.numeric(t$toc - t$tic)
  )

  # Prepare summarised result -----
  timings <- dplyr::bind_rows(timings) |>
    dplyr::mutate(time_taken_mins = round(.data$time_taken_secs / 60, 2)) |>
    dplyr::mutate(dbms = attr(attr(cdm, "cdm_source"), "source_type")) |>
    dplyr::mutate(n_person = cdm$person |>
                    dplyr::count() |>
                    dplyr::pull()) |>
    dplyr::mutate(n_concepts = cdm$concept |>
                    dplyr::select("concept_id") |>
                    dplyr::pull() |>
                    dplyr::n_distinct())

  # As a summarised result
  timings <- timings |>
    dplyr::mutate(
      "result_id" = 1L,
      "cdm_name"  = omopgenerics::cdmName(cdm),
      "group_name"  = "task",
      "group_level" = stringr::str_to_sentence(gsub("_"," ",.data$task)),
      "strata_name" = "overall",
      "strata_level"   = "overall",
      "variable_name"  = "overall",
      "variable_level" = "overall",
      "estimate_name"  = "time_taken_minutes",
      "estimate_type"  = "numeric",
      "estimate_value" = as.character(.data$time_taken_mins),
      "additional_name" = paste0(
        "dbms &&& n_person &&& ",
        "n_concepts"
      ),
      additional_level = paste0(
        .data$dbms, " &&& ",
        formatC(.data$n_person, big.mark = ","), " &&& ",
        formatC(.data$n_concepts, big.mark = ",")
      )
    ) |>
    dplyr::select(dplyr::all_of(
      colnames(omopgenerics::emptySummarisedResult())
    )) |>
    omopgenerics::newSummarisedResult(settings = dplyr::tibble(
      result_id = 1L,
      result_type = "CodelistGenerator benchmark",
      package_name = "CodelistGenerator",
      package_version = as.character(utils::packageVersion("CodelistGenerator"))
    ))

  return(timings)
}
