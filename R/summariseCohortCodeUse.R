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


#' Summarise code use among a cohort in the cdm reference
#'
#' @inheritParams cdmDoc
#' @param cohortTable A cohort table from the cdm reference.
#' @param x A codelist, codelist_with_details, or a concept_set. See `newCodelist()`,
#' `newCodelistWithDetails()`, `newConceptSetExpression()` functions for more details.
#'  If set to NULL, the codelist associated with the cohort will be used.
#' @inheritParams xDocCohort
#' @param cohortId A vector of cohort IDs to include
#' @param timing When to assess the code use relative cohort dates. This can
#' be "any"(code use any time by individuals in the cohort) or  "entry" (code
#' use on individuals' cohort start date).
#' @inheritParams countByDoc
#' @inheritParams byConceptDoc
#' @inheritParams bySexDoc
#' @inheritParams byYearDoc
#' @inheritParams ageGroupDoc
#' @param useSourceCodes Whether the codelist provided contains source codes
#' (TRUE) or standard codes (FALSE).
#' @param codelistType The reason for the codelist. Can be "index event",
#' "inclusion criteria", or "exit criteria". Only apply when x is NULL and using
#' codes associated with a cohort.
#'
#' @return A tibble with results overall and, if specified, by strata
#' @export
#'
#' @examples
#' \dontrun{
#' library(CodelistGenerator)
#' library(duckdb)
#' library(DBI)
#' library(CDMConnector)
#' con <- dbConnect(duckdb(),
#'                  dbdir = eunomiaDir())
#' cdm <- cdmFromCon(con,
#'                  cdmSchema = "main",
#'                  writeSchema = "main")
#' cdm <- generateConceptCohortSet(cdm = cdm,
#'                   conceptSet = list(a = 260139,
#'                                     b = 1127433),
#'                   name = "cohorts",
#'                   end = "observation_period_end_date",
#'                   overwrite = TRUE)
#'
#'results_cohort_mult <-
#'summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = c(260139,19133873))),
#'                       cdm = cdm,
#'                       cohortTable = "cohorts",
#'                       timing = "entry")
#'
#'results_cohort_mult
#'CDMConnector::cdmDisconnect(cdm)
#'}
summariseCohortCodeUse <- function(cdm,
                                   cohortTable,
                                   x = NULL,
                                   cohortId = NULL,
                                   timing = "any",
                                   countBy = c("record", "person"),
                                   byConcept = TRUE,
                                   byYear = FALSE,
                                   bySex = FALSE,
                                   ageGroup = NULL,
                                   useSourceCodes = FALSE,
                                   codelistType = "index event"
) {

  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertTrue(inherits(cdm[[cohortTable]], "GeneratedCohortSet"))
  omopgenerics::assertTrue(all(c("cohort_definition_id", "subject_id", "cohort_start_date",
                                 "cohort_end_date") %in% colnames(cdm[[cohortTable]])))
  omopgenerics::assertChoice(codelistType,
                             choices = c("index event", "inclusion criteria", "exit criteria"))

  if(is.null(cohortId)){
    cohortId <- sort(CDMConnector::settings(cdm[[cohortTable]]) |>
                       dplyr::pull("cohort_definition_id"))
  } else {
    cohortId <- omopgenerics::validateCohortIdArgument(cohortId = cohortId,
                                                       cohort = cdm[[cohortTable]])
  }

  checkCodelist(x, allowConceptSetExpression = FALSE)

  if(!is.null(x)){
    if(inherits(x, "codelist_with_details")){
      x <- asCodelist(x)
    }
    if(length(x) == 0){
      return(omopgenerics::emptySummarisedResult())
    }
    # if x is null we'll use cohort codelist attribute
    # otherwise will use codelist specified
    settings <- expand.grid(codelist_name = names(x),
                            cohort_definition_id = omopgenerics::settings(cdm[[cohortTable]]) |>
                              dplyr::pull("cohort_definition_id")) |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::left_join(omopgenerics::settings(cdm[[cohortTable]]) |>
                         dplyr::select("cohort_definition_id", "cohort_name"),
                       by = "cohort_definition_id") |>
      dplyr::arrange(.data$cohort_name) |>
      dplyr::mutate(codelist_name = as.character(.data$codelist_name))
  } else {
    x <- attr(cdm[[cohortTable]], "cohort_codelist") |>
      dplyr::filter(.data$codelist_type %in% !!codelistType) |>
      dplyr::select("codelist_name", "concept_id") |>
      dplyr::distinct()

    settings <- attr(cdm[[cohortTable]], "cohort_codelist") |>
      dplyr::filter(.data$codelist_type %in% !!codelistType) |>
      dplyr::select("cohort_definition_id", "codelist_name") |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::left_join(omopgenerics::settings(cdm[[cohortTable]]) |>
                         dplyr::select("cohort_definition_id", "cohort_name"),
                       by = "cohort_definition_id") |>
      dplyr::arrange(.data$cohort_name) |>
      dplyr::mutate(codelist_name = as.character(.data$codelist_name))
  }


  cohortCodeUse <- list()
  for(i in seq_along(settings$codelist_name)){
    workingCohortName <- settings$cohort_name[[i]]
    workingCohortId <- settings$cohort_definition_id[[i]]
    workingCodelistName <- settings$codelist_name[[i]]

    if(inherits(x, "cdm_table")){
      workingCodelist <- x |>
        dplyr::filter(.data$codelist_name == !!workingCodelistName) |>
        dplyr::select("concept_id")
      workingCodelist <- list(workingCodelist) |>
        stats::setNames(workingCodelistName)
    } else {
      workingCodelist <- x[workingCodelistName]
    }

    cli::cli_inform(" Getting counts of {names(workingCodelist)} codes for cohort {workingCohortName}")
    cohortCodeUse[[i]] <- getCodeUse(workingCodelist,
                                     cdm = cdm,
                                     cohortTable = cohortTable,
                                     cohortId = workingCohortId,
                                     timing = timing,
                                     countBy = countBy,
                                     byConcept = byConcept,
                                     byYear = byYear,
                                     bySex = bySex,
                                     ageGroup = ageGroup,
                                     dateRange = as.Date(c(NA,NA)),
                                     useSourceCodes = useSourceCodes)
  }
  cohortCodeUse <- dplyr::bind_rows(cohortCodeUse) |>
    dplyr::arrange(dplyr::across(!c("variable_level", "estimate_value")))

  if (nrow(cohortCodeUse) > 0) {
    cohortCodeUse <- cohortCodeUse |>
      dplyr::mutate(
        result_id = 1L,
        cdm_name = omopgenerics::cdmName(cdm)
      ) |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(

          result_id = 1L,
          result_type = "cohort_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion("CodelistGenerator")),
          timing = timing
        )
      )
  } else {
    cohortCodeUse <- omopgenerics::emptySummarisedResult()
  }

  return(cohortCodeUse)
}
