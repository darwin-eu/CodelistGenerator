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

#' Format the result of summariseCohortCodeUse into a table.
#'
#' @param result A `<summarised_result>` with results of the type "cohort_code_use".
#' @inheritParams typeTableDoc
#' @inheritParams tableStyleDoc
#' @inheritParams headerStrataDoc
#' @inheritParams groupColumnStrataDoc
#' @inheritParams hideStrataDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseCohortCodeUse
#' result.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomiaDir())
#' cdm <- CDMConnector::cdmFromCon(con,
#'                                   cdmSchema = "main",
#'                                   writeSchema = "main")
#' cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
#' conceptSet = list(a = 260139,
#'                   b = 1127433),
#'                   name = "cohorts",
#'                   end = "observation_period_end_date",
#'                   overwrite = TRUE)
#'
#'results_cohort_mult <-
#'summariseCohortCodeUse(list(cs = c(260139,19133873)),
#'                       cdm = cdm,
#'                       cohortTable = "cohorts",
#'                       timing = "entry")
#'
#'tableCohortCodeUse(results_cohort_mult)
#'CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableCohortCodeUse <- function(result,
                               type = NULL,
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = character(),
                               hide = c("timing"),
                               .options = list(),
                               style = NULL) {

  rlang::check_installed("visOmopResults", version = "1.4.0")

  # checks
  result <- omopgenerics::validateResultArgument(result)

  # empty result object
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(visOmopResults::emptyTable(type = type))
  }

  # no cohort_code_use
  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "cohort_code_use")
  if(nrow(result) == 0){
    cli::cli_warn("No code use results found in result object")
    return(visOmopResults::emptyTable(type = type))
  }

  x <- internalTableCodeUse(
    result = result,
    resultType = "cohort_code_use",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    style = style,
    .options = .options
  )

  return(x)
}
