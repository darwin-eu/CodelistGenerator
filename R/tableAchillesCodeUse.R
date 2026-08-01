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

#' Format the result of summariseAchillesCodeUse into a table
#'
#' @param result A `<summarised_result>` with results of the type
#' "achilles_code_use".
#' @inheritParams typeTableDoc
#' @inheritParams tableStyleDoc
#' @inheritParams headerDoc
#' @inheritParams groupColumnDoc
#' @inheritParams hideDoc
#' @inheritParams tableStyleDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseCohortCodeUse
#' result.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#'
#' cdm <- mockVocabRef("database")
#' oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#' result_achilles <- summariseAchillesCodeUse(newCodelist(list(oa = oa$concept_id)),
#'                                             cdm = cdm)
#' tableAchillesCodeUse(result_achilles)
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableAchillesCodeUse <- function(result,
                                 type = NULL,
                                 header = c("cdm_name", "estimate_name"),
                                 groupColumn = character(),
                                 hide = character(),
                                 style = NULL,
                                 .options = list()) {

  rlang::check_installed("visOmopResults", version = "1.4.0")

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(visOmopResults::emptyTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "achilles_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No achilles code use results found in result object")
    return(visOmopResults::emptyTable(type = type))
  }

  x <- internalTableAchillesResult(
    result = result,
    resultType = "achilles_code_use",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    style = style,
    .options = .options
  )

  return(x)
}

