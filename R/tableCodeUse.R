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

#' Format the result of summariseCodeUse into a table.
#'
#' @param result A `<summarised_result>` with results of the type "code_use".
#' @inheritParams typeTableDoc
#' @inheritParams tableStyleDoc
#' @inheritParams headerStrataDoc
#' @inheritParams groupColumnStrataDoc
#' @inheritParams hideStrataDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseCodeUse result.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(omopgenerics)
#' library(CodelistGenerator)
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomiaDir())
#' cdm <- CDMConnector::cdmFromCon(con,
#'                                 cdmSchema = "main",
#'                                 writeSchema = "main")
#'acetiminophen <- c(1125315,  1127433, 40229134,
#'40231925, 40162522, 19133768,  1127078)
#'poliovirus_vaccine <- c(40213160)
#'cs <- list(acetiminophen = acetiminophen,
#'           poliovirus_vaccine = poliovirus_vaccine)
#'results <- summariseCodeUse(newCodelist(cs),cdm = cdm)
#'tableCodeUse(results)
#'CDMConnector::cdmDisconnect(cdm)
#'}
#'

tableCodeUse <- function(result,
                         type = NULL,
                         header = c("cdm_name", "estimate_name"),
                         groupColumn = character(),
                         hide = c("date_range_start", "date_range_end"),
                         style = NULL,
                         .options = list()) {

  rlang::check_installed("visOmopResults", version = "1.4.0")

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(visOmopResults::emptyTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No code use results found in result object")
    return(visOmopResults::emptyTable(type = type))
  }

  x <- internalTableCodeUse(
    result = result,
    resultType = "code_use",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    style = style,
    .options = .options
  )

  return(x)
}


internalTableCodeUse <- function(result,
                                 resultType,
                                 type,
                                 header,
                                 groupColumn,
                                 hide,
                                 style,
                                 .options) {
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(hide, null = TRUE)
  if (!is.list(groupColumn) & !is.null(groupColumn)) groupColumn <- list(groupColumn)
  omopgenerics::assertCharacter(groupColumn[[1]], null = TRUE)
  omopgenerics::assertCharacter(style, null = TRUE, length = 1)
  omopgenerics::assertCharacter(type, null = TRUE, length = 1)

   # .options
  .options <- optionsCodeUse(.options)

  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  levels <- c("overall", unique(x$variable_name)[unique(x$variable_name) != "overall"])

  x <- x |>
    dplyr::mutate("variable_name" = factor(.data$variable_name, levels = levels)) |>
    dplyr::arrange(dplyr::across(dplyr::everything()))

  header <- reformulateTableAchilles(header)
  groupColumn[[1]] <- reformulateTableAchilles(groupColumn[[1]])
  hide <- reformulateTableAchilles(hide)

  # visTable
  x <- visOmopResults::visOmopTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumn,
    type = type,
    settingsColumn = omopgenerics::settingsColumns(x),
    rename = c(
      "Domain ID" = "domain_id", "Database name" = "cdm_name",
      "Standard concept ID" = "standard_concept_id",
      "Source concept ID" = "source_concept_id",
      "Standard concept ID" = "variable_level",
      "Standard concept name" = "variable_name"
    ),
    hide = hide,
    style = style,
    .options = .options
  ) |>
    suppressWarnings() # to remove in next visOmopResults release

  return(x)
}

optionsCodeUse <- function(userOptions) {

  defaultOpts <- visOmopResults::tableOptions()

  idsIgnored <- ! names(userOptions) %in% names(defaultOpts)
  if (sum(idsIgnored) > 0) {
    cli::cli_warn("The following elements in `.options` do not refer to allowed arguments and will be ignored: {paste0(names(userOptions)[idsIgnored], collapse = ', ')}.")
  }

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}
