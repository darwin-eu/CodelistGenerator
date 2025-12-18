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
#'
tableCodeUse <- function(result,
                         type = "gt",
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
                               type = "gt",
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

   # .options
  .options <- optionsCodeUse(.options)

  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

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
