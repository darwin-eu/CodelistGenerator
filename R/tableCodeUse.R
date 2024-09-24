#' Format the result of summariseCodeUse into a table.
#'
#' @param result A summarised result with results of the type "code_use".
#' @param type Type of desired formatted table. To see supported formats
#' use visOmopResults::tableType()
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' Alternatively, it can include other names to use as overall header labels.
#' @param groupColumns Variables to use as group labels. Allowed columns are:
#' "cdm_name", "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' These cannot be used in header.
#' @param hide Table columns to exclude, options are: "cdm_name",
#' "codelist_name", "year", "sex", "age_group", "standard_concept_name",
#' "standard_concept_id", "estimate_name", "source_concept_name",
#' "source_concept_id", "domain_id". If results are stratified, "year", "sex",
#' "age_group" can also be used. These cannot be used in header or groupColumn.
#' @param .options Named list with additional formatting options.
#' visOmopResults::tableOptions() shows allowed arguments and
#' their default values.
#' @param excludeColumns Deprecated.
#' @param conceptId Deprecated.
#' @param splitStrata Deprecated.
#' @param sourceConcept Deprecated.
#'
#' @return A table with a formatted version of the summariseCodeUse result.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomia_dir())
#' cdm <- CDMConnector::cdm_from_con(con,
#'                                   cdm_schem = "main",
#'                                   write_schema = "main")
#'acetiminophen <- c(1125315,  1127433, 40229134,
#'40231925, 40162522, 19133768,  1127078)
#'poliovirus_vaccine <- c(40213160)
#'cs <- list(acetiminophen = acetiminophen,
#'           poliovirus_vaccine = poliovirus_vaccine)
#'results <- summariseCodeUse(cs,cdm = cdm)
#'tableCodeUse(results)
#'CDMConnector::cdmDisconnect(cdm)
#'}
#'
#'
tableCodeUse <- function(result,
                         type = "gt",
                         header = c("cdm_name", "estimate_name"),
                         groupColumns = character(),
                         hide = character(),
                         .options = list(),
                         splitStrata = lifecycle::deprecated(),
                         conceptId = lifecycle::deprecated(),
                         sourceConcept = lifecycle::deprecated(),
                         excludeColumns = lifecycle::deprecated()) {
  # lifecyle deprecate warns
  if (lifecycle::is_present(conceptId)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCodeUse(conceptId)",
      with = "tableCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(sourceConcept)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCodeUse(sourceConcept)",
      with = "tableCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(splitStrata)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCodeUse(splitStrata)"
    )
  }
  if (lifecycle::is_present(excludeColumns)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCodeUse(excludeColumns)",
      with = "tableCodeUse(hide)"
    )
  }

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No code use results found in result object")
    return(emptyResultTable(type = type))
  }

  x <- internalTableCodeUse(
    result = result,
    resultType = "code_use",
    type = type,
    header = header,
    groupColumns = groupColumns,
    timing = FALSE,
    hide = hide,
    .options = .options
  )

  return(x)
}

#' Format the result of summariseCohortCodeUse into a table.
#'
#' @param result A summarised result with results of the type "cohort_code_use".
#' @param type Type of desired formatted table. To see supported formats
#' use visOmopResults::tableType()
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' Alternatively, it can include other names to use as overall header labels.
#' @param groupColumns Variables to use as group labels. Allowed columns are:
#' "cdm_name", "codelist_name", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "source_concept_name", "source_concept_id", "domain_id". If
#' results are stratified, "year", "sex", "age_group" can also be used.
#' These cannot be used in header.
#' @param timing If TRUE the timing setting will be displayed.
#' @param hide Table columns to exclude, options are: "cdm_name",
#' "codelist_name", "year", "sex", "age_group", "standard_concept_name",
#' "standard_concept_id", "estimate_name", "source_concept_name",
#' "source_concept_id", "domain_id". If results are stratified, "year", "sex",
#' "age_group" can also be used. These cannot be used in header or groupColumn.
#' @param .options Named list with additional formatting options.
#' visOmopResults::tableOptions() shows allowed arguments and
#' their default values.
#' @param excludeColumns Deprecated.
#' @param conceptId Deprecated.
#' @param splitStrata Deprecated.
#' @param sourceConcept Deprecated.
#'
#' @return A table with a formatted version of the summariseCohortCodeUse
#' result.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomia_dir())
#' cdm <- CDMConnector::cdm_from_con(con,
#'                                   cdm_schem = "main",
#'                                   write_schema = "main")
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
                               groupColumns = NULL,
                               timing = FALSE,
                               hide = character(),
                               .options = list(),
                               excludeColumns = lifecycle::deprecated(),
                               splitStrata = lifecycle::deprecated(),
                               conceptId = lifecycle::deprecated(),
                               sourceConcept = lifecycle::deprecated()) {
  # lifecyle deprecate warns
  if (lifecycle::is_present(conceptId)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCohortCodeUse(conceptId)",
      with = "tableCohortCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(sourceConcept)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCohortCodeUse(sourceConcept)",
      with = "tableCohortCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(splitStrata)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCohortCodeUse(splitStrata)"
    )
  }
  if (lifecycle::is_present(excludeColumns)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableCohortCodeUse(excludeColumns)",
      with = "tableCohortCodeUse(hide)"
    )
  }

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "cohort_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No code use results found in result object")
    return(emptyResultTable(type = type))
  }
  omopgenerics::assertLogical(timing, length = 1)

  x <- internalTableCodeUse(
    result = result,
    resultType = "cohort_code_use",
    type = type,
    header = header,
    groupColumns = groupColumns,
    timing = timing,
    hide = hide,
    .options = .options
  )

  return(x)
}

internalTableCodeUse <- function(result,
                                 resultType,
                                 type,
                                 header,
                                 groupColumns,
                                 timing,
                                 hide,
                                 .options) {
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(hide, null = TRUE)
  if (!is.list(groupColumns) & !is.null(groupColumns)) groupColumns <- list(groupColumns)
  omopgenerics::assertCharacter(groupColumns[[1]], null = TRUE)

   # .options
  .options <- optionsCodeUse(.options)

  if (timing) {
    settingsColumns <- "timing"
  } else {
    settingsColumns <- character()
  }

  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  header <- reformulateTableAchilles(header)
  groupColumns[[1]] <- reformulateTableAchilles(groupColumns[[1]])
  hide <- reformulateTableAchilles(hide)

  # visTable
  x <- visOmopResults::visOmopTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumns,
    type = type,
    settingsColumns = settingsColumns,
    rename = c(
      "Domain ID" = "domain_id", "Database name" = "cdm_name",
      "Standard concept ID" = "standard_concept_id",
      "Source concept ID" = "source_concept_id",
      "Standard concept ID" = "variable_level",
      "Standard concept name" = "variable_name"
    ),
    hide = hide,
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
