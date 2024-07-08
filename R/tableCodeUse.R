#' Format the result of summariseCodeUse into a table.
#'
#' @param result A summarised result with results of the type "code_use".
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param splitStrata If TRUE strata will be split.
#' @param conceptId If TRUE concept ids will be displayed.
#' @param sourceConcept If TRUE source concepts will be displayed.
#' @param groupColumns Columns to use as group labels. Allowed columns are
#' `cdm_name` and/or `codelist_name`.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' visOmopResults::optionsVisOmopTable() shows allowed arguments and
#' their default values.
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
tableCodeUse <- function(result,
                         type = "gt",
                         header = c("cdm_name", "estimate"),
                         splitStrata = TRUE,
                         conceptId = TRUE,
                         sourceConcept = TRUE,
                         groupColumns = NULL,
                         excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                         .options = list()) {

  if(nrow(result) == 0){
    cli::cli_warn("Result object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No code use results found in result object")
    return(emptyResultTable(type = type))
  }


  # checks
  if (inherits(groupColumns, "list")) {
    checkmate::assertList(groupColumns, len = 1)
    groupCheck <- groupColumns[[1]]
  } else if (inherits(groupColumns, "character")) {
    groupCheck <- groupColumns
  } else {
    groupCheck <- NULL
  }
  if (!is.null(groupCheck)) {
    idsErr <- !groupCheck %in% c("cdm_name", "codelist_name")
    if (sum(idsErr) > 0) {
      cli::cli_abort("Allowed group columns are `cdm_name` and/or `codelist_name`.")
    }
  }

  x <- internalTableCodeUse(
    result = result,
    resultType = "code_use",
    type = type,
    header = header,
    splitStrata = splitStrata,
    groupColumns = groupColumns,
    conceptId = conceptId,
    sourceConcept = sourceConcept,
    timing = FALSE,
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(x)
}

#' Format the result of summariseCohortCodeUse into a table.
#'
#' @param result A summarised result with results of the type "cohort_code_use".
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param splitStrata If TRUE strata will be split.
#' @param conceptId If TRUE concept ids will be displayed.
#' @param sourceConcept If TRUE source concepts will be displayed.
#' @param timing If TRUE the timing setting will be displayed.
#' @param groupColumns Columns to use as group labels. Allowed columns are
#' `cdm_name`, `cohort_name` and/or `codelist_name`.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' visOmopResults::optionsVisOmopTable() shows allowed arguments and
#' their default values.
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
                               header = c("cdm_name", "estimate"),
                               splitStrata = TRUE,
                               conceptId = TRUE,
                               sourceConcept = TRUE,
                               timing = FALSE,
                               groupColumns = NULL,
                               excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                               .options = list()) {

  # checks
  if (inherits(groupColumns, "list")) {
    checkmate::assertList(groupColumns, len = 1)
    groupCheck <- groupColumns[[1]]
  } else if (inherits(groupColumns, "character")) {
    groupCheck <- groupColumns
  } else {
    groupCheck <- NULL
  }
  if (!is.null(groupCheck)) {
    idsErr <- !groupCheck %in% c("cdm_name", "cohort_name", "codelist_name")
    if (sum(idsErr) > 0) {
      cli::cli_abort("Allowed group columns are `cdm_name`, `cohort_name`, and/or `codelist_name`.")
    }
  }
  checkmate::assertLogical(timing, len = 1, any.missing = FALSE)

  x <- internalTableCodeUse(
    result = result,
    resultType = "cohort_code_use",
    type = type,
    header = header,
    splitStrata = splitStrata,
    groupColumns = groupColumns,
    conceptId = conceptId,
    sourceConcept = sourceConcept,
    timing = timing,
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(x)
}

internalTableCodeUse <- function(result,
                                 resultType,
                                 type,
                                 header,
                                 splitStrata,
                                 groupColumns,
                                 conceptId,
                                 sourceConcept,
                                 timing,
                                 excludeColumns,
                                 .options) {
  # checks
  checkmate::assertLogical(splitStrata, len = 1, any.missing = FALSE)
  checkmate::assertList(.options)

  # .options
  .options <- optionsCodeUse(.options)

  x <- result |> visOmopResults::filterSettings(.data$result_type == .env$resultType)

  split <- c("group", "additional")
  if (splitStrata) {
    split <- c(split, "strata")
  }

  if (!conceptId) {
    x <- x |>
      dplyr::mutate(variable_level = NA_character_)
    if (sourceConcept) {
      x <- x |>
        dplyr::mutate(
          additional_name = gsub(" &&& source_concept_id", "", .data$additional_name),
          additional_level = sub("&.*&", "&&&", .data$additional_level)
        )
    }
    renameColumns = c("Standard concept name" = "variable_name")
    excludeColumns <- c(excludeColumns, "variable_level")
  } else {
    x <- x
    renameColumns = c("Standard concept name" = "variable_name",
                      "Standard concept id" = "variable_level")
  }

  if (sourceConcept & any(grepl("additional", excludeColumns))) {
    # remove domain from additional
    domains <- c("condition", "drug", "observation", "measurement", "visit", "procedure", "device")
    domains <- paste0(" &&& ", domains, collapse = "|")
    x <- x |>
      dplyr::mutate(
        additional_name = gsub(" &&& domain_id", "", .data$additional_name),
        additional_level = gsub(domains, "", .data$additional_level)
      )
    # remove additional of excluded columns
    excludeColumns <- excludeColumns[!grepl("additional", excludeColumns)]
    # add additional to split if not there
    if (!"additional" %in% split) {split <- c(split, "additional")}

  } else if (!sourceConcept & all(!grepl("additional", excludeColumns))) {
    # only keep domain from additional
    x <- x |>
      visOmopResults::splitAdditional() |>
      dplyr::select(- dplyr::starts_with("source")) |>
      visOmopResults::uniteAdditional(cols = "domain_id")
  } else if (!sourceConcept & any(grepl("additional", excludeColumns))) {
    split <- split[!split %in% "additional"]
  }

  # fix split
  if (any(grepl("group", excludeColumns))) {
    split <- split[!split %in% "group"]
  }

  x <- x |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  if (timing) {
    if (any(grepl("additional", excludeColumns))) {
      # additional = timing, and don't exclude = split
      x <- x |>
        visOmopResults::addSettings(columns = "timing") |>
        dplyr::mutate(
          "additional_name" = "timing",
          "additional_level" = .data$timing
          ) |>
        dplyr::select(!"timing")
      excludeColumns <- excludeColumns[!grepl("additional", excludeColumns)]
      split <- c(split, "additional")
    } else {
      x <- x |>
        visOmopResults::addSettings(columns = "timing") |>
        dplyr::mutate(
          additional_name = dplyr::if_else(
            .data$additional_name == "overall",
            "timing",
            paste0(.data$additional_name, " &&& timing")
          ),
          additional_level = dplyr::if_else(
            .data$additional_level == "overall",
            .data$timing,
            paste0(.data$additional_level, " &&& ", .data$timing)
          )
        ) |>
        dplyr::select(!"timing")
    }
  }

  x <- visOmopResults::visOmopTable(
    result = x,
    formatEstimateName = character(),
    header = header,
    split = split,
    groupColumn = groupColumns,
    type = type,
    renameColumns = renameColumns,
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(x)
}

optionsCodeUse <- function(userOptions) {

  defaultOpts <- visOmopResults::optionsVisOmopTable()

  idsIgnored <- ! names(userOptions) %in% names(defaultOpts)
  if (sum(idsIgnored) > 0) {
    cli::cli_warn("The following elements in `.options` do not refer to allowed arguments and will be ignored: {paste0(names(userOptions)[idsIgnored], collapse = ', ')}.")
  }

  for (opt in names(userOptions)) {
    defaultOpts[[opt]] <- userOptions[[opt]]
  }

  return(defaultOpts)
}

