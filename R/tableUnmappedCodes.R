#' Format the result of summariseUnmappedCodeUse into a table
#'
#' @param result A `<summarised_result>` with results of the type
#' "umapped_codes".
#' @inheritParams typeTableDoc
#' @inheritParams headerDoc
#' @inheritParams groupColumnDoc
#' @inheritParams hideDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseUnmappedCodes
#' result.
#'
#' @export
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' codes <- list("Musculoskeletal disorder" = 1)
#' cdm <- omopgenerics::insertTable(cdm, "condition_occurrence",
#' dplyr::tibble(person_id = 1,
#'               condition_occurrence_id = 1,
#'               condition_concept_id = 0,
#'               condition_start_date  = as.Date("2000-01-01"),
#'               condition_type_concept_id  = NA,
#'               condition_source_concept_id = 7))
#' unmapped_codes <- summariseUnmappedCodes(x = list("osteoarthritis" = 2),
#' cdm = cdm, table = "condition_occurrence")
#' tableUnmappedCodes(unmapped_codes)
#'
#'cdm <- omopgenerics::insertTable(
#'  cdm,
#'  "measurement",
#'  dplyr::tibble(
#'    person_id = 1,
#'    measurement_id = 1,
#'    measurement_concept_id = 0,
#'    measurement_date  = as.Date("2000-01-01"),
#'    measurement_type_concept_id  = NA,
#'    measurement_source_concept_id = 7
#'  )
#')
#' table <- summariseUnmappedCodes(x = list("cs" = 2),
#'                                cdm = cdm,
#'                                table = c("measurement"))
#' tableUnmappedCodes(unmapped_codes)
#'
#' CDMConnector::cdmDisconnect(cdm)
#' }
#'
tableUnmappedCodes <- function(result,
                               type = "gt",
                               header = c("cdm_name", "estimate_name"),
                               groupColumn = character(),
                               hide = character(),
                               .options = list()) {

  rlang::check_installed("visOmopResults", version = "1.0.0")

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "unmapped_codes")

  if(nrow(result) == 0){
    cli::cli_warn("No unmapped codes results found in result object")
    return(emptyResultTable(type = type))
  }

  x <- internalTableUnmappedCodes(
    result = result,
    resultType = "unmapped_codes",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    .options = .options
  )

  return(x)
}


internalTableUnmappedCodes <- function(result,
                                        type,
                                        resultType,
                                        header,
                                        groupColumn,
                                        hide,
                                        .options) {
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(hide, null = TRUE)
  if (!is.list(groupColumn) & !is.null(groupColumn)) groupColumn <- list(groupColumn)
  omopgenerics::assertCharacter(groupColumn[[1]], null = TRUE)

  # filter result + nice estimate name
  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  header <- reformulateTableUnmappedCodes(header)
  groupColumn[[1]] <- reformulateTableUnmappedCodes(groupColumn[[1]])
  hide <- reformulateTableUnmappedCodes(hide)

  # visOmopTable
  x <- visOmopResults::visOmopTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumn,
    type = type,
    rename = c(
      "Domain ID" = "domain_id", "Vocabulary ID" = "vocabulary_id",
      "Database name" = "cdm_name", "Unmapped concept ID" = "variable_level",
      "Unmapped concept name" = "variable_name"
    ),
    hide = hide,
    .options = .options
  ) |>
    suppressWarnings() # to remove after next release of visOmopResults

  return(x)
}

emptyResultTable <- function(type){
  dplyr::tibble()
}

reformulateTableUnmappedCodes <- function(vect) {
  if (length(vect) > 0) {
    purrr::map(vect, function(x) {
      if (x == "standard_concept_id") {
        "variable_level"
      } else if (x == "standard_concept_name") {
        "variable_name"
      } else {
        x
      }
    }) |> unlist()
  } else {
    vect
  }
}
