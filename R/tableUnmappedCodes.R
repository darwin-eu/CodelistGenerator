#' Format the result of summariseUnmappedCodeUse into a table.
#'
#' @param result A `<summarised_result>` with results of the type
#' "umapped_codes".
#' @param type Type of desired formatted table. To see supported formats
#' use visOmopResults::tableType()
#' @param header A vector specifying the elements to include in the header. The
#' order of elements matters, with the first being the topmost header.
#' The header vector can contain one of the following variables: "cdm_name",
#' "codelist_name", "domain_id", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "standard_concept", "vocabulary_id".
#' Alternatively, it can include other names to use as overall header labels.
#' @param groupColumns Variables to use as group labels. Allowed columns are:
#' "cdm_name", "codelist_name", "domain_id", "standard_concept_name",
#' "standard_concept_id", "estimate_name", "standard_concept", "vocabulary_id".
#' These cannot be used in header.
#' @param hide Table columns to exclude, options are:  "cdm_name",
#' "codelist_name", "domain_id", "standard_concept_name", "standard_concept_id",
#' "estimate_name", "standard_concept", "vocabulary_id". These cannot be used in
#' header or groupColumn.
#' @param .options Named list with additional formatting options.
#' visOmopResults::tableOptions() shows allowed arguments and
#' their default values.
#'
#' @return A table with a formatted version of the summariseUnmappedCodes
#' result.
#'
#' @export
#'
#'
tableUnmappedCodes <- function(result,
                               type = "gt",
                               header = c("cdm_name", "estimate_name"),
                               groupColumns = character(),
                               hide = character(),
                               .options = list()) {
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
    groupColumns = groupColumns,
    hide = hide,
    .options = .options
  )

  return(x)
}


internalTableUnmappedCodes <- function(result,
                                        type,
                                        resultType,
                                        header,
                                        groupColumns,
                                        hide,
                                        .options) {
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(hide, null = TRUE)
  if (!is.list(groupColumns) & !is.null(groupColumns)) groupColumns <- list(groupColumns)
  omopgenerics::assertCharacter(groupColumns[[1]], null = TRUE)

  # filter result + nice estimate name
  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  header <- reformulateTableUnmappedCodes(header)
  groupColumns[[1]] <- reformulateTableUnmappedCodes(groupColumns[[1]])
  hide <- reformulateTableUnmappedCodes(hide)

  # visOmopTable
  x <- visOmopResults::visOmopTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumns,
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
