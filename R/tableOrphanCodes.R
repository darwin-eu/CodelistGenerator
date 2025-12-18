#' Format the result of summariseOrphanCodes into a table
#'
#' @param result A `<summarised_result>` with results of the type
#' "orphan_codes".
#' @inheritParams typeTableDoc
#' @inheritParams tableStyleDoc
#' @inheritParams headerDoc
#' @inheritParams groupColumnDoc
#' @inheritParams hideDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseOrphanCodes
#' result.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#' keywords = "Musculoskeletal disorder",
#' domains = "Condition",
#' includeDescendants = FALSE)
#'
#' orphan_codes <- summariseOrphanCodes(x = newCodelist(list("msk" = codes$concept_id)),
#' cdm = cdm)
#'
#' tableOrphanCodes(orphan_codes)
#'
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableOrphanCodes <- function(result,
                             type = "gt",
                             header = c("cdm_name", "estimate_name"),
                             groupColumn = character(),
                             hide = character(),
                             style = NULL,
                             .options = list()) {

  rlang::check_installed("visOmopResults", version = "1.4.0")

  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(visOmopResults::emptyTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "orphan_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No orphan code results found in result object")
    return(visOmopResults::emptyTable(type = type))
  }

  x <- internalTableAchillesResult(
    result = result,
    resultType = "orphan_code_use",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    style = style,
    .options = .options
  )

  return(x)
}

internalTableAchillesResult <- function(result,
                                        type,
                                        resultType,
                                        header,
                                        groupColumn,
                                        hide,
                                        style,
                                        .options) {
  omopgenerics::assertCharacter(header, null = TRUE)
  omopgenerics::assertCharacter(hide, null = TRUE)
  if (!is.list(groupColumn) & !is.null(groupColumn)) groupColumn <- list(groupColumn)
  omopgenerics::assertCharacter(groupColumn[[1]], null = TRUE)

  # filter result + nice estimate name
  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  header <- reformulateTableAchilles(header)
  groupColumn[[1]] <- reformulateTableAchilles(groupColumn[[1]])
  hide <- reformulateTableAchilles(hide)

  # visOmopTable
  x <- visOmopResults::visOmopTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumn,
    type = type,
    rename = c(
      "Domain ID" = "domain_id", "Vocabulary ID" = "vocabulary_id",
      "Database name" = "cdm_name", "Standard concept ID" = "variable_level",
      "Standard concept name" = "variable_name", "Source concept ID" = "source_concept_id"
    ),
    hide = hide,
    style = style,
    .options = .options
  ) |>
    suppressWarnings() # to remove after next release of visOmopResults

  return(x)
}

reformulateTableAchilles <- function(vect) {
  vect <- as.character(vect)
  vect[vect == "standard_concept_id"] <- "variable_level"
  vect[vect == "standard_concept_name"] <- "variable_name"
  vect
}
