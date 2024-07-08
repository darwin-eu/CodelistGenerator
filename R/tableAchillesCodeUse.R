#' Format the result of summariseAchillesCodeUse into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised result with results of the type
#' "achilles_code_use".
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param conceptId If TRUE concept ids will be displayed.
#' @param standard If TRUE a column indicating if the code is standard will be
#' displayed.
#' @param vocabulary If TRUE vocabulary id will be displayed.
#' @param groupColumns Columns to use as group labels. Allowed columns are
#' `cdm_name` and/or `codelist_name`.
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
#' cdm <- mockVocabRef("database")
#' oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#' result_achilles <- summariseAchillesCodeUse(list(oa = oa$concept_id), cdm = cdm)
#' tableAchillesCodeUse(result_achilles)
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableAchillesCodeUse <- function(result,
                                 type = "gt",
                                 header = c("cdm_name", "estimate"),
                                 conceptId = TRUE,
                                 standard = TRUE,
                                 vocabulary = TRUE,
                                 groupColumns = NULL,
                                 excludeColumns = c("result_id", "estimate_type"),
                                 .options = list()) {

  if(nrow(result) == 0){
    cli::cli_warn("result object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "achilles_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No achilles code use results found in result object")
    return(emptyResultTable(type = type))
  }

  x <- internalTableAchillesResult(
    result = result,
    resultType = "achilles_code_use",
    type = type,
    header = header,
    conceptId = conceptId,
    standard = standard,
    vocabulary =  vocabulary,
    relationship = FALSE,
    groupColumns = groupColumns,
    settings = character(),
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(x)
}

#' Format the result of summariseOrphanCodes into a visual table.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param result A summarised result with results of the type
#' "orphan_codes".
#' @param type Type of desired formatted table, possibilities: "gt",
#' "flextable", "tibble".
#' @param header A vector containing which elements should go into the header
#' in order. Allowed are: `cdm_name`, `group`, `strata`, `additional`,
#' `variable`, `estimate`, `settings`.
#' @param conceptId If TRUE concept ids will be displayed.
#' @param standard If TRUE a column indicating if the code is standard will be
#' displayed.
#' @param vocabulary If TRUE vocabulary id will be displayed.
#' @param relationship If TRUE relationship id will be displayed.
#' @param settings Vector with the settings columns to display.
#' @param groupColumns Columns to use as group labels. Allowed columns are
#' `cdm_name` and/or `codelist_name`.
#' @param excludeColumns Columns to drop from the output table.
#' @param .options Named list with additional formatting options.
#' visOmopResults::optionsVisOmopTable() shows allowed arguments and
#' their default values.
#'
#' @return A table with a formatted version of the summariseOrphanCodes
#' result.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#' keywords = "Musculoskeletal disorder",
#' domains = "Condition",
#' includeDescendants = FALSE)
#'
#' orphan_codes <- summariseOrphanCodes(x = list("msk" = codes$concept_id),
#' cdm = cdm,
#' domains = "Condition",
#' standardConcept = "Standard",
#' searchInSynonyms = FALSE,
#' searchNonStandard = FALSE,
#' includeDescendants = TRUE,
#' includeAncestor = FALSE)
#'
#' tableOrphanCodes(orphan_codes)
#'
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableOrphanCodes <- function(result,
                             type = "gt",
                             header = c("cdm_name", "estimate"),
                             conceptId = TRUE,
                             standard = TRUE,
                             vocabulary = TRUE,
                             groupColumns = NULL,
                             excludeColumns = c("result_id", "estimate_type"),
                             .options = list()) {

  if(nrow(result) == 0){
    cli::cli_warn("Result object is empty")
    return(emptyResultTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "orphan_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No orphan code results found in result object")
    return(emptyResultTable(type = type))
  }

  x <- internalTableAchillesResult(
    result = result,
    resultType = "orphan_code_use",
    type = type,
    header = header,
    conceptId = conceptId,
    standard = standard,
    vocabulary =  vocabulary,
    relationship = FALSE,
    groupColumns = groupColumns,
    settings = character(),
    excludeColumns = excludeColumns,
    .options = .options
  )


}

internalTableAchillesResult <- function(result,
                                        type,
                                        resultType,
                                        header,
                                        conceptId,
                                        standard,
                                        vocabulary,
                                        relationship,
                                        groupColumns,
                                        settings,
                                        excludeColumns,
                                        .options) {
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
  checkmate::assertLogical(conceptId, len = 1, any.missing = FALSE)
  checkmate::assertLogical(standard, len = 1, any.missing = FALSE)
  checkmate::assertLogical(vocabulary, len = 1, any.missing = FALSE)
  if (standard & vocabulary & any(grepl("additional", excludeColumns))) {
    cli::cli_abort(
      c("!" = "Incompatible input arguments.",
        "i" = "`additional` columns cannot be excluded while both `standard` and `vocabulary` are TRUE.")
    )
  }

  # filter result + nice estimate name
  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  # additional:
    condition <- !standard & !vocabulary

  if (any(grepl("additional", excludeColumns)) & !condition) {
    # remove additonal from exclude columns
    excludeColumns <- excludeColumns[!grepl("additional", excludeColumns)]
  } else if (!any(grepl("additional", excludeColumns)) & condition) {
    # add additional to exclude
    excludeColumns <- c(excludeColumns, "additional_name", "additional_level")
  }

  if (!condition) {
    toInclude <- c("standard_concept", "vocabulary_id", "relationship_id")[c(standard, vocabulary, relationship)]
    if (length(settings) > 0) {
      toInclude <- c(toInclude, settings)
    }
    x <- x |>
      dplyr::left_join(
        omopgenerics::settings(x),
        by = "result_id"
      ) |>
      visOmopResults::splitAdditional() |>
      visOmopResults::uniteAdditional(cols = toInclude) |>
      dplyr::select(omopgenerics::resultColumns())
  }

  # concept name and id
  if (!conceptId) {
    renameColumns = c("Standard concept name" = "variable_name")
    excludeColumns <- c(excludeColumns, "variable_level")
  } else {
    x <- x
    renameColumns = c("Standard concept name" = "variable_name",
                      "Standard concept id" = "variable_level")
  }

  # split:
  split <- c("group", "strata", "additional")
  if (any(grepl("group", excludeColumns))) {
    split <- split[!split %in% "group"]
  }
  if (any(grepl("strata", excludeColumns))) {
    split <- split[!split %in% "strata"]
  }
  if (any(grepl("additional", excludeColumns))) {
    split <- split[!split %in% "additional"]
  }

  # visOmopTable
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

emptyResultTable <- function(type){

  empty_result <- dplyr::tibble()

}
