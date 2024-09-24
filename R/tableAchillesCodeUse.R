#' Format the result of summariseAchillesCodeUse into a table.
#'
#' @param result A `<summarised_result>` with results of the type
#' "achilles_code_use".
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
#' visOmopResults::optionsVisOmopTable() shows allowed arguments and
#' their default values.
#' @param excludeColumns Deprecated.
#' @param conceptId Deprecated.
#' @param standard Deprecated.
#' @param vocabulary Deprecated.
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
                                 groupColumns = character(),
                                 hide = character(),
                                 .options = list(),
                                 conceptId = lifecycle::deprecated(),
                                 standard = lifecycle::deprecated(),
                                 vocabulary = lifecycle::deprecated(),
                                 excludeColumns = lifecycle::deprecated()) {
  # lifecyle deprecate warns
  if (lifecycle::is_present(conceptId)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableAchillesCodeUse(conceptId)",
      with = "tableAchillesCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(standard)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableAchillesCodeUse(standard)",
      with = "tableAchillesCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(vocabulary)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableAchillesCodeUse(vocabulary)",
      with = "tableAchillesCodeUse(hide)"
    )
  }
  if (lifecycle::is_present(excludeColumns)) {
    lifecycle::deprecate_soft(
      when = "2.2.4",  what = "tableAchillesCodeUse(excludeColumns)",
      with = "tableAchillesCodeUse(hide)"
    )
  }

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
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
    # conceptId = conceptId,
    # standard = standard,
    # vocabulary =  vocabulary,
    # relationship = FALSE,
    groupColumns = groupColumns,
    # settings = character(),
    hide = hide,
    .options = .options
  )

  return(x)
}

#' Format the result of summariseOrphanCodes into a table.
#'
#' @param result A summarised result with results of the type
#' "orphan_codes".
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
#' visOmopResults::optionsVisOmopTable() shows allowed arguments and
#' their default values.
#' @param excludeColumns Deprecated.
#' @param conceptId Deprecated.
#' @param standard Deprecated.
#' @param vocabulary Deprecated.
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
                             groupColumns = character(),
                             hide = character(),
                             .options = list(),
                             conceptId = lifecycle::deprecated(),
                             standard = lifecycle::deprecated(),
                             vocabulary = lifecycle::deprecated(),
                             excludeColumns = lifecycle::deprecated()) {
  # lifecyle deprecate warns
  if (lifecycle::is_present(conceptId)) {
    lifecycle::deprecate_soft(when = "2.2.4", what = "conceptId", with = "hide")
  }
  if (lifecycle::is_present(standard)) {
    lifecycle::deprecate_soft(when = "2.2.4", what = "standard", with = "hide")
  }
  if (lifecycle::is_present(vocabulary)) {
    lifecycle::deprecate_soft(when = "2.2.4", what = "vocabulary", with = "hide")
  }
  if (lifecycle::is_present(excludeColumns)) {
    lifecycle::deprecate_soft(when = "2.2.4", what = "excludeColumns", with = "hide")
  }

  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
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
    # conceptId = conceptId,
    # standard = standard,
    # vocabulary =  vocabulary,
    # relationship = FALSE,
    groupColumns = groupColumns,
    # settings = character(),
    hide = hide,
    .options = .options
  )

  return(x)
}

internalTableAchillesResult <- function(result,
                                        type,
                                        resultType,
                                        header,
                                        # conceptId,
                                        # standard,
                                        # vocabulary,
                                        # relationship,
                                        groupColumns,
                                        # settings,
                                        hide,
                                        .options) {

  # filter result + nice estimate name
  x <- result |>
    visOmopResults::filterSettings(.data$result_type == .env$resultType) |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name))) |>
    visOmopResults::splitAll() |>
    dplyr::rename(
      "standard_concept_name" = "variable_name",
      "standard_concept_id" = "variable_level"
    )

  # hide
  hide <- c(hide, "result_id", "estimate_type")

  # visTable
  x <- visOmopResults::visTable(
    result = x,
    estimateName = character(),
    header = header,
    groupColumn = groupColumns,
    type = type,
    rename = c(
      "Domain ID" = "domain_id", "Vocabulary ID" = "vocabulary_id",
      "Database name" = "cdm_name", "Standard concept ID" = "standard_concept_id"
    ),
    hide = hide,
    .options = .options
  )

  return(x)
}

emptyResultTable <- function(type){
  dplyr::tibble()
}
