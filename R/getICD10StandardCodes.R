#' Get corresponding standard codes for ICD-10 chapters and sub-chapters
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level Can be either "ICD10 Chapter" or "ICD10 SubChapter"
#'
#' @return A named list, with each element containing the corresponding
#' standard codes (and descendants) of ICD chapters and sub-chapters
#' @export
#'
#' @examples
#' cdm <- mockVocabRef()
#' getICD10StandardCodes(cdm = cdm, level = c(
#'   "ICD10 Chapter",
#'   "ICD10 SubChapter"
#' ))
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getICD10StandardCodes <- function(cdm,
                                  level = c(
                                    "ICD10 Chapter",
                                    "ICD10 SubChapter"
                                  )) {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  levelCheck <- all(level %in%
                      c("ICD10 Chapter",
                        "ICD10 SubChapter"
                      ))
  if (!isTRUE(levelCheck)) {
    errorMessage$push(
      "- level can only be from: ICD10 Chapter, ICD10 SubChapter "
    )
  }
  checkmate::assertTRUE(levelCheck, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # first get non standard codes
  ICD10NonStandardCodes <- getICD10NonStandardCodes(
    cdm = cdm,
    level = level
  )

  if (!length(ICD10NonStandardCodes) > 0) {
    cli::cli_h2(
      c(
      "i" = "No codes found"
    ))
    cli::cli_inform(
      c(
        "i" = "No ICD-10 chapter codes were found in the concept table.",
         "--  Were ICD-10 codes included when loading vocabularies into the database?"
      ))
    return(dplyr::tibble())
  }

  # for each, map from non-standard to standard and add descendants
  ICD10StandardCodes <- vector("list", length(ICD10NonStandardCodes))
  cli::cli_progress_bar(
    total = length(ICD10NonStandardCodes),
    format = " -- getting standard codes {cli::pb_bar} {cli::pb_current} of {cli::pb_total} ICD10 categories"
  )
  for (i in seq_along(ICD10NonStandardCodes)) {
    cli::cli_progress_update()
    workingCode <- dplyr::tibble("concept_id_1" = ICD10NonStandardCodes[[i]])
    workingName <- names(ICD10NonStandardCodes)[i]

    workingMappings <- cdm[["concept_relationship"]] %>%
      dplyr::filter(.data$relationship_id == "Maps to") %>%
      dplyr::inner_join(workingCode,
        by = "concept_id_1",
        copy = TRUE
      ) %>%
      dplyr::select("concept_id_2") %>%
      dplyr::rename("ancestor_concept_id" = "concept_id_2") %>%
      dplyr::distinct()

    ICD10StandardCodes[[i]] <- cdm$concept_ancestor %>%
      dplyr::inner_join(workingMappings,
        by = "ancestor_concept_id"
      ) %>%
      dplyr::select("descendant_concept_id") %>%
      dplyr::collect() %>%
      dplyr::distinct() %>%
      dplyr::pull()

    names(ICD10StandardCodes)[i] <- workingName
  }

  return(ICD10StandardCodes)
}


# get non-standard descendant codes for icd chapters and sub-chapters
getICD10NonStandardCodes <- function(cdm,
                                     level = c(
                                       "ICD10 Chapter",
                                       "ICD10 SubChapter"
                                     )) {
  ICDChapters <- cdm[["concept"]] %>%
    dplyr::filter(.data$vocabulary_id == "ICD10") %>%
    dplyr::filter(.data$concept_class_id %in% .env$level) %>%
    dplyr::collect()

  ICDNonStandardCodes <- vector("list", length(ICDChapters$concept_id))
  cli::cli_progress_bar(
    total = length(ICDNonStandardCodes),
    format = " -- getting non-standard codes {cli::pb_bar} {cli::pb_current} of {cli::pb_total} ICD10 categories"
  )
  for (i in seq_along(ICDChapters$concept_id)) {
    cli::cli_progress_update()
    workingCode <- ICDChapters$concept_id[[i]]
    workingName <- paste0(
      ICDChapters$concept_name[[i]],
      " [", ICDChapters$concept_code[[i]], "]"
    )
    isChapter <- ICDChapters$concept_class_id[[i]] == "ICD10 Chapter"

    descendantCodes <- getICDDescendants(
      cdm = cdm,
      codes = workingCode
    )

    if (isChapter) {
      # we need to get the descendants of the sub-chapters
      descendantCodes <- getICDDescendants(cdm = cdm, codes = descendantCodes)
    }

    ICDNonStandardCodes[[i]] <- descendantCodes
    names(ICDNonStandardCodes)[i] <- workingName
  }

  return(ICDNonStandardCodes)
}

# get the non-standard descendants for ICD chapters and sub-chapters
getICDDescendants <- function(cdm, codes) {
  descendantCodes <- cdm[["concept_relationship"]] %>%
    dplyr::filter(.data$concept_id_1 %in% .env$codes &
                  .data$relationship_id == "Subsumes") %>%
    dplyr::select("concept_id_2") %>%
    dplyr::rename("concept_id" = "concept_id_2") %>%
    dplyr::left_join(cdm$concept,
      by = "concept_id"
    ) %>%
    dplyr::pull("concept_id")
  return(descendantCodes)
}
