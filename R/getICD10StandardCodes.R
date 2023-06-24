#' Get corresponding standard codes for ICD-10 chapters and sub-chapters
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level Can be either "ICD10 Chapter" or "ICD10 SubChapter"
#' @param includeDescendants If FALSE only direct mappings from ICD-10 codes
#' to standard codes will be returned. If TRUE descendants of standard concepts
#' will also be included.
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
                                  ),
                                  includeDescendants = TRUE) {
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  levelCheck <- all(level %in%
    c(
      "ICD10 Chapter",
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
      )
    )
    cli::cli_inform(
      c(
        "i" = "No ICD-10 chapter codes were found in the concept table.",
        "--  Were ICD-10 codes included when loading vocabularies into the database?"
      )
    )
    return(dplyr::tibble())
  }

  # map from non-standard to standard and add descendants
  for (i in seq_along(ICD10NonStandardCodes)) {
    ICD10NonStandardCodes[[i]] <- dplyr::tibble(
      concept_id = ICD10NonStandardCodes[[i]],
      name = names(ICD10NonStandardCodes)[i]
    )
  }
  ICD10NonStandardCodes <- dplyr::bind_rows(ICD10NonStandardCodes)

  # map to standard
  ICD10NonStandardCodes <- ICD10NonStandardCodes %>%
    dplyr::inner_join(
      cdm[["concept_relationship"]] %>%
        dplyr::filter(.data$relationship_id == "Maps to"),
      by = c("concept_id" = "concept_id_1"),
      copy = TRUE
    ) %>%
    dplyr::select("concept_id_2", "name") %>%
    dplyr::rename("concept_id" = "concept_id_2")

  ICD10MapsTo <- cdm$concept %>%
    dplyr::select("concept_id") %>%
    dplyr::inner_join(ICD10NonStandardCodes,
      by = "concept_id",
      copy = TRUE
    ) %>%
    dplyr::distinct() %>%
    CDMConnector::compute_query()
  # add descendants
  if (isTRUE(includeDescendants)) {
    ICD10MapsTo <- ICD10MapsTo %>%
      dplyr::left_join(cdm$concept_ancestor,
        by = c("concept_id" = "ancestor_concept_id")
      ) %>%
      dplyr::select("name", "descendant_concept_id") %>%
      dplyr::rename("concept_id" = "descendant_concept_id") %>%
      CDMConnector::compute_query()
  }

  # split into list
  ICD10StandardCodes <- ICD10MapsTo %>%
    dplyr::select("name", "concept_id") %>%
    dplyr::collect()

  ICD10StandardCodes <- split(
    x = ICD10StandardCodes$concept_id,
    f = ICD10StandardCodes$name
  )

  return(ICD10StandardCodes)
}


# get non-standard descendant codes for icd chapters and sub-chapters
getICD10NonStandardCodes <- function(cdm,
                                     level = c(
                                       "ICD10 Chapter",
                                       "ICD10 SubChapter"
                                     )) {
  if ("ICD10 SubChapter" %in% level) {
    # go down two levels to get specific codes
    icd_sub <- cdm[["concept"]] %>%
      dplyr::filter(.data$vocabulary_id == "ICD10") %>%
      dplyr::filter(.data$concept_class_id %in% "ICD10 SubChapter") %>%
      dplyr::select("concept_id", "concept_name", "concept_code") %>%
      CDMConnector::computeQuery()

    icd_sub1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub %>%
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    ) %>%
      CDMConnector::computeQuery()
    # one more level down
    icd_sub2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub1
    ) %>%
      CDMConnector::computeQuery()

    icd_subchapter <- icd_sub2 %>%
      dplyr::collect() %>%
      dplyr::mutate(name = paste0(
        .data$concept_name,
        " [", .data$concept_code, "]"
      )) %>%
      dplyr::select("concept_id_1", "name") %>%
      dplyr::distinct()
  } else {
    icd_subchapter <- dplyr::tibble()
  }

  if ("ICD10 Chapter" %in% level) {
    # go down three levels to get specific codes
    icd_ch <- cdm[["concept"]] %>%
      dplyr::filter(.data$vocabulary_id == "ICD10") %>%
      dplyr::filter(.data$concept_class_id %in% "ICD10 Chapter") %>%
      dplyr::select("concept_id", "concept_name", "concept_code") %>%
      CDMConnector::computeQuery()

    icd_ch1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch %>%
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    ) %>%
      CDMConnector::computeQuery()
    # one more level down
    icd_ch2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch1
    ) %>%
      CDMConnector::computeQuery()
    # and one more level down
    icd_ch3 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch2
    ) %>%
      CDMConnector::computeQuery()

    icd_chapter <- icd_ch3 %>%
      dplyr::collect() %>%
      dplyr::mutate(name = paste0(
        .data$concept_name,
        " [", .data$concept_code, "]"
      )) %>%
      dplyr::select("concept_id_1", "name") %>%
      dplyr::distinct()
  } else {
    icd_chapter <- dplyr::tibble()
  }



  icd <- dplyr::bind_rows(icd_chapter, icd_subchapter)
  ICDNonStandardCodes <- split(
    x = icd$concept_id_1,
    f = icd$name
  )


  return(ICDNonStandardCodes)
}

# get the subsumed concepts
# and return switching from concept_id_2 to concept_id_1 (so that we can
# run again to get next set of subsumed)
get_subsumed_concepts <- function(cdm,
                                  concepts) {
  concepts %>%
    dplyr::inner_join(cdm[["concept_relationship"]],
      by = "concept_id_1"
    ) %>%
    dplyr::filter(.data$relationship_id == "Subsumes") %>%
    dplyr::select("concept_id_2", "concept_name", "concept_code") %>%
    dplyr::rename("concept_id_1" = "concept_id_2")
}
