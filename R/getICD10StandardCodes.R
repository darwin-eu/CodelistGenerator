#' Get corresponding standard codes for ICD-10 chapters and sub-chapters
#'
#' @param cdm cdm_reference via CDMConnector
#' @param level Can be either "ICD10 Chapter" or "ICD10 SubChapter"
#' @param name Name of chapter or sub-chapter of interest. If NULL, all
#' will be considered.
#' @param includeDescendants If FALSE only direct mappings from ICD-10 codes
#' to standard codes will be returned. If TRUE descendants of standard concepts
#' will also be included.
#' @param withConceptDetails If FALSE a vector of concept IDs will be returned
#' for each ICD group If TRUE a tibble will be returned with additional
#' information on the identified concepts.
#'
#' @return A named list, with each element containing the corresponding
#' standard codes (and descendants) of ICD chapters and sub-chapters
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef()
#' getICD10StandardCodes(cdm = cdm, level = c(
#'   "ICD10 Chapter",
#'   "ICD10 SubChapter"
#' ))
#' CDMConnector::cdmDisconnect(cdm)
#' }
getICD10StandardCodes <- function(cdm,
                                  level = c(
                                    "ICD10 Chapter",
                                    "ICD10 SubChapter"
                                  ),
                                  name = NULL,
                                  includeDescendants = TRUE,
                                  withConceptDetails = FALSE) {
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
    level = level,
    name = name
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
      copy = TRUE,
      relationship = "many-to-many"
    ) %>%
    dplyr::select("concept_id_2", "name") %>%
    dplyr::rename("concept_id" = "concept_id_2")

  ICD10MapsTo <- cdm$concept %>%
    dplyr::select("concept_id") %>%
    dplyr::inner_join(ICD10NonStandardCodes,
      by = "concept_id",
      copy = TRUE
    ) %>%
    dplyr::distinct()
  if(!is.null(attr(cdm, "dbcon"))){
    ICD10MapsTo <- ICD10MapsTo %>%
      CDMConnector::compute_query()
  }

  # add descendants
  if (isTRUE(includeDescendants)) {
    ICD10MapsTo <- ICD10MapsTo %>%
      dplyr::left_join(cdm$concept_ancestor,
        by = c("concept_id" = "ancestor_concept_id")
      ) %>%
      dplyr::select("name", "descendant_concept_id") %>%
      dplyr::rename("concept_id" = "descendant_concept_id")

    if(!is.null(attr(cdm, "dbcon"))){
      ICD10MapsTo <- ICD10MapsTo  %>%
      CDMConnector::compute_query()
      }
  }

  if(isTRUE(withConceptDetails)) {
    ICD10MapsTo <- ICD10MapsTo %>%
      dplyr::left_join(cdm[["concept"]] %>%
                         dplyr::select("concept_id", "concept_name",
                                         "domain_id", "vocabulary_id"),
                       by = "concept_id")
    # split into list
    ICD10StandardCodes <- ICD10MapsTo %>%
      dplyr::collect()
    ICD10StandardCodes <- split(
      x = ICD10StandardCodes,
      f = as.factor(ICD10StandardCodes$name),
      drop = TRUE
    )
  } else {
    # split into list (only returning vector of concept ids)
    ICD10StandardCodes <- ICD10MapsTo %>%
      dplyr::collect()
    ICD10StandardCodes <- split(
      x = ICD10StandardCodes$concept_id,
      f = ICD10StandardCodes$name
    )
  }


  if(isFALSE(withConceptDetails)){
    ICD10StandardCodes <- omopgenerics::newCodelist(ICD10StandardCodes)
  }

  return(ICD10StandardCodes)
}


# get non-standard descendant codes for icd chapters and sub-chapters
getICD10NonStandardCodes <- function(cdm,
                                     level = c(
                                       "ICD10 Chapter",
                                       "ICD10 SubChapter"
                                     ),
                                     name) {


  if(!is.null(name)){
    conceptDb <- cdm[["concept"]] %>%
      dplyr::filter(tolower(.data$concept_name) == tolower(.env$name))
  } else {
    conceptDb <- cdm[["concept"]]
  }

  if ("ICD10 SubChapter" %in% level) {
    # go down two levels to get specific codes
    icd_sub <- conceptDb %>%
      dplyr::filter(.data$vocabulary_id == "ICD10") %>%
      dplyr::filter(.data$concept_class_id %in% "ICD10 SubChapter") %>%
      dplyr::select("concept_id", "concept_name", "concept_code")
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub <- icd_sub %>%
      CDMConnector::computeQuery()
      }

    icd_sub1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub %>%
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub1 <- icd_sub1  %>%
      CDMConnector::computeQuery()
      }
    # one more level down
    icd_sub2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_sub1
    )

    if(!is.null(attr(cdm, "dbcon"))){
      icd_sub2 <-  icd_sub2 %>%
      CDMConnector::computeQuery()}

    icd_subchapter <- icd_sub2 %>%
      dplyr::collect() %>%
      dplyr::mutate(name = stringr::str_to_lower(.data$concept_name)) %>%
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) %>%
      dplyr::select("concept_id_1", "name") %>%
      dplyr::distinct()
  } else {
    icd_subchapter <- dplyr::tibble()
  }

  if ("ICD10 Chapter" %in% level) {
    # go down three levels to get specific codes
    icd_ch <- conceptDb %>%
      dplyr::filter(.data$vocabulary_id == "ICD10") %>%
      dplyr::filter(.data$concept_class_id %in% "ICD10 Chapter") %>%
      dplyr::select("concept_id", "concept_name", "concept_code")
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch <-icd_ch  %>%
      CDMConnector::computeQuery()
      }

    icd_ch1 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch %>%
        dplyr::rename(
          "concept_id_1" =
            "concept_id"
        )
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch1 <- icd_ch1 %>%
      CDMConnector::computeQuery()
    }
    # one more level down
    icd_ch2 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch1
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch2 <- icd_ch2 %>%
      CDMConnector::computeQuery()
    }
    # and one more level down
    icd_ch3 <- get_subsumed_concepts(
      cdm = cdm,
      concepts = icd_ch2
    )
    if(!is.null(attr(cdm, "dbcon"))){
      icd_ch3 <-icd_ch3 %>%
      CDMConnector::computeQuery()}

    icd_chapter <- icd_ch3 %>%
      dplyr::collect() %>%
      dplyr::mutate(name = stringr::str_to_lower(.data$concept_name)) %>%
      dplyr::mutate(name = stringr::str_replace_all(.data$name, " ", "_")) %>%
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
