#' Find orphan codes related to a codelist
#'
#' @param x Codes for which to find codes related but not included (orphan
#' codes)
#' @param cdm cdm_reference via CDMConnector
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#' will be included in the candidate codelist.
#' @param minCellCount ```r lifecycle::badge("deprecated")```
#'
#' @return A codelist containing code related to (but not in) the target
#' codelist that are present used in the cdm
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
#' orphan_codes
#' CDMConnector::cdmDisconnect(cdm)
#' }

summariseOrphanCodes <- function(x,
                            cdm,
                            domains = "Condition",
                            standardConcept = "Standard",
                            searchInSynonyms = TRUE,
                            searchNonStandard = TRUE,
                            includeDescendants = TRUE,
                            includeAncestor = TRUE,
                            minCellCount = lifecycle::deprecated()) {

  if (lifecycle::is_present(minCellCount)) {
    lifecycle::deprecate_warn("2.3.0", "summariseOrphanCodes()", with = "omopgenerics::suppress()")
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertVector(domains, add = errorMessage)
  checkmate::assertVector(standardConcept, add = errorMessage)
  standardConceptCheck <- all(tolower(standardConcept) %in%
                                c(
                                  "standard",
                                  "classification",
                                  "non-standard"
                                ))
  if (!isTRUE(standardConceptCheck)) {
    errorMessage$push(
      "- standardConcept must be from Standard, Non-standard, or Classification"
    )
  }
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
  checkmate::assert_logical(searchInSynonyms, add = errorMessage)
  checkmate::assert_logical(searchNonStandard, add = errorMessage)
  checkmate::assert_logical(includeDescendants, add = errorMessage)
  checkmate::assert_logical(includeAncestor, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  checkmate::assertList(x)
  if(length(names(x)) != length(x)){
    cli::cli_abort("Must be a named list")
  }


  x <- addDetails(cdm = cdm, conceptList = x)

  orphanConcepts <- list()
  # rerun search
  for (i in seq_along(x)) {
    cli::cli_inform("Searching for orphan codes for {names(x)[i]}")

    suppressMessages(
      candidateCodes <- getCandidateCodes(
        cdm = cdm,
        keywords = x[[i]]$concept_name,
        domains = domains,
        standardConcept = standardConcept,
        searchInSynonyms = searchInSynonyms,
        searchNonStandard = searchNonStandard,
        includeDescendants = includeDescendants,
        includeAncestor = includeAncestor))

    # Exclude codes that are in the original set of codes
    candidateCodes <- candidateCodes %>%
      dplyr::anti_join(x[[i]] %>%
                         dplyr::select("concept_id"),
                       by = "concept_id")

    # Use achilles counts to summarise code use
    if ("achilles_results" %in% names(cdm)) {
      cli::cli_inform("Using achilles results to restict to codes that appear in the cdm reference")
      orphanConcepts[[i]] <- summariseAchillesCodeUse(
        x = list("cs" = candidateCodes$concept_id),
        cdm = cdm
      ) %>%
        dplyr::left_join(
          candidateCodes %>%
            dplyr::mutate("variable_level" = as.character(.data$concept_id)) %>%
            dplyr::select("variable_level", "found_from"),
          by = "variable_level"
        ) %>%
        dplyr::mutate(
          additional_name = paste0(.data$additional_name, " &&& relationship_id"),
          additional_level = paste0(.data$additional_level, " &&& ", .data$found_from)
        ) |>
        dplyr::select(!"found_from")
    } else {
      cli::cli_inform("Achilles tables not found in cdm reference - querying cdm directly for code counts")
      orphanConcepts[[i]] <- summariseCodeUse(
        x = list("cs" = candidateCodes$concept_id),
        cdm = cdm,
        countBy = "record"
      )
      if (nrow(orphanConcepts[[i]]) > 0) {
        # transform to achilles result format
        orphanConcepts[[i]] <- orphanConcepts[[i]] %>%
          dplyr::filter(.data$variable_name != "overall") %>%
          visOmopResults::splitAdditional() %>%
          dplyr::select(!c("strata_name", "strata_level", "source_concept_name", "source_concept_id")) %>%
          dplyr::left_join(
            candidateCodes %>%
              dplyr::mutate("variable_level" = as.character(.data$concept_id)) %>%
              dplyr::select(
                "variable_level", "vocabulary_id", "standard_concept", "relationship_id" = "found_from"
              ) %>%
              dplyr::mutate(
                standard_concept = dplyr::case_when(
                  standard_concept == "S" ~ "standard",
                  standard_concept == "C" ~ "classification",
                  is.na(standard_concept) ~ "non-standard"
                )
              ),
            by = "variable_level"
          ) %>%
          visOmopResults::uniteAdditional(cols = c("standard_concept", "vocabulary_id", "relationship_id")) %>%
          visOmopResults::uniteStrata(cols = "domain_id")
      }
    }

    if (nrow(orphanConcepts[[i]]) == 0) {
      cli::cli_inform("-- No orphan codes found for codelist {names(x)[i]}")
      orphanConcepts[[i]] <- omopgenerics::emptySummarisedResult()
    }
  }

  orphanConcepts <- dplyr::bind_rows(orphanConcepts) |>
    dplyr::as_tibble()
  attr(orphanConcepts, "settings") <- NULL

  orphanConcepts <- orphanConcepts %>%
    dplyr::mutate(result_id = 1L) %>%
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = 1L,
        result_type = "orphan_codes",
        package_name = "CodelistGenerator",
        package_version = as.character(utils::packageVersion(
          pkg = "CodelistGenerator")),
        search_domains = paste0(domains, collapse = " &&& "), # "search" added not to mistake with code domain in summarised result
        search_standard_concept = paste0(standardConcept, collapse = " &&& "), # "search" added not to mistake with strata_concept in summarised result
        search_in_synonyms = searchInSynonyms,
        search_non_standard = searchNonStandard,
        include_descendants = includeDescendants,
        include_ancestor = includeAncestor
      )
    )

  return(orphanConcepts)
}

#' Find orphan codes related to a codelist
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param x Codes for which to find codes related but not included (orphan
#' codes)
#' @param cdm cdm_reference via CDMConnector
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#' will be included in the candidate codelist.
#' @param minCellCount ```r lifecycle::badge("deprecated")```
#'
#' @return A codelist containing code related to (but not in) the target
#' codelist that are present used in the cdm
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
#' orphan_codes <- findOrphanCodes(x = list("msk" = codes$concept_id),
#' cdm = cdm,
#' domains = "Condition",
#' standardConcept = "Standard",
#' searchInSynonyms = FALSE,
#' searchNonStandard = FALSE,
#' includeDescendants = TRUE,
#' includeAncestor = FALSE)
#'
#' orphan_codes
#' CDMConnector::cdmDisconnect(cdm)
#' }

findOrphanCodes <- function(x,
                            cdm,
                            domains = "Condition",
                            standardConcept = "Standard",
                            searchInSynonyms = TRUE,
                            searchNonStandard = TRUE,
                            includeDescendants = TRUE,
                            includeAncestor = TRUE,
                            minCellCount = lifecycle::deprecated()) {

  lifecycle::deprecate_soft(
    when = "2.4.0",
    what = "findOrphanCodes()",
    with = "summariseOrphanCodes()"
  )

  x <- findOrphanCodes(x = x,
                       cdm = cdm,
                       domains = domains,
                       standardConcept = standardConcept,
                       searchInSynonyms = searchInSynonyms,
                       searchNonStandard = searchNonStandard,
                       includeDescendants = includeDescendants,
                       includeAncestor = includeAncestor,
                       minCellCount = lifecycle::deprecated())
  return(x)
}
