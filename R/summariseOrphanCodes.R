#' Find orphan codes related to a codelist
#'
#' @param x A codelist for which to find related codes used in the database
#' @param cdm cdm_reference via CDMConnector
#'
#' @return A summarised result containg the frequency of codes related
#' to (but not in) the codelist
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
#' cdm = cdm)
#'
#' orphan_codes
#' CDMConnector::cdmDisconnect(cdm)
#' }
summariseOrphanCodes <- function(x,
                                  cdm){

  if(isFALSE(inherits(cdm, "cdm_reference"))){
    cli::cli_abort("cdm is not a cdm reference but is {class(cdm)}")
  }
  if(isFALSE(inherits(x, "codelist")) && isFALSE(is.list(x))){
    cli::cli_abort("x is not a codelist but is {class(cdm)}")
  }

  x <- omopgenerics::newCodelist(x)

  # will only return codes that are used in the database
  # which we get from achilles tables
  codesUsed <- fetchAchillesCodesInUse(cdm = cdm,
                                       minimumCount = 0,
                                       collect = FALSE)
  descendantsUsed <- cdm$concept_ancestor |>
    dplyr::inner_join(codesUsed,
                      by = c("descendant_concept_id"="concept_id"))
  ancestorsUsed <- cdm$concept_ancestor |>
    dplyr::inner_join(codesUsed,
                      by = c("ancestor_concept_id"="concept_id"))
  relationshipUsed1 <- cdm$concept_relationship |>
    dplyr::inner_join(codesUsed,
                      by = c("concept_id_1"="concept_id"))
  relationshipUsed2 <- cdm$concept_relationship |>
    dplyr::inner_join(codesUsed,
                      by = c("concept_id_2"="concept_id"))

  if("concept_recommended" %in% names(cdm)){
    phoebe <- TRUE
    phoebeUsed <- cdm$concept_recommended |>
      dplyr::inner_join(codesUsed,
                        by = c("concept_id_2"="concept_id"))
  } else {
    phoebe <- FALSE
    cli::cli_inform(c("PHOEBE results not available",
                      "i" = "The concept_recommened table is not present in the cdm."))
  }

  orphanCodes <- list()
  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  #
  for(i in seq_along(x)){
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    # get descendants used in db
    orphanDescendants <- cdm[[tableCodelist]] |>
      dplyr::inner_join(descendantsUsed,
                       by = c("concept_id" = "ancestor_concept_id")) |>
      dplyr::select("concept_id" = "descendant_concept_id") |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get ancestors used in db
    orphanAncestors <- cdm[[tableCodelist]] |>
      dplyr::left_join(ancestorsUsed,
                       by = c("concept_id" = "descendant_concept_id")) |>
      dplyr::select("concept_id" = "ancestor_concept_id")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get relationship 1
    orphanRelationship1 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_2")) |>
      dplyr::select("concept_id" = "concept_id_1")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    # get relationship 2
    orphanRelationship2 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_1")) |>
      dplyr::select("concept_id" = "concept_id_2")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::pull("concept_id")

    orphanCodes[[names(x)[i]]] <- c(orphanDescendants,
                                    orphanAncestors,
                                    orphanRelationship1,
                                    orphanRelationship2)


    if(isTRUE(phoebe)){
      phoebeCodes <-  cdm[[tableCodelist]] |>
        dplyr::left_join(phoebeUsed,
                         by = c("concept_id" = "concept_id_1")) |>
        dplyr::select("concept_id" = "concept_id_2")  |>
        dplyr::filter(!is.na(.data$concept_id)) |>
        dplyr::distinct() |>
        dplyr::pull("concept_id")

      orphanCodes[[names(x)[i]]] <- c(orphanCodes[[names(x)[i]]],
                                      phoebeCodes)
    }

    # make sure we don't have any of the original codes
    orphanCodes[[names(x)[i]]] <- setdiff(orphanCodes[[names(x)[i]]], x[[i]])
  }

  orphanCodes <- orphanCodes |> vctrs::list_drop_empty()
  if(length(orphanCodes) == 0){
  orphanCodes <- omopgenerics::emptySummarisedResult() |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = as.integer(1),
          result_type = "orphan_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion(
            pkg = "CodelistGenerator"))
        )
      )
  } else {
    orphanCodes <- summariseAchillesCodeUse(orphanCodes, cdm)
    attr(orphanCodes, "settings")$result_type <- "orphan_code_use"
  }

  orphanCodes

}


# summariseOrphanCodes <- function(x,
#                                  cdm,
#                                  domains = "Condition",
#                                  standardConcept = "Standard",
#                                  searchInSynonyms = TRUE,
#                                  searchNonStandard = TRUE,
#                                  includeDescendants = TRUE,
#                                  includeAncestor = TRUE,
#                                  minCellCount = lifecycle::deprecated()) {
#
#   if (lifecycle::is_present(minCellCount)) {
#     lifecycle::deprecate_warn("2.3.0", "summariseOrphanCodes()", with = "omopgenerics::suppress()")
#   }
#
#   errorMessage <- checkmate::makeAssertCollection()
#   checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
#   checkmate::assertVector(domains, add = errorMessage)
#   checkmate::assertVector(standardConcept, add = errorMessage)
#   standardConceptCheck <- all(tolower(standardConcept) %in%
#                                 c(
#                                   "standard",
#                                   "classification",
#                                   "non-standard"
#                                 ))
#   if (!isTRUE(standardConceptCheck)) {
#     errorMessage$push(
#       "- standardConcept must be from Standard, Non-standard, or Classification"
#     )
#   }
#   checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
#   checkmate::assert_logical(searchInSynonyms, add = errorMessage)
#   checkmate::assert_logical(searchNonStandard, add = errorMessage)
#   checkmate::assert_logical(includeDescendants, add = errorMessage)
#   checkmate::assert_logical(includeAncestor, add = errorMessage)
#   checkmate::reportAssertions(collection = errorMessage)
#
#   checkmate::assertList(x)
#   if(length(names(x)) != length(x)){
#     cli::cli_abort("Must be a named list")
#   }
#
#
#   x <- addDetails(cdm = cdm, conceptList = x)
#
#   orphanConcepts <- list()
#   # rerun search
#   for (i in seq_along(x)) {
#     cli::cli_inform("Searching for orphan codes for {names(x)[i]}")
#
#     suppressMessages(
#       candidateCodes <- getCandidateCodes(
#         cdm = cdm,
#         keywords = x[[i]]$concept_name,
#         domains = domains,
#         standardConcept = standardConcept,
#         searchInSynonyms = searchInSynonyms,
#         searchNonStandard = searchNonStandard,
#         includeDescendants = includeDescendants,
#         includeAncestor = includeAncestor))
#
#     # Exclude codes that are in the original set of codes
#     candidateCodes <- candidateCodes %>%
#       dplyr::anti_join(
#         x[[i]] %>% dplyr::select("concept_id"),
#         by = "concept_id"
#       )
#
#     # Use achilles counts to summarise code use
#     if ("achilles_results" %in% names(cdm)) {
#       cli::cli_inform("Using achilles results to restict to codes that appear in the cdm reference")
#       orphanConcepts[[i]] <- summariseAchillesCodeUse(
#         x = list("cs" = candidateCodes$concept_id),
#         cdm = cdm
#       )
#       if (nrow(orphanConcepts[[i]]) > 0) {
#         # transform to orphan codes result format
#         orphanConcepts[[i]] <- orphanConcepts[[i]] %>%
#           dplyr::left_join(
#             candidateCodes %>%
#               dplyr::mutate("variable_level" = as.character(.data$concept_id)) %>%
#               dplyr::select("variable_level", "found_from"),
#             by = "variable_level"
#           ) %>%
#           dplyr::mutate(
#             additional_name = paste0(.data$additional_name, " &&& relationship_id"),
#             additional_level = paste0(.data$additional_level, " &&& ", .data$found_from)
#           ) |>
#           dplyr::select(!"found_from")
#       }
#     } else {
#       cli::cli_inform("Achilles tables not found in cdm reference - querying cdm directly for code counts")
#       orphanConcepts[[i]] <- summariseCodeUse(
#         x = list("cs" = candidateCodes$concept_id),
#         cdm = cdm,
#         countBy = "record"
#       )
#       if (nrow(orphanConcepts[[i]]) > 0) {
#         # transform to orphan codes result format
#         orphanConcepts[[i]] <- orphanConcepts[[i]] %>%
#           dplyr::filter(.data$variable_name != "overall") %>%
#           visOmopResults::splitAdditional() %>%
#           dplyr::select(!c("strata_name", "strata_level", "source_concept_name", "source_concept_id")) %>%
#           dplyr::left_join(
#             candidateCodes %>%
#               dplyr::mutate("variable_level" = as.character(.data$concept_id)) %>%
#               dplyr::select(
#                 "variable_level", "vocabulary_id", "standard_concept", "relationship_id" = "found_from"
#               ) %>%
#               dplyr::mutate(
#                 standard_concept = dplyr::case_when(
#                   standard_concept == "S" ~ "standard",
#                   standard_concept == "C" ~ "classification",
#                   is.na(standard_concept) ~ "non-standard"
#                 )
#               ),
#             by = "variable_level"
#           ) %>%
#           visOmopResults::uniteAdditional(cols = c("standard_concept", "vocabulary_id", "relationship_id")) %>%
#           visOmopResults::uniteStrata(cols = "domain_id")
#       }
#     }
#
#     if (nrow(orphanConcepts[[i]]) == 0) {
#       cli::cli_inform("-- No orphan codes found for codelist {names(x)[i]}")
#       orphanConcepts[[i]] <- omopgenerics::emptySummarisedResult()
#     }
#   }
#
#   orphanConcepts <- dplyr::bind_rows(orphanConcepts) |>
#     dplyr::as_tibble()
#   attr(orphanConcepts, "settings") <- NULL
#
#   orphanConcepts <- orphanConcepts %>%
#     dplyr::mutate(result_id = 1L) %>%
#     omopgenerics::newSummarisedResult(
#       settings = dplyr::tibble(
#         result_id = 1L,
#         result_type = "orphan_codes",
#         package_name = "CodelistGenerator",
#         package_version = as.character(utils::packageVersion(
#           pkg = "CodelistGenerator")),
#         search_domains = paste0(domains, collapse = " &&& "), # "search" added not to mistake with code domain in summarised result
#         search_standard_concept = paste0(standardConcept, collapse = " &&& "), # "search" added not to mistake with strata_concept in summarised result
#         search_in_synonyms = searchInSynonyms,
#         search_non_standard = searchNonStandard,
#         include_descendants = includeDescendants,
#         include_ancestor = includeAncestor
#       )
#     )
#
#   return(orphanConcepts)
# }
#
