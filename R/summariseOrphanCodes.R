#' Find orphan codes related to a codelist using achilles counts and, if
#' available, PHOEBE concept recommendations
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams domainDoc
#'
#' @return A summarised result containg the frequency of codes related
#' to (but not in) the codelist.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#'
#' cdm <- mockVocabRef("database")
#' codes <- getCandidateCodes(cdm = cdm,
#'                           keywords = "Musculoskeletal disorder",
#'                           domains = "Condition",
#'                           includeDescendants = FALSE)
#' codelist <- omopgenerics::newCodelist(list("msk" = codes$concept_id))
#' orphan_codes <- summariseOrphanCodes(x = codelist,
#'                                       cdm = cdm)
#'
#' orphan_codes
#' CDMConnector::cdmDisconnect(cdm)
#' }
summariseOrphanCodes <- function(x,
                                 cdm,
                                 domain = c("condition",
                                            "device",
                                            "drug",
                                            "measurement",
                                            "observation",
                                            "procedure",
                                            "visit")){
  omopgenerics::validateCdmArgument(cdm,
                                    requiredTables = c("achilles_analysis",
                                                       "achilles_results",
                                                       "achilles_results_dist"))
  checkCodelist(x, allowConceptSetExpression = FALSE)
  domain <- assertDomain(domains = domain, cdm = cdm)

  if(inherits(x, "codelist_with_details")){
    x <- asCodelist(x)
  }

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

  orphanCodes <- list()
  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())

  for(i in seq_along(x)){
    cli::cli_inform("Getting orphan codes for {names(x)[i]}")
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble("concept_id" = x[[i]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    # get descendants used in db
    orphanDescendants <- cdm[[tableCodelist]] |>
      dplyr::inner_join(descendantsUsed,
                        by = c("concept_id" = "ancestor_concept_id")) |>
      dplyr::select("concept_id" = "descendant_concept_id") |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::mutate("relationship" = "Descendant") |>
      dplyr::collect()

    # get direct ancestors used in db
    orphanAncestors <- cdm[[tableCodelist]] |>
      dplyr::left_join(ancestorsUsed,
                       by = c("concept_id" = "descendant_concept_id")) |>
      dplyr::filter(.data$min_levels_of_separation == 1) |>
      dplyr::select("concept_id" = "ancestor_concept_id")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::mutate("relationship" = "Ancestor") |>
      dplyr::collect()

    # get relationship 1
    orphanRelationship1 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_2")) |>
      dplyr::select("concept_id" = "concept_id_1",
                    "relationship" = "relationship_id")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::collect()

    # get relationship 2
    orphanRelationship2 <- cdm[[tableCodelist]] |>
      dplyr::left_join(relationshipUsed1,
                       by = c("concept_id" = "concept_id_1")) |>
      dplyr::select("concept_id" = "concept_id_2",
                    "relationship" = "relationship_id")  |>
      dplyr::filter(!is.na(.data$concept_id)) |>
      dplyr::distinct() |>
      dplyr::collect()

    orphanCodes[[names(x)[i]]] <- orphanDescendants |>
      dplyr::bind_rows(orphanAncestors) |>
      dplyr::bind_rows(orphanRelationship1) |>
      dplyr::bind_rows(orphanRelationship2)

    # make sure we don't have any of the original codes
    orphanCodes[[names(x)[i]]] <- orphanCodes[[names(x)[i]]] |>
      dplyr::filter(! .data$concept_id %in% x[[i]])

    # Merge rows of same concept_id but multiple relationships
    orphanCodes[[names(x)[i]]] <- orphanCodes[[names(x)[i]]] |>
      dplyr::group_by(.data$concept_id) |>
      dplyr::summarise("relationship" = paste(.data$relationship, collapse = ", "), .groups = "drop")
  }

  orphanCodes <- orphanCodes |> vctrs::list_drop_empty()

  if(length(orphanCodes) == 0){
    orphanCodesSummarisedResult <- omopgenerics::emptySummarisedResult() |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = 1L,
          result_type = "orphan_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion(
            pkg = "CodelistGenerator"))
        )
      )
  } else {
    # Convert to a list
    orphanCodesList <- purrr::map(orphanCodes, ~. |> dplyr::pull("concept_id"))

    # Subset on domain
    orphanCodesList <- orphanCodesList |>
      omopgenerics::newCodelist() |>
      subsetOnDomain(cdm = cdm, domain = domain)

    orphanCodesSummarisedResult <- summariseAchillesCodeUse(orphanCodesList, cdm = cdm)

    # Add relationship
    if(nrow(orphanCodesSummarisedResult) > 0){
    orphanCodesSummarisedResult <- orphanCodesSummarisedResult |>
      omopgenerics::splitAdditional() |>
      dplyr::left_join(
        dplyr::bind_rows(orphanCodes) |>
          dplyr::rename("variable_level" = "concept_id") |>
          dplyr::mutate("variable_level" = as.character(.data$variable_level)),
        by = "variable_level"
      ) |>
      omopgenerics::uniteAdditional(cols = c(omopgenerics::additionalColumns(orphanCodesSummarisedResult),
                                    "relationship"))
    }

    attr(orphanCodesSummarisedResult, "settings")$result_type <- "orphan_code_use"
  }

  return(orphanCodesSummarisedResult)

}

fetchAchillesCodesInUse <- function(cdm, minimumCount = 0L, collect = TRUE){

  minimumCount <- as.integer(minimumCount)
  codes <- cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id %in%
                    c(
                      401L, # condition occurrence
                      701L, # drug_exposure
                      801L, # observation
                      1801L, # measurement
                      201L, # visit_occurrence
                      601L, # procedure_occurrence
                      2101L # device_exposure
                    ),
                  .data$count_value >= .env$minimumCount) |>
    dplyr::select("concept_id" = "stratum_1") |>
    dplyr::mutate(concept_id = as.integer(.data$concept_id)) |>
    dplyr::distinct()

  if(isTRUE(collect)){
    codes <- codes |>
      dplyr::pull("concept_id")
  }

  codes

}

fetchAchillesSourceCodesInUse <- function(cdm, minimumCount = 0L){

  minimumCount <- as.integer(minimumCount)

  cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id %in%
                    c(
                      425L, # condition occurrence
                      725L, # drug_exposure
                      825L, # observation
                      1825L, # measurement
                      225L, # visit_occurrence
                      625L, # procedure_occurrence
                      2125L # device_exposure
                    )) |>
    dplyr::filter(.data$count_value >= .env$minimumCount) |>
    dplyr::select("stratum_1") |>
    dplyr::distinct() |>
    dplyr::mutate(stratum_1 = as.integer(.data$stratum_1)) |>
    dplyr::pull("stratum_1")
}

