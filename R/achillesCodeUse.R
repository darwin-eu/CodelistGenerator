
#' Summarise code use from achilles counts
#'
#' @param x Codelist
#' @param cdm cdm_reference via CDMConnector::cdm_from_con()
#' @param countBy Either "record" for record-level counts or "person" for
#' person-level counts
#' @param minCellCount The minimum number of counts to reported, below which
#' results will be suppressed.
#'
#' @return A tibble with results
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- mockVocabRef("database")
#' oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#' result_achilles <- achillesCodeUse(list(oa = oa$concept_id), cdm = cdm)
#' result_achilles
#' CDMConnector::cdmDisconnect(cdm)
#' }

achillesCodeUse <- function(x,
                            cdm,
                            countBy = c("record", "person"),
                            minCellCount = 5) {

  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertTRUE(all(countBy %in% c("record", "person")),
                        add = errorMessage)
  checkmate::assert_numeric(minCellCount, len = 1,
                            add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  checkmate::assertList(x)
  if(length(names(x)) != length(x)){
    cli::cli_abort("Must be a named list")
  }

  if(is.null(cdm[["achilles_results"]])){
    cli::cli_abort("No achilles tables found in cdm reference")
  }

  version <- achillesVersionDate(cdm)
  cli::cli_inform(version)

  allCodes <-  purrr::list_c(x)

  codesWithDetails <- addDetails(x, cdm)
  codesWithDetails <- purrr::list_rbind(codesWithDetails) %>%
    dplyr::distinct()

  codeUse <- list()

  if("record" %in% countBy){
  allRecordCount <- getAchillesRecordCounts(cdm = cdm, conceptId = allCodes)
  if(nrow(allRecordCount)>=1){
  allRecordCount <- allRecordCount %>%
    dplyr::mutate(concept_id = as.character(.data$concept_id)) %>%
    dplyr::left_join(codesWithDetails %>%
                       dplyr::mutate(concept_id = as.character(.data$concept_id)),
                     by = "concept_id")
    for(i in seq_along(x)){
    codeUse[[paste0(i, "_record")]] <- allRecordCount %>%
      dplyr::filter(.data$concept_id %in% x[[i]]) %>%
      dplyr::mutate(variable_name = "Record count",
                    group_level = .data$concept_id,
                    standard_concept_name = .data$concept_name,
                    standard_concept_id = .data$concept_id
                    ) %>%
      dplyr::mutate(codelist_name = names(x)[i])
    }
  }
  }

  if("person" %in% countBy){
  allPersonCount <- getAchillesPersonCounts(cdm = cdm, conceptId = allCodes)
  if(nrow(allPersonCount)>=1){
  allPersonCount <- allPersonCount %>%
    dplyr::mutate(concept_id = as.character(.data$concept_id)) %>%
    dplyr::left_join(codesWithDetails %>%
                       dplyr::mutate(concept_id = as.character(.data$concept_id)),
                     by = "concept_id")
  for(i in seq_along(x)){
    codeUse[[paste0(i, "_person")]] <- allPersonCount %>%
      dplyr::filter(.data$concept_id %in% x[[i]]) %>%
      dplyr::mutate(variable_name = "Person count",
                    group_level = .data$concept_id,
                    standard_concept_name = .data$concept_name,
                    standard_concept_id = .data$concept_id
      ) %>%
      dplyr::mutate(codelist_name = names(x)[i])
  }
  }
  }

  if(length(codeUse) == 0){
    cli::cli_inform(
      c(
        "i" = "No achilles counts found for the concepts provided."
      ))
    return(omopgenerics::emptySummarisedResult())
  } else {
  cols <- c("standard_concept_name", "standard_concept_id")

  codeUse <- dplyr::bind_rows(codeUse) %>%
    dplyr::mutate(cdm_name = CDMConnector::cdmName(cdm),
                  package_name = "CodelistGenerator",
                  package_version = as.character(utils::packageVersion(
                    pkg = "CodelistGenerator")),
           group_name = "by_concept",
           result_type = "achilles_code_use",
           strata_name = .data$codelist_name,
           strata_level  = "overall",
           variable_level  = "overall",
           estimate_name  = "count",
           estimate_type = "integer",
           estimate_value = as.character(.data$n)
           ) %>%
    dplyr::mutate(additional_name = paste0(cols, collapse = " ; ")) %>%
    tidyr::unite(
      col = "additional_level", dplyr::all_of(cols), sep = " ; ", remove = TRUE
    ) %>%
    dplyr::select(dplyr::any_of(omopgenerics::resultColumns("summarised_result")))



  }

  # currently omopgenerics doesn´t like "and" in concept name
  # codeUse  <- omopgenerics::newSummarisedResult(codeUse) %>%
  #   omopgenerics::suppress(minCellCount = minCellCount)

  codeUse <- codeUse %>%
    dplyr::mutate(estimate_value = dplyr::if_else(as.integer(.data$estimate_value) <= as.integer(.env$minCellCount) &
                                           as.character(.data$estimate_value) != 0,
                                           as.character(NA), as.character(.data$estimate_value)))


  return(codeUse)

}

achillesVersionDate <- function(cdm){
  if(is.null(cdm[["achilles_results"]])){
    cli::cli_abort("No achilles tables found in cdm reference")
  }

  cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id == 0) %>%
    dplyr::collect() %>%
    dplyr::mutate(achilles_version = paste0("Using achilles results from version ",
                                            .data$stratum_2,
                                            " which was run on ",
                                            .data$stratum_3)) %>%
    dplyr::pull("achilles_version")

}

getAchillesPersonCounts <- function(cdm, conceptId = NULL){
  fetchAchillesCounts(cdm = cdm,
                      analysisId = c(400, # condition occurrence
                                     700, # drug_exposure
                                     800, # observation
                                     1800, # measurement
                                     200, # visit_occurrence
                                     600, # procedure_occurrence
                                     2100  # device_exposure
                                     ),
                      conceptId = conceptId)



}

getAchillesRecordCounts <- function(cdm, conceptId = NULL){
  fetchAchillesCounts(cdm = cdm,
                      analysisId = c(401, # condition occurrence
                                     701, # drug_exposure
                                     801, # observation
                                     1801, # measurement
                                     201, # visit_occurrence
                                     601, # procedure_occurrence
                                     2101  # device_exposure
                      ),
                      conceptId = conceptId)

}

fetchAchillesCounts <- function(cdm, analysisId, conceptId = NULL){
 analyses <- cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id %in%  .env$analysisId) %>%
    dplyr::select("stratum_1", "count_value") %>%
    dplyr::rename("concept_id" = "stratum_1",
                  "n"="count_value") %>%
   dplyr::collect()

  if(!is.null(conceptId)){
    analyses <- analyses %>%
    dplyr::filter(.data$concept_id %in%  .env$conceptId)
    }

 analyses %>%
    dplyr::mutate(n = as.integer(.data$n))


}


