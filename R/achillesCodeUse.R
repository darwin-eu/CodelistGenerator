
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
  allRecordCount <- allRecordCount %>%
    dplyr::mutate(concept_id = as.character(.data$concept_id)) %>%
    dplyr::left_join(codesWithDetails %>%
                       dplyr::mutate(concept_id = as.character(.data$concept_id)),
                     by = "concept_id")
    for(i in seq_along(x)){
    codeUse[[paste0(i, "_record")]] <- allRecordCount %>%
      dplyr::filter(.data$concept_id %in% x[[i]]) %>%
      dplyr::mutate(variable_name = "Record count",
                    group_level = paste0(.data$standard_concept,
                                         " concept: ",
                                         .data$concept_name, " (",
                                         .data$concept_id, ")"
                                          ),
                    standard_concept_name = .data$concept_name,
                    standard_concept_id = .data$concept_id
                    ) %>%
      dplyr::mutate(codelist_name = names(x)[i])

  }
  }

  if("person" %in% countBy){
  allPersonCount <- getAchillesPersonCounts(cdm = cdm, conceptId = allCodes)
  allPersonCount <- allPersonCount %>%
    dplyr::mutate(concept_id = as.character(.data$concept_id)) %>%
    dplyr::left_join(codesWithDetails %>%
                       dplyr::mutate(concept_id = as.character(.data$concept_id)),
                     by = "concept_id")
  for(i in seq_along(x)){
    codeUse[[paste0(i, "_person")]] <- allPersonCount %>%
      dplyr::filter(.data$concept_id %in% x[[i]]) %>%
      dplyr::mutate(variable_name = "Person count",
                    group_level = paste0(.data$standard_concept,
                                         " concept: ",
                                         .data$concept_name, " (",
                                         .data$concept_id, ")"
                    ),
                    standard_concept_name = .data$concept_name,
                    standard_concept_id = .data$concept_id
      ) %>%
      dplyr::mutate(codelist_name = names(x)[i])
  }
  }

  codeUse <- dplyr::bind_rows(codeUse) %>%
    dplyr::mutate(group_name = "By concept",
           strata_name = "Overall",
           strata_level  = "Overall",
           variable_level  = "Overall",
           variable_type  = "Numeric",
           estimate_type  = "Count",
           estimate  = .data$n,
           source_concept_name = NA,
           source_concept_id = NA,
           cohort_name = NA
           ) %>%
    dplyr::select("group_name", "group_level",
                  "strata_name", "strata_level",
                  "variable_name", "variable_level", "variable_type",
                  "estimate_type", "estimate",
                  "standard_concept_name", "standard_concept_id",
                  "source_concept_name",
                  "source_concept_id",
                  "domain_id", "codelist_name","cohort_name")


  if(nrow(codeUse) == 0){
    cli::cli_inform(
      c(
        "i" = "No achilles counts found for the concepts provided."
      ))
  }

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

