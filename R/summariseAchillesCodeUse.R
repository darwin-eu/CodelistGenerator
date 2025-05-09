#' Summarise code use from achilles counts.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams countByDoc
#'
#' @return A tibble with summarised counts.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef("database")
#' oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#' result_achilles <- summariseAchillesCodeUse(list(oa = oa$concept_id), cdm = cdm)
#' result_achilles
#' CDMConnector::cdmDisconnect(cdm)
#' }

summariseAchillesCodeUse <- function(x,
                            cdm,
                            countBy = c("record", "person")) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm,
                                           requiredTables = c("achilles_analysis",
                                                              "achilles_results",
                                                              "achilles_results_dist"))
  omopgenerics::assertList(x, named = TRUE)
  omopgenerics::assertChoice(countBy, choices = c("record", "person"))

  if(length(x) == 0){
    cli::cli_inform(
      c(
        "i" = "Empty codelist - no achilles counts to return"
      ))
    return(omopgenerics::emptySummarisedResult())
  }

  version <- achillesVersionDate(cdm)
  cli::cli_inform(version)

  allCodes <-  purrr::list_c(x)

  codesWithDetails <- addDetails(x, cdm)
  codesWithDetails <- purrr::list_rbind(codesWithDetails) |>
    dplyr::distinct()

  codeUse <- list()

  if("record" %in% countBy){
    allRecordCount <- getAchillesRecordCounts(cdm = cdm, conceptId = allCodes)
    if(nrow(allRecordCount)>=1){
      allRecordCount <- allRecordCount |>
        dplyr::mutate(concept_id = as.character(.data$concept_id)) |>
        dplyr::left_join(codesWithDetails |>
                           dplyr::mutate(concept_id = as.character(.data$concept_id)),
                         by = "concept_id")
      for(i in seq_along(x)){
        codeUse[[paste0(i, "_record")]] <- allRecordCount |>
          dplyr::filter(.data$concept_id %in% x[[i]]) |>
          dplyr::rename(
            "variable_name" = "concept_name", # standard concept name
            "variable_level" = "concept_id"   # standard concept id
          ) |>
          dplyr::mutate(estimate_name = "record_count") |>
          dplyr::mutate(group_level = names(x)[i])
      }
    }
  }

  if("person" %in% countBy){
    allPersonCount <- getAchillesPersonCounts(cdm = cdm, conceptId = allCodes)
    if(nrow(allPersonCount)>=1){
      allPersonCount <- allPersonCount |>
        dplyr::mutate(concept_id = as.character(.data$concept_id)) |>
        dplyr::left_join(codesWithDetails |>
                           dplyr::mutate(concept_id = as.character(.data$concept_id)),
                         by = "concept_id")
      for(i in seq_along(x)){
        codeUse[[paste0(i, "_person")]] <- allPersonCount |>
          dplyr::filter(.data$concept_id %in% x[[i]]) |>
          dplyr::rename(
            "variable_name" = "concept_name", # standard concept name
            "variable_level" = "concept_id"   # standard concept id
          ) |>
          dplyr::mutate(estimate_name = "person_count") |>
          dplyr::mutate(group_level = names(x)[i])
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
    codeUse <- dplyr::bind_rows(codeUse) |>
      dplyr::mutate(
        result_id = as.integer(1),
        cdm_name = CDMConnector::cdmName(cdm),
        group_name = "codelist_name",
        domain_id = tolower(.data$domain_id),
        estimate_type = "integer",
        estimate_value = as.character(.data$n)
      ) |>
      omopgenerics::uniteAdditional(cols = c("standard_concept", "vocabulary_id")) |>
      omopgenerics::uniteStrata(cols = c("domain_id")) |>
      dplyr::select(dplyr::any_of(omopgenerics::resultColumns("summarised_result")))

    codeUse <- codeUse |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = as.integer(1),
          result_type = "achilles_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion(
            pkg = "CodelistGenerator"))
        )
      )
  }

  return(codeUse)

}

achillesVersionDate <- function(cdm){

  omopgenerics::validateCdmArgument(cdm,
                                    requiredTables = c("achilles_analysis",
                                                       "achilles_results",
                                                       "achilles_results_dist"))

  cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id == 0) |>
    dplyr::collect() |>
    dplyr::mutate(achilles_version = paste0("Using achilles results from version ",
                                            .data$stratum_2,
                                            " which was run on ",
                                            .data$stratum_3)) |>
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
  analyses <- cdm[["achilles_results"]] |>
    dplyr::filter(.data$analysis_id %in%  .env$analysisId) |>
    dplyr::select("stratum_1", "count_value") |>
    dplyr::rename("concept_id" = "stratum_1",
                  "n"="count_value") |>
    dplyr::collect()

  if(!is.null(conceptId)){
    analyses <- analyses |>
      dplyr::filter(.data$concept_id %in%  .env$conceptId)
  }

  # the same code might appear in multiple tables so we will sum them
  analyses |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::summarise(n = sum(.data$n, na.rm = TRUE)) |>
    dplyr::mutate(n = as.integer(.data$n))
}
