
#' Findunmapped concepts related to codelist
#'
#' @param x A codelist
#' @param cdm A cdm reference
#' @param table Names of clinical tables in which to search for unmapped codes.
#' Can be one or more of "condition_occurrence", "device_exposure",
#' "drug_exposure", "measurement", "observation", and "procedure_occurrence".
#'
#' @return A summarised result
#' @export
summariseUnmappedCodes <- function(x,
                                   cdm,
                                   table = c(
                                     "condition_occurrence",
                                     "device_exposure",
                                     "drug_exposure",
                                     "measurement",
                                     "observation",
                                     "procedure_occurrence"
                                   )) {
  if (isFALSE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm is not a cdm reference but is {class(cdm)}")
  }
  if (isFALSE(inherits(x, "codelist")) && isFALSE(is.list(x))) {
    cli::cli_abort("x is not a codelist but is {class(cdm)}")
  }

  omopgenerics::assertChoice(
    table,
    c(
      "condition_occurrence",
      "device_exposure",
      "drug_exposure",
      "measurement",
      "observation",
      "procedure_occurrence"
    )
  )

  x <- omopgenerics::newCodelist(x)

  result <- list()
  for (i in seq_along(x)) {
    workingName <- names(x)[i]
    cli::cli_inform("Searching for unmapped codes related to {workingName}")
    ns <- getMappings(
      candidateCodelist = data.frame(concept_id = x[[i]]),
      cdm = cdm,
      nonStandardVocabularies = getVocabularies(cdm = cdm)
    )
    ns_concept_id <- unique(ns |> dplyr::pull("non_standard_concept_id"))

    tempNs <- paste0(
      omopgenerics::uniqueTableName(),
      omopgenerics::uniqueId()
    )
    cdm <- omopgenerics::insertTable(
      cdm = cdm,
      name = tempNs,
      table = dplyr::tibble(concept_id = ns_concept_id),
      overwrite = TRUE,
      temporary = FALSE
    )
    cdm[[tempNs]] <- cdm[[tempNs]] |>
      dplyr::inner_join(
        cdm$concept |>
          dplyr::select("concept_id", "concept_name"),
        by = c("concept_id")
      ) |>
      dplyr::compute(
        name = tempNs,
        temporary = FALSE
      )


    unmappedWorkingCount <- list()
    for (j in seq_along(table)) {
      workingTable <- table[j]
      standardConcept <- dplyr::case_when(
        workingTable == "condition_occurrence" ~ "condition_concept_id",
        workingTable == "device_exposure" ~ "device_concept_id",
        workingTable == "drug_exposure" ~ "drug_concept_id",
        workingTable == "measurement" ~ "measurement_concept_id",
        workingTable == "observation" ~ "observation_concept_id",
        workingTable == "procedure_occurrence" ~ "procedure_concept_id"
      )

      workingConcept <- dplyr::case_when(
        workingTable == "condition_occurrence" ~ "condition_source_concept_id",
        workingTable == "device_exposure" ~ "device_source_concept_id",
        workingTable == "drug_exposure" ~ "drug_source_concept_id",
        workingTable == "measurement" ~ "measurement_source_concept_id",
        workingTable == "observation" ~ "observation_source_concept_id",
        workingTable == "procedure_occurrence" ~ "procedure_source_concept_id"
      )
      workingCount <- cdm[[workingTable]] |>
        dplyr::filter(!!rlang::sym(standardConcept) == 0) |>
        dplyr::inner_join(
          cdm[[tempNs]] |>
            dplyr::rename(!!workingConcept := "concept_id"),
          by = workingConcept
        ) |>
        dplyr::group_by(!!rlang::sym(workingConcept), .data$concept_name) |>
        dplyr::summarise(n = as.integer(dplyr::n())) |>
        dplyr::ungroup() |>
        dplyr::rename("concept_id" = .env$workingConcept) |>
        dplyr::collect()

      if (nrow(workingCount) > 0) {
        workingCount <- workingCount |>
          dplyr::mutate(name = .env$workingName)
      }
      unmappedWorkingCount[[paste0(workingTable, "_", j)]] <- workingCount
    }
    result[[workingName]] <- dplyr::bind_rows(unmappedWorkingCount)
    if (nrow(result[[workingName]]) == 0) {
      cli::cli_inform(" - No unmapped codes found for {workingName}")
    }
  }
  result <- result |>
    vctrs::list_drop_empty() |>
    dplyr::bind_rows()

  CDMConnector::dropTable(cdm, name = tempNs)

  # code could be in multiple tables
  if (nrow(result) == 0) {
    return(omopgenerics::emptySummarisedResult(settings = dplyr::tibble(
      result_id = 1L,
      result_type = "unmapped_codes",
      package_name = "CodelistGenerator",
      package_version = as.character(utils::packageVersion(
        pkg = "CodelistGenerator"
      ))
    )))
  }
  result <- result |>
    dplyr::summarise(
      n = sum(.data$n, na.rm = TRUE),
      .by = c("concept_id", "concept_name", "name")
    )

  result <- result |>
    dplyr::mutate(
      result_id = 1L,
      cdm_name = omopgenerics::cdmName(cdm),
      group_name = "codelist_name",
      group_level = .data$name,
      strata_name = "overall",
      strata_level = "overall",
      variable_name = as.character(.data$concept_name),
      variable_level = as.character(.data$concept_id),
      estimate_name = "record_count",
      estimate_type = "integer",
      estimate_value = as.character(.data$n),
      additional_name = "overall",
      additional_level = "overall"
    ) |>
    dplyr::select(!c("n", "concept_name", "concept_id", "name")) |>
    omopgenerics::newSummarisedResult(
      settings = dplyr::tibble(
        result_id = 1L,
        result_type = "unmapped_codes",
        package_name = "CodelistGenerator",
        package_version = as.character(utils::packageVersion(
          pkg = "CodelistGenerator"
        ))
      )
    )

  result
}
