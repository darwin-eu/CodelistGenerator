#' Filter a codelist to keep only the codes used in the database
#'
#' @param x A codelist
#' @param cdm cdm_reference via CDMConnector
#' @param table cdm table
#'
#' @return
#' @export
#'
#' @examples
restrictToCodesInUse <- function(x,
                               cdm,
                               table = c("condition_occurrence",
                                         "device_exposure",
                                         "drug_exposure",
                                         "measurement",
                                         "observation",
                                         "procedure_occurrence",
                                         "visit_occurrence")){

dbCodes <- codesInUse(cdm = cdm,
                      table = table)

if(is.null(dbCodes)){
  for(i in seq_along(x)){
    cli::cli_inform("No codes from any codelist found in the database")
    return(invisible(NULL))
  }
} else {
  for(i in seq_along(x)){
    x[[i]] <- intersect(x[[i]], dbCodes)
  }
}

x

}

#' Get codes used in the database
#'
#' @param cdm cdm_reference via CDMConnector
#' @param table cdm table
#'
#' @return
#' @export
#'
#' @examples
codesInUse <- function(cdm,
                       table = c("condition_occurrence",
                                 "device_exposure",
                                 "drug_exposure",
                                 "measurement",
                                 "observation",
                                 "procedure_occurrence",
                                 "visit_occurrence")){

if(!is.null(cdm[["achilles_results"]])){
  codes <- fetchAchillesCodesInUse(cdm)
} else {
  # if achilles not available, query cdm
  codes <- list()
  for(i in seq_along(table)){
  workingTable <- table[i]
  workingConcept <- dplyr::case_when(
    workingTable == "condition_occurrence" ~ "condition_concept_id",
    workingTable == "device_exposure" ~ "device_concept_id",
    workingTable == "drug_exposure" ~ "drug_concept_id",
    workingTable == "measurement" ~ "measurement_concept_id",
    workingTable == "observation" ~ "observation_concept_id",
    workingTable == "procedure_occurrence" ~ "procedure_concept_id",
    workingTable == "visit_occurrence" ~ "visit_concept_id"
    )

  if(!is.null(cdm[[workingTable]])){
    codes[[i]] <- as.integer(cdm[[workingTable]] %>%
                               dplyr::select(workingConcept) %>%
                               dplyr::distinct() %>%
                               dplyr::pull())
  } else {
    codes[[i]] <- NULL
  }

  }
  codes <- unlist(codes)
  }

  codes
}

#' Get source codes used in the database
#'
#' @param cdm cdm_reference via CDMConnector
#' @param table cdm table
#'
#' @return
#' @export
#'
#' @examples
sourceCodesInUse <- function(cdm,
                                 table = c("condition_occurrence",
                                           "device_exposure",
                                           "drug_exposure",
                                           "measurement",
                                           "observation",
                                           "procedure_occurrence",
                                           "visit_occurrence")){

  if(!is.null(cdm[["achilles_results"]])){
    codes <- fetchAchillesCodesInUse(cdm)
  } else {
    # if achilles not available, query cdm
    codes <- list()
    for(i in seq_along(table)){
      workingTable <- table[i]
      workingConcept <- dplyr::case_when(
        workingTable == "condition_occurrence" ~ "condition_source_concept_id",
        workingTable == "device_exposure" ~ "device_source_concept_id",
        workingTable == "drug_exposure" ~ "drug_source_concept_id",
        workingTable == "measurement" ~ "measurement_source_concept_id",
        workingTable == "observation" ~ "observation_source_concept_id",
        workingTable == "procedure_occurrence" ~ "procedure_source_concept_id",
        workingTable == "visit_occurrence" ~ "visit_source_concept_id"
      )

      codes[[i]] <- as.integer(cdm[[workingTable]] %>%
                                 dplyr::select(workingConcept) %>%
                                 dplyr::distinct() %>%
                                 dplyr::pull())
    }
    codes <- unlist(codes)
  }

  codes
}

unmappedSourceCodesInUse <- function(cdm,
                                         table = c("condition_occurrence",
                                                   "device_exposure",
                                                   "drug_exposure",
                                                   "measurement",
                                                   "observation",
                                                   "procedure_occurrence",
                                                   "visit_occurrence")){

  # note, no achilles query for this so will have to query the cdm

  codes <- list()
  for(i in seq_along(table)){
    workingTable <- table[i]
    standardConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_concept_id",
      workingTable == "device_exposure" ~ "device_concept_id",
      workingTable == "drug_exposure" ~ "drug_concept_id",
      workingTable == "measurement" ~ "measurement_concept_id",
      workingTable == "observation" ~ "observation_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_concept_id",
      workingTable == "visit_occurrence" ~ "visit_concept_id"
    )

    workingConcept <- dplyr::case_when(
      workingTable == "condition_occurrence" ~ "condition_source_concept_id",
      workingTable == "device_exposure" ~ "device_source_concept_id",
      workingTable == "drug_exposure" ~ "drug_source_concept_id",
      workingTable == "measurement" ~ "measurement_source_concept_id",
      workingTable == "observation" ~ "observation_source_concept_id",
      workingTable == "procedure_occurrence" ~ "procedure_source_concept_id",
      workingTable == "visit_occurrence" ~ "visit_source_concept_id"
    )

    # keep unmapped codes
    codes[[i]] <- as.integer(cdm[[workingTable]] %>%
      dplyr::filter(!!rlang::sym(standardConcept) == 0) %>%
      dplyr::select(dplyr::all_of(workingConcept)) %>%
      dplyr::distinct() %>%
      dplyr::pull())
    codes[[i]] <- na.omit(codes[[i]])
  }

  codes <- unlist(codes)

  codes
}

fetchAchillesCodesInUse <- function(cdm){
  cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id %in%
      c(
        401, # condition occurrence
        701, # drug_exposure
        801, # observation
        1801, # measurement
        201, # visit_occurrence
        601, # procedure_occurrence
        2101 # device_exposure
      )) %>%
    dplyr::select("stratum_1") %>%
    dplyr::distinct() %>%
    dplyr::mutate(stratum_1 = as.integer(.data$stratum_1)) %>%
    dplyr::pull("stratum_1")
}

fetchAchillesSourceCodesInUse <- function(cdm){
  cdm[["achilles_results"]] %>%
    dplyr::filter(.data$analysis_id %in%
                    c(
                      425, # condition occurrence
                      725, # drug_exposure
                      825, # observation
                      1825, # measurement
                      225, # visit_occurrence
                      625, # procedure_occurrence
                      2125 # device_exposure
                    )) %>%
    dplyr::select("stratum_1") %>%
    dplyr::distinct() %>%
    dplyr::mutate(stratum_1 = as.integer(.data$stratum_1)) %>%
    dplyr::pull("stratum_1")
}
