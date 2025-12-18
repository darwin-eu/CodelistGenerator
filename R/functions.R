getConceptIDName <- function(omopTableName){
  if(omopTableName == "visit_occurrence") {
    return("visit_concept_id")
  }
  if(omopTableName == "visit_detail") {
    return("visit_detail_concept_id")
  }
  if(omopTableName == "condition_occurrence") {
    return("condition_concept_id")
  }
  if(omopTableName == "drug_exposure") {
    return("drug_concept_id")
  }
  if(omopTableName == "procedure_occurrence") {
    return("procedure_concept_id")
  }
  if(omopTableName == "device_exposure") {
    return("device_concept_id")
  }
  if(omopTableName == "measurement") {
    return("measurement_concept_id")
  }
  if(omopTableName == "observation") {
    return("observation_concept_id")
  }
  if(omopTableName == "drug_strength") {
    return("drug_concept_id")
  }
  if(omopTableName == "concept_relationship"){
    return("concept_id_1")
  }
}

subsetOmopTable <- function(cdm, newOmopTable, omopTable, x){

  if(!is.null(x)){
    if(inherits(x, "codelist_with_details")){
      x <- as.numeric(unlist(purrr::map(x, \(x) x |> dplyr::pull("concept_id"))))
    } else {
      x <- as.numeric(unlist(x))
    }

    tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                            omopgenerics::uniqueId())

    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble("concept_id" = x),
                                     overwrite = TRUE,
                                     temporary = FALSE)

    cdm[[newOmopTable]] <- cdm[[omopTable]] |>
      dplyr::rename("concept_id" = !!getConceptIDName(omopTable)) |>
      dplyr::inner_join(
        cdm[[tableCodelist]],
        by = "concept_id"
      ) |>
      dplyr::compute(temporary = FALSE, name = newOmopTable)

    omopgenerics::dropSourceTable(cdm, name = tableCodelist)

  } else {
    cdm[[newOmopTable]] <- cdm[[omopTable]] |>
      dplyr::rename("concept_id" = !!getConceptIDName(omopTable)) |>
      dplyr::compute(name = newOmopTable, temporary = FALSE)
  }

  return(cdm)
}
