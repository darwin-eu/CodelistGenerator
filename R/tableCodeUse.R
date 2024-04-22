#' ....
#'
#' @param x .
#'
#' @return ...
#' @export
#'
#' @examples
#'
#'
tableCodeUse <- function(result,
                         type = "gt",
                         header = c("cdm_name", "estimate"),
                         split = c("group", "additional"),
                         groupColumn = NULL, # specify which can be: cdm_name, codelist_name
                         conceptId = TRUE,
                         sourceConcept = TRUE,
                         excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                         minCellCount = 5) {

  # checks
  # FILTER FOR RESULT TYPE

  if (!conceptId) {
    x <- result |>
      dplyr::mutate(variable_level = NA_character_)
    if (sourceConcept) {
      x <- x |>
        dplyr::mutate(
          additional_name = gsub(" &&& source_concept_id", "", .data$additional_name),
          additional_level = sub("&.*&", "&&&", .data$additional_level)
        )
    }
    renameColumns = c("standard_concept_name" = "variable_name")
  } else {
    x <- result
    renameColumns = c("standard_concept_name" = "variable_name",
                      "standard_concept_id" = "variable_level")
  }

  if (sourceConcept & any(grepl("additional", excludeColumns))) {
    # remove domain from additional
    domains <- c("condition", "drug", "observation", "measurement", "visit", "procedure", "device")
    domains <- paste0(" &&& ", domains, collapse = "|")
    x <- x |>
      dplyr::mutate(
        additional_name = gsub(" &&& domain_id", "", .data$additional_name),
        additional_level = gsub(domains, "", .data$additional_level)
      )
    # remove additional of excluded columns
    excludeColumns <- excludeColumns[!grepl("additional", excludeColumns)]
    # add additional to split if not there
    if (!"additional" %in% split) {split <- c(split, "additional")}

  } else if (!sourceConcept & !grepl("additional", excludeColumns)) {
    # only keep domain from additional
    x <- x |>
      visOmopResults::splitAdditional() |>
      dplyr::select(- dplyr::starts_with("source")) |>
      visOmopResults::uniteAdditional(cols = "domain_id")
  }

  x <- x |>
    dplyr::mutate(estimate_name = stringr::str_to_sentence(gsub("_", " ", .data$estimate_name)))

  x <- visOmopResults::formatTable(
    result = x,
    formatEstimateName = formatEstimateName,
    header = header,
    split = split,
    groupColumn = groupColumn,
    type = type,
    renameColumns = c("standard_concept_name" = "variable_name", "standard_concept_id" = "variable_level"),
    minCellCount = minCellCount,
    excludeColumns = excludeColumns,
    .options = .options
  )

  return(x)
}
