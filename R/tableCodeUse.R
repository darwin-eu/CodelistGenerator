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
                         formatEstimateName = c("N (%)" = "<count> (<percentage> %)"),
                         header = c("cdm_name", "estimate"),
                         split = c("variable", "additional"),
                         groupColumn = NULL, # specify which can be: cdm_name, codelist_name
                         conceptId = TRUE,
                         sourceConcept = TRUE,
                         excludeColumns = c("result_id", "estimate_type", "additional_name", "additional_level"),
                         minCellCount = 5) {

  # checks
  # if sourceConcept TRUE but additional in exclude: only exclude domain

  if (sourceConcept & grepl("additional", excludeColumns)) {
    # remove domain from additional
    domains <- c("condition", "drug", "observation", "measurement", "visit", "procedure", "device")
    domains <- paste0(domains, collapse = "|")
    x <- result |>
      dplyr::mutate(
        additional_name = gsub(" &&& domain_id", "", .data$additional_name),
        additional_level = stringr::str_replace(.data$additional_level, paste0(" &&& ", .env$domains), "")
      )
    # remove additional of excluded columns
    excludeColumns <- excludeColumns[!grepl("additional", excludeColumns)]
    # add additional to split if not there
    if (!"additional" %in% split) {split <- c(split, "additional")}
  }

  if (!conceptId) {

  }

}
