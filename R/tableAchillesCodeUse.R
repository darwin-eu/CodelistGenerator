#' Format the result of summariseAchillesCodeUse into a table
#'
#' @param result A `<summarised_result>` with results of the type
#' "achilles_code_use".
#' @inheritParams typeTableDoc
#' @inheritParams tableStyleDoc
#' @inheritParams headerDoc
#' @inheritParams groupColumnDoc
#' @inheritParams hideDoc
#' @inheritParams tableStyleDoc
#' @inheritParams .optionsDoc
#'
#' @return A table with a formatted version of the summariseCohortCodeUse
#' result.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omopgenerics)
#'
#' cdm <- mockVocabRef("database")
#' oa <- getCandidateCodes(cdm = cdm, keywords = "osteoarthritis")
#' result_achilles <- summariseAchillesCodeUse(newCodelist(list(oa = oa$concept_id)),
#'                                             cdm = cdm)
#' tableAchillesCodeUse(result_achilles)
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
tableAchillesCodeUse <- function(result,
                                 type = "gt",
                                 header = c("cdm_name", "estimate_name"),
                                 groupColumn = character(),
                                 hide = character(),
                                 style = "default",
                                 .options = list()) {

  rlang::check_installed("visOmopResults", version = "1.4.0")

  # checks
  if(nrow(result) == 0){
    cli::cli_warn("`result` object is empty")
    return(visOmopResults::emptyTable(type = type))
  }

  result <- result |>
    visOmopResults::filterSettings(.data$result_type == "achilles_code_use")

  if(nrow(result) == 0){
    cli::cli_warn("No achilles code use results found in result object")
    return(visOmopResults::emptyTable(type = type))
  }

  x <- internalTableAchillesResult(
    result = result,
    resultType = "achilles_code_use",
    type = type,
    header = header,
    groupColumn = groupColumn,
    hide = hide,
    style = style,
    .options = .options
  )

  return(x)
}

