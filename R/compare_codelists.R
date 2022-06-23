#' Compare two codelists
#'
#' @param codelist_1 Output of get_candidate_codes
#' @param codelist_2 Output of get_candidate_codes
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here ")
#' vocabulary_database_schema <- " Your vocabulary schema here "
#' asthma_codes <- get_candidate_codes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabulary_database_schema = " Your vocabulary schema here "
#' )
#' persistant_asthma_codes <- get_candidate_codes(
#'   keywords = "Persistent asthma",
#'   db = db,
#'   vocabulary_database_schema = " Your vocabulary schema here "
#' )
#' compare_codelists(
#'   codelist_1 = asthma_codes,
#'   codelist_2 = persistant_asthma_codes
#' )
#' }
compare_codelists <- function(codelist_1,
                              codelist_2) {

  ## checks for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(codelist_1,
    add = error_message
  )
  checkmate::assert_tibble(codelist_2,
    add = error_message
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist_1)),
    add = error_message
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist_2)),
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  all <- dplyr::bind_rows(codelist_1, codelist_2)
  duplicates <- all[duplicated(all), ]
  unique <- unique(all)

  # function to return new column which indicate which codelist the concept came from
  # If returns "Both" it means the concept contain in both codelists
  unique$codelist <- dplyr::if_else(is.na(
    match(
      paste0(unique$concept_id, unique$concept_name),
      paste0(duplicates$concept_id, duplicates$concept_name)
    )
  ), dplyr::if_else(is.na(
    match(
      paste0(unique$concept_id, unique$concept_name),
      paste0(codelist_1$concept_id, codelist_1$concept_name)
    )
  ),
  "Only codelist_2",
  "Only codelist_1"),
  "Both")

  return(unique)
}
