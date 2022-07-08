#' Compare two codelists
#'
#' @param codelist1 Output of get_candidate_codes
#' @param codelist2 Output of get_candidate_codes
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here ")
#' vocabularyDatabaseSchema <- " Your vocabulary schema here "
#' asthmaCodes <- get_candidate_codes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabularyDatabaseSchema = " Your vocabulary schema here "
#' )
#' persistantAsthmaCodes <- get_candidate_codes(
#'   keywords = "Persistent asthma",
#'   db = db,
#'   vocabularyDatabaseSchema = " Your vocabulary schema here "
#' )
#' compareCodelists(
#'   codelist1 = asthmaCodes,
#'   codelist2 = persistantAsthmaCodes
#' )
#' }
compareCodelists <- function(codelist1,
                              codelist2) {

  ## checks for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(codelist1,
    add = errorMessage
  )
  checkmate::assert_tibble(codelist2,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist1)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(c("concept_id", "concept_name")  %in%
      names(codelist2)),
    add = errorMessage
  )
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)


  all <- dplyr::bind_rows(codelist1, codelist2)
  duplicates <- all[duplicated(all), ]
  unique <- unique(all)

  # function to return new column which
  # indicate which codelist the concept came from
  # If returns "Both" it means the concept contain
  # in both codelists
  unique$codelist <- dplyr::if_else(is.na(
    match(
      paste0(unique$concept_id, unique$concept_name),
      paste0(duplicates$concept_id, duplicates$concept_name)
    )
  ), dplyr::if_else(is.na(
    match(
      paste0(unique$concept_id, unique$concept_name),
      paste0(codelist1$concept_id, codelist1$concept_name)
    )
  ),
  "Only codelist 2",
  "Only codelist 1"),
  "Both")

  return(unique)
}
