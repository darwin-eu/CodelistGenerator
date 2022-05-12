
#' Get vocabulary version
#'
#' @param db Database connection via DBI::dbConnect()
#' @param vocabulary_database_schema Name of database schema with vocab tables
#'
#' @return Character vector
#' @export
#'
#' @examples
#' library(Eunomia)
#' library(DBI)
#' library(RSQLite)
#' \dontrun{
#' untar(xzfile(system.file("sqlite", "cdm.tar.xz",
#'   package = "Eunomia"
#' ), open = "rb"),
#' exdir = tempdir()
#' )
#' db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
#' get_vocab_version(
#'   db = db,
#'   vocabulary_database_schema = "main"
#' )
#' }
get_vocab_version <- function(db,
                              vocabulary_database_schema) {

  # connect to relevant vocabulary table
  vocabulary_db <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
    ".vocabulary"
  )))

  # ensure names are lowercase
  vocabulary_db <- dplyr::rename_with(vocabulary_db, tolower)

  # get vocab version
  vocabulary_db %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()
}
