
#' Prepare words for search
#' @param words A character vector.
#' @noRd

clean_words <- function(words) {
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertVector(words, add = error_message)
  checkmate::reportAssertions(collection = error_message)

  # some generic formatting
  working_words <- trimws(words)
  working_words <- stringr::str_replace_all(working_words, "-", " ")
  working_words <- stringr::str_replace_all(working_words, "[[:punct:]]", "")
  working_words <- stringr::str_remove_all(working_words, "[^[\\da-zA-Z ]]")
  working_words <- stringr::str_remove_all(working_words, "[^\x01-\x7F]+")
  working_words <- stringr::str_to_lower(working_words)
  working_words <- trimws(working_words)

  working_words
}
