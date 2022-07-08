
#' Prepare words for search
#' @param words A character vector.
#' @noRd

tidyWords <- function(words) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(words, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # some generic formatting
  workingWords <- trimws(words)
  workingWords <- stringr::str_replace_all(workingWords, "-", " ")
  workingWords <- stringr::str_replace_all(workingWords, "[[:punct:]]", "")
  workingWords <- stringr::str_remove_all(workingWords, "[^[\\da-zA-Z ]]")
  workingWords <- stringr::str_remove_all(workingWords, "[^\x01-\x7F]+")
  workingWords <- stringr::str_to_lower(workingWords)
  workingWords <- trimws(workingWords)

  workingWords
}
