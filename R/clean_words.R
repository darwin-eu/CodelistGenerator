
#' Prepare words for search.
#'
#' @param words A character vector.
#'
#' @return A character vector of the same length.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples clean_words(c("Type-1 diabetes "))
clean_words<-function(words){
  # some generic formatting
  clean.words<-trimws(words)
  clean.words<-stringr::str_replace_all(clean.words, "[[:punct:]]", "")
  clean.words<-stringr::str_remove_all(clean.words, "[^[\\da-zA-Z ]]")
  clean.words<-stringr::str_remove_all(clean.words, "[^\x01-\x7F]+")
  clean.words<-stringr::str_to_lower(clean.words)
  clean.words<-stringr::str_replace_all(clean.words, "-", " ")
  clean.words<-trimws(clean.words)
  clean.words
}
