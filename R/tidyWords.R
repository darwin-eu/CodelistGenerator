# Copyright 2022 DARWIN EU (C)
#
# This file is part of IncidencePrevalence
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


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
