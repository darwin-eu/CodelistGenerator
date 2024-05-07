# Copyright 2024 DARWIN EU (C)
#
# This file is part of CodelistGenerator
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

checkInputs <- function(...) {
  inputs <- list(...)
  lapply(names(inputs), function(x) {
    funName <- paste0(
      "check", toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))
    )
    varName <- eval(parse(text = paste0("names(formals(", funName, "))")))
    eval(parse(text = paste0(
      funName, "(",
      paste0( paste0("inputs[[\"", varName, "\"]]"), collapse = ", "), ")"
    )))
  })
  invisible(NULL)
}

checkPath <- function(path) {
  if(typeof(path) != "character" || length(path) != 1) {
    cli::cli_abort("path is not a character of length 1")
  }

  if (!file.exists(path)) {
    cli::cli_abort(glue::glue("Invalid path: {path}"))
  }
}

checkCdm <- function(cdm) {
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}
