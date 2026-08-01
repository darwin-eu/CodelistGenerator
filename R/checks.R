# Copyright 2025 DARWIN EU®
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
      paste0(paste0("inputs[[\"", varName, "\"]]"), collapse = ", "), ")"
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
  if (!(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkCodelist <- function(x, allowNULL = TRUE, allowConceptSetExpression = TRUE, call = parent.frame()){
  type <- class(x)

  # If X is NULL
  if (is.null(x)) {
    if (allowNULL) {
      return(x)
    } else {
      cli::cli_abort(c(x = "`x` cannot be NULL."), call = call)
    }
  }

  opts <- c("codelist", "codelist_with_details")
  if (allowConceptSetExpression) {
    funs <- "newCodelist()/newCodelistWithDetails()/newConceptSetExpression()/newCodeSearch()"
    opts <- c(opts, "concept_set_expression")
  } else {
    funs <- "newCodelist()/newCodelistWithDetails()/newCodeSearch()"
  }
  opts <- c(opts, "code_search")

  # if x is a list ask to convert
  if ("list" %in% type & !any(opts %in% type)) {
    cli::cli_abort(
      message = c(x = "Please convert your list `x` into a codelist using {funs} functions from omopgenerics package.
                       If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html"),
      call = call
    )
  }

  # if x is concept_set_expression but not allowed
  if ("concept_set_expression" %in% type & !allowConceptSetExpression) {
    cli::cli_abort(c(x = "concept_set_expression is not supported for this function yet. Please convert your codelist to a codelist or a codelist with details using asCodelist() or asCodelistWithDetails().
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html"), call = call)
  }

  # If x is not any of the supported classes
  if (!any(opts %in% type)) {
    cli::cli_abort(c(x = "Please convert your list `x` into a codelist using {funs} functions from omopgenerics package.
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html"), call = call)
  }

  return(x)
}

dropEmptyCodelist <- function(x_original, newX, call = parent.frame()){

  newX <- newX |> vctrs::list_drop_empty()

  n1 <- names(x_original)
  n2 <- names(newX)

  n1 <- setdiff(n1, n2)
  if(length(n1) > 0){
    cli::cli_warn("{.val {n1}} codelist{?s} will be removed from the final codelist, as there are no elements left after subsetting.",
                  call = call)
  }

  if(length(newX) == 0){
    newX <- createEmptyCodelist(newX)
  }
  return(newX)
}

createEmptyCodelist <- function(x){
  if(inherits(x, "codelist")){
    x <- omopgenerics::emptyCodelist()
  }
  if(inherits(x, "codelist_with_details")){
    x <- omopgenerics::emptyCodelistWithDetails()
  }
  return(x)
}

validateType <- function(type, call = parent.frame()) {
  if (is.null(type)) {
    type <- getOption("CodelistGenerator.type", NULL)
  }
  omopgenerics::assertChoice(type, choices = c(
    "codelist", "codelist_with_details", "concept_set_expression", "code_search"
  ), call = call)
  return(type)
}
