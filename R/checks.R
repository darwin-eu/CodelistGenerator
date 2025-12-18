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
  if (!("cdm_reference" %in% class(cdm))) {
    cli::cli_abort(
      "Argument cdm is not a valid cdm reference, please use
      CDMConnector::cdmFromCon() to create a valid cdm reference"
    )
  }
}

checkCodelist <- function(x, allowNULL = TRUE, allowConceptSetExpression = TRUE, call = parent.frame()){
  type <- class(x)

  # If X is NULL
  if(is.null(x) && isFALSE(allowNULL)){
    cli::cli_abort("`x` cannot be NULL.")
  }
  # If X is NULL and is allowed
  if(is.null(x) && isTRUE(allowNULL)){
    return(invisible(x))
  }
  # If x is not a codelist_with_details, codelist and concept set expression is allowed
  if(!any(c("codelist_with_details", "codelist", "concept_set_expression") %in% type) && isTRUE(allowConceptSetExpression)){
    cli::cli_abort("Please convert your list `x` into a codelist using newCodelist()/newCodelistWithDetails()/newConceptSetExpression() functions from omopgenerics package.
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html", call = call)
  }
  # If x is not a codelist_with_details, codelist and concept set expression is not supported
  if(!any(c("codelist_with_details", "codelist", "concept_set_expression") %in% type) && isFALSE(allowConceptSetExpression)){
    cli::cli_abort("Please convert your list `x` into a codelist using newCodelist()/newCodelistWithDetails() functions from omopgenerics package.
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html", call = call)
  }
  # If x is a concept_set_expression but is not yet supported
  if("concept_set_expression" %in% type && isFALSE(allowConceptSetExpression)){
    cli::cli_abort("concept_set_expression is not supported for this function yet. Please convert your codelist to a codelist or a codelist with details using asCodelist() or asCodelistWithDetails().
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html", call = call)
  }
  # If x is a list and concept_set_expression is supported
  if(!any(c("codelist_with_details", "codelist", "concept_set_expression") %in% type) && "list" %in% type && isTRUE(allowConceptSetExpression)){
    cli::cli_abort("Please convert your list `x` into a codelist using newCodelist()/newCodelistWithDetails()/newConceptSetExpression() functions from omopgenerics package.
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html", call = call)
  }
  # If x is a list and concept_set_expression is not supported
  if(!any(c("codelist_with_details", "codelist", "concept_set_expression") %in% type) && "list" %in% type && isTRUE(allowConceptSetExpression)){
    cli::cli_abort("Please convert your list `x` into a codelist using newCodelist()/newCodelistWithDetails()functions from omopgenerics package.
                     If you are unfamiliar with this classes, please visit: https://darwin-eu.github.io/CodelistGenerator/articles/a03_TypesOfCodelist.html", call = call)
  }
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

