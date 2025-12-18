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

#' Perform a systematic search to identify a candidate codelist using the OMOP
#' CDM vocabulary tables.
#'
#' @description
#' Based on the given search strategy, this function identifies a set of codes
#' that may represent a clinical event of interest in data mapped to the OMOP
#' CDM. These codes can then be considered for creating a study phenotype.
#'
#' @inheritParams cdmDoc
#' @param keywords Character vector of terms to search for.
#' * The search performed is broad, matching the provided keywords as substrings
#' anywhere within concept names. For example, `keywords = c("sep")` will
#' find "sepsis", "aseptic necrosis", and "acquired cardiac septal defect"
#' (along with many more).
#' * Where more than one word is given, all combinations of those words will be
#' identified. For example, `keywords = c("knee osteoarthritis")` will identify
#' a concept with the name "osteoarthritis of knee".
#' * Multiple keywords can be provided. For example,
#' `keywords = c("knee osteoarthritis", "hip osteoarthritis")` would identify
#' both "osteoarthritis of knee" and "osteoarthritis of hip"
#' @param exclude  Character vector of words to identify concepts to exclude.
#' For example, `getCandidateCodes(cdm,  keywords = "septic", exclude = "aseptic",  domains = "condition")`
#' would remove concepts "aseptic" when seaching for concepts with "septic" in
#' their name.
#' * When one term contains multiple words (e.g., "knee osteoarthritis"),
#' each word will be search individually, so that "osteoarthritis of knee" would also be excluded.
#' If you only want to exclude partial matching terms, please add "/" at the beginning and the end of each term
#' (e.g., `"/knee osteoarthritis/`"). Notice that, with this options,
#' concepts like "rightknee osteoarthritis" will also be excluded (as this is a partial match), but
#' "osteoarthritis of knee" won't be excluded. Different terms can have different rules (e.g.,
#' c("hip osteoarthritis", "/knee osteoarthritis/")).
#' * With multiple words, if we want exact matches accounting for word boundaries, we need to use
#' `/\b` at the beginning and at the end of each expression. In the previous example, using
#' `"/bknee osteoarthritis/\b"`, "rightknee osteoarthritis" won't be excluded, but
#' "History of knee osteoarthritis" will be excluded.
#' @param domains Character vector with one or more of the OMOP CDM domain for
#' which to search within. If NULL, all domains are included in the search. Use
#' `availableDomains(cdm = cdm)` to identify available domains to search within.
#' @inheritParams standardConceptDoc
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @inheritParams includeDescendantsDoc
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.
#' @return A "candidate_codes" object. This includes a tibble with the potential
#' codes of interest, along with an attribute containing the search strategy.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' cdm <- mockVocabRef()
#' getCandidateCodes(
#'   cdm = cdm,
#'   keywords = "osteoarthritis"
#'  )
#' }
getCandidateCodes <- function(cdm,
                              keywords,
                              exclude = NULL,
                              domains = "Condition",
                              standardConcept = "Standard",
                              searchInSynonyms = FALSE,
                              searchNonStandard = FALSE,
                              includeDescendants = TRUE,
                              includeAncestor = FALSE) {

  start <- Sys.time()

  ## checks for standard types of user error
  omopgenerics::assertCharacter(keywords, null = FALSE, na = FALSE)
  domains <- assertDomain(domains, cdm)
  omopgenerics::assertChoice(standardConcept, choices = c("Standard", "Classification", "Non-standard"))
  omopgenerics::assertLogical(searchInSynonyms)
  omopgenerics::assertLogical(searchNonStandard)
  omopgenerics::assertLogical(includeDescendants)
  omopgenerics::assertLogical(includeAncestor)

  requiredTables <- c("concept", "concept_relationship", "concept_ancestor",
                      "concept_synonym", "vocabulary")
  if("Drug" %in% stringr::str_to_sentence(domains)){
    requiredTables <- append(requiredTables, "drug_strength")
  }
  omopgenerics::validateCdmArgument(cdm, requiredTables = requiredTables)

  # run search by domain
  searchSpecs <- data.frame(
    id = seq_along(domains),
    domain = domains
  )
  searchSpecs <- split(
    searchSpecs,
    searchSpecs[, c("id")]
  )

  searchResults <- runSearch(keywords,
                             cdm = cdm,
                             exclude = exclude,
                             domains = domains,
                             standardConcept = standardConcept,
                             searchInSynonyms = searchInSynonyms,
                             searchNonStandard = searchNonStandard,
                             includeDescendants = includeDescendants,
                             includeAncestor = includeAncestor
  ) |>
    dplyr::arrange(.data$concept_id)

  if (nrow(searchResults) == 0) {
    cli::cli_inform("No codes found for the given search strategy")
    return(searchResults)
  }

  cli::cli_alert_success(
    "{nrow(searchResults)} candidate concept{?s} identified"
  )
  duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
  cli::cli_inform(
    "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
  )

  searchResults <- newCandidateCodesClass(searchResults)
  searchResults <- addAtribute(searchResults,
                               cdm,
                               keywords = keywords,
                               exclude = exclude,
                               domains = domains,
                               standardConcept = standardConcept,
                               searchInSynonyms = searchInSynonyms,
                               searchNonStandard = searchNonStandard,
                               includeDescendants = includeDescendants,
                               includeAncestor = includeAncestor)
  return(searchResults)
}

newCandidateCodesClass <- function(x){

  x <- validateCandidateCodes(x)
  x <- addClass(x, "candidate_codes")

  return(x)
}

addClass <- function(x, value) {
  if (any(value %in% class(x))) x <- removeClass(x, value)
  base::class(x) <- c(value, base::class(x))

  return(x)
}

removeClass <- function(x, value) {
  if(value == "candidate_codes" | value == "candidateCodes"){
    attr(x, "search_strategy") <- NULL
  }
  base::class(x) <- base::class(x)[!(base::class(x) %in% value)]
  if(value == "candidate_codes"){
    attr(x, "search_strategy") <- NULL
  }
  return(x)
}

validateCandidateCodes <- function(x, call = parent.frame()) {
  # Check column presence
  x <- x |>
    omopgenerics::assertTable(
      class = c("tbl", "data.frame"),
      allowExtraColumns = FALSE,
      null = FALSE,
      columns = c("concept_id", "found_from", "concept_name", "domain_id",
                  "vocabulary_id", 'standard_concept')
    )

  # Check column types
  required_types <- list(
    "concept_id"       = "integer",
    "found_from"       = "character",
    "concept_name"     = "character",
    "domain_id"        = "character",
    "vocabulary_id"    = "character",
    "standard_concept" = "character"
  )

  # Check column types and warn
  for(i in names(required_types)) {
    actual <- class(x[[i]])
    expected <- required_types[[i]]
    if (actual != expected) {
      cli::cli_warn("Column {.val {colnames(x)[i]}} is {.val {actual}} but should be {.val {expected}}. It will be coerced.")
    }
  }

  x <- x |>
    dplyr::mutate(
      "concept_id" = as.integer(.data$concept_id),
      "found_from" = as.character(.data$found_from),
      "concept_name" = as.character(.data$concept_name),
      "domain_id"  = as.character(.data$domain_id),
      "vocabulary_id" = as.character(.data$vocabulary_id),
      "standard_concept" = as.character(.data$standard_concept)
    )

  # sort by concept_id and found_from
  found_from_values <- append(
    unique(x$found_from)[stringr::str_ilike(unique(x$found_from), pattern = "from initial search")],
    unique(x$found_from)[!stringr::str_ilike(unique(x$found_from), pattern = "from initial search")]
  )
  x <- x |>
    dplyr::mutate("found_from" = factor(.data$found_from, .env$found_from_values)) |>
    dplyr::arrange(.data$concept_id, .data$found_from) |>
    dplyr::mutate("found_from" = as.character(.data$found_from))

  # Add attribute
  attr(x, "search_strategy") <- dplyr::tibble(
    "vocabulary_version" = character(),
    "keywords" = character(),
    "exclude"  = character(),
    "domains"  = character(),
    "standard_concept" = character(),
    "search_in_synonyms" = logical(),
    "search_non_standard" = logical(),
    "include_descendants" = logical(),
    "include_ancestor" = logical())

  return(x)
}

addAtribute <- function(x, cdm, keywords, exclude, domains, standardConcept,
                        searchInSynonyms, searchNonStandard, includeDescendants,
                        includeAncestor){
  attr(x, "search_strategy") <- dplyr::tibble(
    "cdm_name" = omopgenerics::cdmName(cdm),
    "vocabulary_version" = vocabularyVersion(cdm),
    "keywords" = paste0('"', keywords, '"', collapse = ", "),
    "exclude"  = paste0('"', exclude, '"', collapse = ", "),
    "domains"  = paste0('"', domains, '"', collapse = ", "),
    "standard_concept" = paste0('"', standardConcept, '"', collapse = ", "),
    "search_in_synonyms" = searchInSynonyms,
    "search_non_standard" = searchNonStandard,
    "include_descendants" = includeDescendants,
    "include_ancestor" = includeAncestor) |>
    dplyr::mutate("exclude" = dplyr::na_if(.data$exclude, ""))

  return(x)
}
