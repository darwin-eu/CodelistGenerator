# Copyright 2022 DARWIN EU®
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



#' Generate candidate codelist for the OMOP CDM
#'
#' @description
#' This function generates a set of codes that
#' can be considered for creating a phenotype
#' using the OMOP CDM.
#'
#' @param cdm cdm_reference via CDMConnector
#' @param keywords Character vector of words to search for.
#' Where more than one word is given (e.g. "knee osteoarthritis"),
#' all combinations of those words should be identified
#' positions (e.g. "osteoarthritis of knee") should be identified.
#' @param exclude  Character vector of words
#' to identify concepts to exclude.
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param conceptClassId Character vector with one or more concept class
#' of the Concept
#' @param doseForm The dose form associated with a drug
#' @param vocabularyId Character vector with one or more vocabulary
#' of the Concept
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchViaSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.
#' @param fuzzyMatch Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param maxDistanceCost, The
#' maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.

#'
#' @return tibble
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' cdm <- CodelistGenerator::mockVocabRef()
#' CodelistGenerator::getCandidateCodes(
#'   cdm = cdm,
#'   keywords = "osteoarthritis"
#'  )
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getCandidateCodes <- function(cdm = NULL,
                              keywords,
                              exclude = NULL,
                              domains = "Condition",
                              conceptClassId = NULL,
                              doseForm = NULL,
                              vocabularyId = NULL,
                              standardConcept = "Standard",
                              searchInSynonyms = FALSE,
                              searchViaSynonyms = FALSE,
                              searchNonStandard = FALSE,
                              includeDescendants = TRUE,
                              includeAncestor = FALSE,
                              fuzzyMatch = FALSE,
                              maxDistanceCost = 0.1,
                              verbose = FALSE) {
  if (verbose == TRUE) {
    # to report time taken at the end
    start <- Sys.time()

    # summary of search strategy
    message(glue::glue("Search strategy"))
    message(glue::glue("-- keywords: {toString(keywords)}"))
    message(glue::glue("-- domains: {toString(domains)}"))
    message(glue::glue("-- conceptClassId: {toString(conceptClassId)}"))
    message(glue::glue("-- vocabularyId: {toString(vocabularyId)}"))
    message(glue::glue("-- standardConcept: {toString(standardConcept)}"))
    message(glue::glue("-- searchInSynonyms: {toString(searchInSynonyms)}"))
    message(glue::glue("-- searchViaSynonyms: {toString(searchViaSynonyms)}"))
    message(glue::glue("-- searchNonStandard: {toString(searchNonStandard)}"))
    message(glue::glue("-- fuzzyMatch: {toString(fuzzyMatch)}"))
    message(glue::glue("-- maxDistanceCost: {toString(maxDistanceCost)}"))
    message(glue::glue("-- exclude: {toString(exclude)}"))
    message(glue::glue("-- includeDescendants: {toString(includeDescendants)}"))
    message(glue::glue("-- includeAncestor: {toString(includeAncestor)}"))

    # now we´ll start checking the inputs
    message("Checking inputs")
  }

  ## checks for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertVector(keywords, add = errorMessage)
  checkmate::assertVector(exclude,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertVector(domains, add = errorMessage)
  checkmate::assertVector(conceptClassId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertCharacter(doseForm, add = errorMessage,
                             null.ok = TRUE)
  checkmate::assertVector(vocabularyId,
                          add = errorMessage,
                          null.ok = TRUE
  )
  checkmate::assertVector(standardConcept, add = errorMessage)
  standardConceptCheck <- all(tolower(standardConcept) %in%
    c(
      "standard",
      "classification",
      "non-standard"
    ))
  if (!isTRUE(standardConceptCheck)) {
    errorMessage$push(
      "- standardConcept should be one or more of Standard, Non-stanadard, or Classification"
    )
  }
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
  checkmate::assert_logical(searchInSynonyms, add = errorMessage)
  checkmate::assert_logical(searchViaSynonyms, add = errorMessage)
  checkmate::assert_logical(searchNonStandard, add = errorMessage)
  checkmate::assert_logical(includeDescendants, add = errorMessage)
  checkmate::assert_logical(includeAncestor, add = errorMessage)
  checkmate::assert_logical(fuzzyMatch, add = errorMessage)
  checkmate::assert_numeric(maxDistanceCost, add = errorMessage)
  checkmate::assert_logical(verbose, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)
  # in addition, now will check we have the required tables
  errorMessage <- checkmate::makeAssertCollection()
  checkTableExists(cdm, "concept", errorMessage)
  checkTableExists(cdm, "concept_relationship", errorMessage)
  checkTableExists(cdm, "concept_ancestor", errorMessage)
  checkTableExists(cdm, "concept_synonym", errorMessage)
  checkTableExists(cdm, "vocabulary", errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


  if (verbose == TRUE) {
    message("Starting search")
  }

  # run search by domain
  searchSpecs <- data.frame(
    id = seq_along(domains),
    domain = domains
  )
  searchSpecs <- split(
    searchSpecs,
    searchSpecs[, c("id")]
  )

  searchResults <- lapply(searchSpecs, function(x) {
    result <- runSearch(keywords,
      cdm = cdm,
      exclude = exclude,
      domains = x$domain,
      conceptClassId = conceptClassId,
      doseForm = doseForm,
      vocabularyId = vocabularyId,
      standardConcept = standardConcept,
      searchInSynonyms = searchInSynonyms,
      searchViaSynonyms = searchViaSynonyms,
      searchNonStandard = searchNonStandard,
      fuzzyMatch = fuzzyMatch,
      maxDistanceCost = maxDistanceCost,
      includeDescendants = includeDescendants,
      includeAncestor = includeAncestor,
      verbose = verbose
    )

    return(result)
  })

  # drop any empty tibbles
  searchResults <- searchResults[lapply(searchResults, nrow) > 0]

  # put the results from each domain together
  searchResults <- dplyr::bind_rows(searchResults,
    .id = NULL
  ) %>%
    dplyr::distinct()

  if (nrow(searchResults) == 0) {
    message(glue::glue("-- No codes found for given search strategy"))
  }

  # return results
  if (verbose == TRUE) {
    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    message(glue::glue(
      "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    ))
  }

  return(searchResults)
}
