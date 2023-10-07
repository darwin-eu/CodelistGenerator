# Copyright 2023 DARWIN EU®
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
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.

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
  errorMessage <- checkmate::makeAssertCollection()
  checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  checkmate::assertCharacter(keywords, add = errorMessage)
  checkmate::assertCharacter(exclude,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assertVector(domains, add = errorMessage)
  checkmate::assertVector(standardConcept, add = errorMessage)
  standardConceptCheck <- all(tolower(standardConcept) %in%
    c(
      "standard",
      "classification",
      "non-standard"
    ))
  if (!isTRUE(standardConceptCheck)) {
    errorMessage$push(
      "- standardConcept must be from Standard, Non-standard, or Classification"
    )
  }
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
  checkmate::assert_logical(searchInSynonyms, add = errorMessage)
  checkmate::assert_logical(searchNonStandard, add = errorMessage)
  checkmate::assert_logical(includeDescendants, add = errorMessage)
  checkmate::assert_logical(includeAncestor, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()
  assertTablesExist(cdm, tableName = c("concept",
                                       "concept_relationship",
                                       "concept_ancestor",
                                       "concept_synonym",
                                       "vocabulary"),
                    messageStore = errorMessage)
  if ("drug" %in% tolower(domains)) {
    assertTablesExist(cdm, tableName = c("drug_strength"),
                      messageStore = errorMessage)
  }
  checkmate::reportAssertions(collection = errorMessage)

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::reportAssertions(collection = errorMessage)

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
      standardConcept = standardConcept,
      searchInSynonyms = searchInSynonyms,
      searchNonStandard = searchNonStandard,
      includeDescendants = includeDescendants,
      includeAncestor = includeAncestor
    )

    return(result)
  })

  # drop any empty tibbles and put results from each domain together
  searchResults <- searchResults[lapply(searchResults, nrow) > 0]
  searchResults <- dplyr::bind_rows(searchResults,
    .id = NULL
  ) %>%
    dplyr::distinct()

  if (nrow(searchResults) == 0) {
    cli::cli_inform("No codes found for the given search strategy")
  } else {
    cli::cli_alert_success(
      "{nrow(searchResults)} candidate concept{?s} identified"
    )
  }

    duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
    cli::cli_inform(
      "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
    )

  return(searchResults)
}
