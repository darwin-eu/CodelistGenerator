# Copyright 2023 DARWIN EU®
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
<<<<<<< HEAD
#' @param vocref A "VocabReference" object containing references to vocab tables
#' created using `vocabRefFromDatabase` or `vocabRefFromFiles`
=======
#' @param cdm cdm_reference via CDMConnector
>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
#' @param keywords Character vector of words to search for.
#' Where more than one word is given (e.g. "knee osteoarthritis"),
#' all combinations of those words should be identified
#' positions (e.g. "osteoarthritis of knee") should be identified.
<<<<<<< HEAD
#' @param exclude Character vector of words to identify concepts to exclude.
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param conceptClassId Character vector with one or more concept class of the Concept
=======
#' @param exclude  Character vector of words
#' to identify concepts to exclude.
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param conceptClassId Character vector with one or more concept class
#' of the Concept
#' @param doseForm The dose form associated with a drug
#' @param vocabularyId Character vector with one or more vocabulary
#' of the Concept
>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param exactMatch Either TRUE or FALSE. If TRUE only exact matches of
#' keywords will be identified when running the initial search.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchViaSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
<<<<<<< HEAD
#' @param fuzzyMatch Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param maxDistanceCost The maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
=======
#' @param includeSequela Either TRUE or FALSE. If TRUE, codes associated via
#' a concept relationship of 'Due to of' or 'Occurs before' will also be
#' identified.
>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
<<<<<<< HEAD
#' will be included in the candidate codelist.
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
=======
#'  will be included in the candidate codelist.
#' @param fuzzyMatch Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param maxDistanceCost, The
#' maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.

>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
#'
#' @return tibble
#' @importFrom rlang .data
#' @export
#'
#' @examples
<<<<<<< HEAD
#' \dontrun{
#' library(CodelistGenerator)
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                        dbname = "cdm",
#'                        host = "localhost",
#'                        user = "postgres",
#'                        password = Sys.getenv("PASSWORD"))
#'
#' vocab <- vocabRefFromDatabase(con, schema = "synthea1k")
#'
#' getCandidateCodes(vocab, keywords = "asthma")
#'
#' #' library(CodelistGenerator)
#' con <- DBI::dbConnect(RPostgres::Postgres(),
#'                        dbname = "cdm",
#'                        host = "localhost",
#'                        user = "postgres",
#'                        password = Sys.getenv("PASSWORD"))
#'
#' downloadVocab(con, "vocabSchema", dirOut = here::here("vocab"))
#'
#' vocab <- vocabRefFromFiles(here::here("vocab"))
#'
#' getCandidateCodes(vocab, keywords = "asthma")
#' }
getCandidateCodes <- function(vocref,
=======
#' cdm <- CodelistGenerator::mockVocabRef()
#' CodelistGenerator::getCandidateCodes(
#'   cdm = cdm,
#'   keywords = "osteoarthritis"
#'  )
#' DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
getCandidateCodes <- function(cdm,
>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
                              keywords,
                              exclude = NULL,
                              domains = "Condition",
                              conceptClassId = NULL,
                              doseForm = NULL,
                              vocabularyId = NULL,
                              standardConcept = "Standard",
                              exactMatch = FALSE,
                              searchInSynonyms = FALSE,
                              searchViaSynonyms = FALSE,
                              searchNonStandard = FALSE,
                              includeSequela = FALSE,
                              includeDescendants = TRUE,
                              includeAncestor = FALSE,
                              fuzzyMatch = FALSE,
                              maxDistanceCost = 0.1,
                              verbose = FALSE) {
  if (verbose == TRUE) {
    # to report time taken at the end
    start <- Sys.time()
<<<<<<< HEAD
    args <- rlang::fn_fmls_syms()
    m <- purrr::map2_chr(names(args), args, ~glue::glue("-- {.x}: {toString(eval(.y))}"))
    message(paste(m, collapse = "\n"))
    message("Checking inputs")
  }

  # TODO Discuss how strict should we be about input format for options?
  # Should we just be case insensitive (i.e. everything is mapped to lower case)?

  ## domains and standardConcept to sentence case
  domains <- tolower(domains)
  # standardConcept <- stringr::str_to_sentence(standardConcept)

  if (!is.null(conceptClassId)) {
    conceptClassId <- tolower(conceptClassId)
  }

  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(keywords, add = errorMessage)
  checkmate::assertCharacter(conceptClassId, add = errorMessage, null.ok = TRUE)
  checkmate::assertSubset(domains,
    choices = c("Condition", "Measurement", "Procedure", "Observation", "Device", "Drug"),
    add = errorMessage
  )
  checkmate::assertChoice(standardConcept,
    choices = c("Standard", "Classification", "Non-standard"),
    add = errorMessage)
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
  checkmate::assertLogical(searchSynonyms, add = errorMessage)
  checkmate::assertLogical(searchNonStandard, add = errorMessage)
  checkmate::assertLogical(fuzzyMatch, add = errorMessage)
  checkmate::assert_numeric(maxDistanceCost, add = errorMessage)
  checkmate::assertCharacter(exclude, null.ok = TRUE, add = errorMessage)
  checkmate::assertLogical(includeDescendants, add = errorMessage)
  checkmate::assertLogical(includeAncestor, add = errorMessage)
  checkmate::assertLogical(verbose, add = errorMessage)
  checkmate::assertClass(vocabReference, "VocabReference", add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)

  # check domains in Concept table
  local({
    inConceptTable <- vocref$concept %>%
      dplyr::distinct(.data$domain_id) %>%
      dplyr::pull() %>%
      tolower()

    # TODO discuss how to handle upper/lower cases
    # TODO discuss when to trigger an error vs warning vs message

    notInConceptTable <- dplyr::setdiff(domains, inConceptTable)
    if (length(notInConceptTable) > 0) {
      s <- ifelse(length(notInConceptTable) > 1, "s", "")
      d <- paste(notInConceptTable, collapse = ', ')
      errorMessage$push(glue::glue("domain_id{s} {d} not found in concept table"))
    }
  })

  # check conceptClassId in Concept table
  # This is almost the same as the code above and could be made into a function
  local({
    inConceptTable <-  vocref$concept %>%
      dplyr::distinct(.data$concept_class_id) %>%
      dplyr::pull() %>%
      tolower()

    notInConceptTable <- dplyr::setdiff(conceptClassId, inConceptTable)
    if (length(notInConceptTable) > 0) {
      s <- ifelse(length(notInConceptTable) > 1, "s", "")
      d <- paste(notInConceptTable, collapse = ', ')
      errorMessage$push(glue::glue("concept_class_id{s} {d} not found in concept table"))
    }
  })

  checkmate::reportAssertions(collection = errorMessage)


  # filter vocab tables to keep only relevant data
  if (verbose == TRUE) {
    message("Limiting to potential concepts of interest")
  }

  conceptDb <- conceptDb %>%
    dplyr::mutate(concept_class_id = tolower(.data$concept_class_id)) %>%
    dplyr::mutate(domain_id = tolower(.data$domain_id)) %>%
    dplyr::filter(.data$domain_id %in% .env$domains)

  if (!is.null(conceptClassId)) {
    # first, check some combination exists
    # return error if not
    errorMessage <- checkmate::makeAssertCollection()
    combCheck <- conceptDb %>%
      dplyr::mutate(concept_class_id = tolower(.data$concept_class_id)) %>%
      dplyr::group_by(
        .data$domain_id,
        .data$concept_class_id,
        .data$standard_concept
      ) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$domain_id %in% .env$domains) %>%
      dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
      dplyr::filter(.data$concept_class_id %in% .env$conceptClassId)
    checkmate::assertTRUE(nrow(combCheck %>% dplyr::collect()) > 0,
      add = errorMessage
    )
    if (!isTRUE(nrow(combCheck %>% dplyr::collect()) > 0)) {
      errorMessage$push(
        glue::glue("- No combination of domains, standardConcept, and conceptClassId found in concept table")
      )
    }
    checkmate::reportAssertions(collection = errorMessage)
    # now filter
    conceptDb <- conceptDb %>%
      dplyr::filter(.data$concept_class_id %in% .env$conceptClassId)
  }
  concept <- conceptDb %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  conceptAncestorDb <- conceptAncestorDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::rename("ancestor_concept_id" = "concept_id") %>%
      dplyr::select("ancestor_concept_id", "domain_id", "standard_concept"),
    by = "ancestor_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% .env$domains) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::compute()

  conceptAncestor <- conceptAncestorDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::rename("descendant_concept_id" = "concept_id") %>%
      dplyr::select("descendant_concept_id", "domain_id", "standard_concept"),
    by = "descendant_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% .env$domains) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  # will only collect conceptSynonym later if needed
  conceptSynonymDb <- conceptSynonymDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::select("concept_id", "domain_id", "standard_concept"),
    by = "concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% .env$domains) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept"))

  # Start finding candidate codes
  # 1) first, get codes to exclude
  # and anti_join throughought to make sure these don't appear
  # exact matches only for codes to exclude (no fuzzy option)

  if (length(exclude) > 0) {
    if (verbose == TRUE) {
      message("Getting concepts to exclude")
    }
    # Get standard, condition concepts which include one of the exclusion words
    # always use exact matching
    excludeCodes <- getExactMatches(
      words = tidyWords(exclude),
      conceptDf = concept
    )
  }

  # 2) Get standard, condition concepts which
  # include one of the keywords
  # note, fuzzy match will also get all exact matches
  # so run exact only if fuzzy = FALSE

  if (verbose == TRUE) {
    message("Getting concepts to include")
  }

  # 2a) on exact match
  if (fuzzyMatch == FALSE) {
    candidateCodes <- getExactMatches(
      words = tidyWords(keywords),
      conceptDf = concept
    )
  }

  # 2b) or using fuzzy match
  if (fuzzyMatch == TRUE) {
    candidateCodes <- getFuzzyMatches(
      words = tidyWords(keywords),
      conceptDf = concept,
      mdCost = maxDistanceCost
    )

    candidateCodes <- candidateCodes %>%
      dplyr::distinct()
  }

  candidateCodes <- candidateCodes %>%
    dplyr::mutate(found_from = "From initial search")

  # run exclusion
  if (length(exclude) > 0) {
    if (nrow(excludeCodes) > 0) {
      candidateCodes <- candidateCodes %>%
        dplyr::anti_join(excludeCodes %>%
          dplyr::select("concept_id"),
        by = "concept_id"
        )
    }
  }

  if (nrow(candidateCodes) > 0) {

    # 3) look for any standard, condition concepts with a synonym of the
    # codes found from the keywords
    if (searchSynonyms == TRUE & verbose == TRUE) {
      message("Getting concepts to include from exact matches of synonyms")
    }

    if (searchSynonyms == TRUE) {
      conceptSynonym <- conceptSynonymDb %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower)

      synonyms <- conceptSynonym %>%
        dplyr::inner_join(candidateCodes %>%
          dplyr::select("concept_id"),
        by = "concept_id"
        ) %>%
        dplyr::select("concept_synonym_name") %>%
        dplyr::distinct() %>%
        dplyr::pull()

      # drop any long synonyms (more than 6 words)
      # looking for these adds a lot of run time
      # while being highly unlikely to have a match
      synonyms <- synonyms[stringr::str_count(synonyms, "\\S+") <= 6]
      synonyms <- unique(tidyWords(synonyms))
      # more than one character
      synonyms <- synonyms[stringr::str_count(synonyms) > 1]

      if (fuzzyMatch == FALSE) {
        synonymCodes <- getExactMatches(
          words = synonyms,
          conceptDf = concept
        )
      }

      if (fuzzyMatch == TRUE) {
        synonymCodes <- getFuzzyMatches(
          words = tidyWords(synonyms),
          conceptDf = concept,
          mdCost = maxDistanceCost
        )
      }

      candidateCodes <- dplyr::bind_rows(
        candidateCodes,
        synonymCodes %>%
          dplyr::mutate(found_from = "From synonyms")
      ) %>%
        dplyr::distinct()
    }

    if (searchSynonyms == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(excludeCodes) > 0) {
          candidateCodes <- candidateCodes %>%
            dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }


    # 4) add any codes lower in the hierachy
    if (includeDescendants == TRUE & verbose == TRUE) {
      message("Getting concepts to include from descendants")
    }

    if (includeDescendants == TRUE) {
      candidateCodeDescendants <- addDescendants(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestor,
        conceptDf = concept
      )

      candidateCodes <- dplyr::bind_rows(
        candidateCodes,
        candidateCodeDescendants %>%
          dplyr::mutate(found_from = "From descendants")
      ) %>%
        dplyr::distinct()
    }

    if (includeDescendants == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(excludeCodes) > 0) {
          candidateCodes <- candidateCodes %>%
            dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }

    # 5) add any codes one level above in the hierachy
    if (includeAncestor == TRUE & verbose == TRUE) {
      message("Getting concepts to include from direct ancestors of identified concepts")
    }

    if (includeAncestor == TRUE) {
      candidateCodeAncestor <- addAncestor(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestor,
        conceptDf = concept
      )

      candidateCodes <- dplyr::bind_rows(
        candidateCodes,
        candidateCodeAncestor %>%
          dplyr::mutate(found_from = "From ancestor")
      ) %>%
        dplyr::distinct()
    }

    if (includeAncestor == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(excludeCodes) > 0) {
          candidateCodes <- candidateCodes %>%
            dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }
  }


  # 6) add codes from non-standard
  # nb we do this last so as to not include descendants
  # which can blow up candiate codelist when there
  # are multiple mappings
  if (searchNonStandard == TRUE & verbose == TRUE) {
    message("Getting concepts to include from non-standard concepts")
  }

  if (searchNonStandard == TRUE) {
    conceptNs <- conceptDb %>%
      dplyr::filter(.data$domain_id %in% .env$domains) %>%
      dplyr::filter(.data$standard_concept == "Non-standard") %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)

    if (fuzzyMatch == FALSE & nrow(conceptNs) > 0) {
      candidateCodesNs <- getExactMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs
      )
    }

    if (fuzzyMatch == TRUE & nrow(conceptNs) > 0) {
      candidateCodesNs <- getFuzzyMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs,
        mdCost = maxDistanceCost
      )
    }

    if (nrow(conceptNs) > 0) {
      candidateCodesNs <- candidateCodesNs %>%
        dplyr::select("concept_id") %>%
        dplyr::inner_join(conceptRelationshipDb %>%
          dplyr::filter(.data$relationship_id == "Mapped from") %>%
          dplyr::collect() %>%
          dplyr::rename_with(tolower),
        by = c("concept_id" = "concept_id_2")
        ) %>%
        dplyr::select("concept_id_1") %>%
        dplyr::rename("concept_id" = "concept_id_1") %>%
        dplyr::distinct() %>%
        dplyr::inner_join(concept,
          by = "concept_id"
        ) %>%
        dplyr::mutate(concept_name = tidyWords(.data$concept_name))

      candidateCodes <- dplyr::bind_rows(
        candidateCodes,
        candidateCodesNs %>%
          dplyr::mutate(found_from = "From non-standard")
      ) %>%
        dplyr::distinct()
    }
  }

  if (searchNonStandard == TRUE) {
    if (length(exclude) > 0) {
      if (nrow(excludeCodes) > 0) {
        candidateCodes <- candidateCodes %>%
          dplyr::anti_join(excludeCodes %>% dplyr::select("concept_id"),
            by = "concept_id"
          )
      }
    }
  }


  if (nrow(candidateCodes) == 0) {
    candidateCodes
    message("-- No codes found for given keywords")
  } else {

    # 7) Finish up
    # get original names back
    candidateCodes <- candidateCodes %>%
      dplyr::select(.data$concept_id, .data$found_from) %>%
      dplyr::inner_join(concept,
        by = c("concept_id")
      ) %>%
      dplyr::distinct()

    candidateCodes <- candidateCodes %>%
      dplyr::select(
        "concept_id", "concept_name",
        "domain_id", "concept_class_id",
        "vocabulary_id", "found_from"
      )

    if (verbose == TRUE) {
      duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
      message(glue::glue(
        "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
      ))
    }

    candidateCodes <- candidateCodes %>%
      dplyr::distinct()

    # remove duplicates (found in different ways)
    # keep first time it was found
    candidateCodes <- candidateCodes %>%
      dplyr::group_by(.data$concept_id) %>%
      dplyr::mutate(seq = dplyr::row_number(.data$concept_id)) %>%
      dplyr::filter(seq == 1) %>%
      dplyr::select(-"seq")

    return(candidateCodes)
=======

    # summary of search strategy
    message(glue::glue("Search strategy"))
    message(glue::glue("-- keywords: {toString(keywords)}"))
    message(glue::glue("-- domains: {toString(domains)}"))
    message(glue::glue("-- conceptClassId: {toString(conceptClassId)}"))
    message(glue::glue("-- vocabularyId: {toString(vocabularyId)}"))
    message(glue::glue("-- exactMatch: {toString(exactMatch)}"))
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
      "- standardConcept must be from Standard, Non-stanadard, or Classification"
    )
  }
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)
  checkmate::assert_logical(exactMatch, add = errorMessage)
  checkmate::assert_logical(searchInSynonyms, add = errorMessage)
  checkmate::assert_logical(searchViaSynonyms, add = errorMessage)
  checkmate::assert_logical(searchNonStandard, add = errorMessage)
  checkmate::assert_logical(includeDescendants, add = errorMessage)
  checkmate::assert_logical(includeAncestor, add = errorMessage)
  checkmate::assert_logical(fuzzyMatch, add = errorMessage)
  checkmate::assert_numeric(maxDistanceCost, add = errorMessage)
  checkmate::assert_logical(verbose, add = errorMessage)
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
  if(exactMatch == TRUE) {
    checkmate::assert_false(fuzzyMatch, add = errorMessage)
  if (!isFALSE(fuzzyMatch)) {
    errorMessage$push(
      "- fuzzyMatch must be FALSE if exactMatch is TRUE"
    )
>>>>>>> d27aaf5662aedd5f33b203d9f884d232fd1290b2
  }
}
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
      exactMatch = exactMatch,
      searchInSynonyms = searchInSynonyms,
      searchViaSynonyms = searchViaSynonyms,
      searchNonStandard = searchNonStandard,
      includeSequela = includeSequela,
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


#' Helper function to prepare words for search
#' @param words A character vector.
#' @noRd
formatWords <- function(words) {
  checkmate::assertCharacter(words)

  # to avoid invalid UTF-8 error
  # TODO why is this needed? What if we want to include UTF8 characters?
  Encoding(words) <- "latin1"

  # some generic formatting
  trimws(words) %>%
    stringr::str_replace_all(workingWords, "-", " ") %>%
    stringr::str_remove_all(workingWords, "[[:punct:]]") %>%
    stringr::str_remove_all(workingWords, "[^[\\da-zA-Z ]]") %>%
    stringr::str_remove_all(workingWords, "[^\x01-\x7F]+") %>%
    stringr::str_to_lower(workingWords) %>%
    trimws(workingWords)
}
