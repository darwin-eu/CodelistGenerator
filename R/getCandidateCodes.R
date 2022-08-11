# Copyright 2022 DARWIN EU (C)
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
#' @param vocref A "VocabReference" object containing references to vocab tables
#' created using `vocabRefFromDatabase` or `vocabRefFromFiles`
#' @param keywords Character vector of words to search for.
#' Where more than one word is given (e.g. "knee osteoarthritis"),
#' all combinations of those words should be identified
#' positions (e.g. "osteoarthritis of knee") should be identified.
#' @param exclude Character vector of words to identify concepts to exclude.
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param conceptClassId Character vector with one or more concept class of the Concept
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param fuzzyMatch Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param maxDistanceCost The maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#' will be included in the candidate codelist.
#' @param verbose Either TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return tibble
#' @importFrom rlang .data
#' @export
#'
#' @examples
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
                              keywords,
                              exclude = NULL,
                              domains = "Condition",
                              conceptClassId = NULL,
                              standardConcept = "Standard",
                              searchSynonyms = FALSE,
                              searchNonStandard = FALSE,
                              fuzzyMatch = FALSE,
                              maxDistanceCost = 0.1,
                              includeDescendants = TRUE,
                              includeAncestor = FALSE,
                              verbose = FALSE) {

  if (verbose == TRUE) {
    # to report time taken at the end
    start <- Sys.time()
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
  }
}


# helper functions for main getCandidateCodes function
getExactMatches <- function(words,
                            conceptDf) {

  # because there may be a lot of synonyms, get these from a loop
  # (stringr::str_detect slows considerably
  # as more options are added in a single call using "|")

  # note, where one term is multiple words (e.g "knee osteoarthritis"),
  # split up and search
  # so that they don´t need to be next to each other
  # (e.g. to find "osteoarthritis of knee"))

  conceptsFound <- list()
  for (i in seq_along(words)) {
    workingExclude <- unlist(strsplit(words[i], " "))
    workingConcepts <- conceptDf %>% # start with all
      dplyr::mutate(concept_name = tidyWords(.data$concept_name))

    conceptsFound[[i]] <- workingConcepts %>%
      dplyr::filter(apply(sapply(
        X = workingExclude,
        FUN = grepl, workingConcepts$concept_name
      ),
      MARGIN = 1, FUN = all
      )) %>%
      dplyr::distinct()
  }
  dplyr::bind_rows(conceptsFound)
}

getFuzzyMatches <- function(words,
                            conceptDf,
                            mdCost) {
  conceptsFound <- list()
  for (i in seq_along(words)) {
    workingKeywords <- unlist(strsplit(words[i], " "))
    # more than one character
    workingKeywords <- workingKeywords[
      stringr::str_count(workingKeywords) > 1
    ]
    workingConcepts <- conceptDf %>% # start with all
      dplyr::mutate(concept_name = tidyWords(.data$concept_name))
    for (j in seq_along(workingKeywords)) {
      # filter each term within the loop, one after the other
      indx <- agrep(workingKeywords[j], workingConcepts$concept_name,
        max.distance = list(
          cost = mdCost
        )
      )
      workingConcepts <- workingConcepts[indx, ]
    }

    conceptsFound[[i]] <- workingConcepts
  }

  dplyr::bind_rows(conceptsFound) %>%
    dplyr::distinct()
}

addDescendants <- function(workingCandidateCodes,
                           conceptAncestorDf,
                           conceptDf) {
  candidateCodeDescendants <- workingCandidateCodes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("ancestor_concept_id" = "concept_id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(conceptAncestorDf %>%
      dplyr::filter("ancestor_concept_id" != "descendant_concept_id"),
    by = "ancestor_concept_id"
    ) %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id")

  candidateCodeDescendants <-
    candidateCodeDescendants %>%
    dplyr::left_join(conceptDf, by = "concept_id") %>%
    dplyr::mutate(concept_name = tidyWords(.data$concept_name))

  candidateCodeDescendants
}

addAncestor <- function(workingCandidateCodes,
                        conceptAncestorDf,
                        conceptDf) {
  candidateCodeAncestor <- workingCandidateCodes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("descendant_concept_id" = "concept_id") %>%
    dplyr::left_join(conceptAncestorDf,
      by = "descendant_concept_id"
    ) %>%
    dplyr::filter(.data$min_levels_of_separation == "1") %>%
    dplyr::select("ancestor_concept_id") %>%
    dplyr::rename("concept_id" = "ancestor_concept_id") %>%
    dplyr::left_join(conceptDf,
      by = "concept_id"
    ) %>%
    dplyr::mutate(concept_name = tidyWords(.data$concept_name))

  # keep if not already in candidateCodes
  candidateCodeAncestor <- candidateCodeAncestor %>%
    dplyr::anti_join(workingCandidateCodes %>%
      dplyr::select("concept_id"),
    by = "concept_id"
    ) %>%
    dplyr::left_join(conceptDf, by = "concept_id")

  return(candidateCodeAncestor)
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
