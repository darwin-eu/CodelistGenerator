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

runSearch <- function(keywords,
                      cdm,
                      exclude,
                      domains,
                      conceptClassId,
                      doseForm,
                      vocabularyId,
                      standardConcept,
                      searchInSynonyms,
                      searchViaSynonyms,
                      searchNonStandard,
                      fuzzyMatch,
                      maxDistanceCost,
                      includeDescendants,
                      includeAncestor,
                      verbose) {
  # connect to relevant vocabulary tables
  # will return informative error if not found
  conceptDb <- cdm$concept
  conceptAncestorDb <- cdm$concept_ancestor
  conceptSynonymDb <- cdm$concept_synonym
  conceptRelationshipDb <- cdm$concept_relationship
  drugStrengthDb <- cdm$drug_strength


  # formatting of conceptDb variables
  conceptDb <- conceptDb %>%
    dplyr::mutate(domain_id = tolower(.data$domain_id)) %>%
    dplyr::mutate(vocabulary_id = tolower(.data$vocabulary_id)) %>%
    dplyr::mutate(concept_class_id = tolower(.data$concept_class_id)) %>%
    dplyr::mutate(
      standard_concept = ifelse(is.na(.data$standard_concept),
        "non-standard", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "C",
        "classification", .data$standard_concept
      )
    ) %>%
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "S",
        "standard", .data$standard_concept
      )
    )

  ## domains, standardConcept vocab, and conceptClassId to lower
  domains <- tolower(domains)
  standardConcept <- tolower(standardConcept)
  if (!is.null(vocabularyId)) {
    vocabularyId <- tolower(vocabularyId)
  }
  if (!is.null(conceptClassId)) {
    conceptClassId <- tolower(conceptClassId)
  }

  # new name for clarity
  standardConceptFlags <- standardConcept

  # message if user inputs are not in vocab tables

  # filter vocab tables to keep only relevant data
  if (verbose == TRUE) {
    message(glue::glue("{domains}: Limiting to potential concepts of interest"))
  }

  conceptDb <- conceptDb %>%
    dplyr::filter(.data$domain_id %in% .env$domains)
  if (!is.null(vocabularyId)) {
    # now filter
    conceptDb <- conceptDb %>%
      dplyr::filter(.data$vocabulary_id %in% .env$vocabularyId)
  }

  concept <- conceptDb %>%
    dplyr::mutate(standard_concept = tolower(.data$standard_concept)) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  conceptAncestorDb <- conceptAncestorDb %>%
    dplyr::left_join(
      conceptDb %>%
        dplyr::rename("ancestor_concept_id" = "concept_id") %>%
        dplyr::select("ancestor_concept_id", "domain_id", "standard_concept"),
      by = "ancestor_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% .env$domains) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept"))

  conceptAncestor <- conceptAncestorDb %>%
    dplyr::left_join(
      conceptDb %>%
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
    dplyr::left_join(
      conceptDb %>%
        dplyr::select("concept_id", "domain_id", "standard_concept"),
      by = "concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% .env$domains) %>%
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept"))

  if (searchInSynonyms == TRUE ||
    searchViaSynonyms == TRUE) {
    conceptSynonym <- conceptSynonymDb %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)
  }

  # collect the drug_strength table if drug
  if (domains == "drug") {
    drugStrength <- drugStrengthDb %>%
      dplyr::left_join(
        conceptDb %>%
          dplyr::rename("drug_concept_id" = "concept_id") %>%
          dplyr::select("drug_concept_id", "domain_id", "standard_concept"),
        by = "drug_concept_id"
      ) %>%
      dplyr::filter(.data$domain_id %in% .env$domains) %>%
      dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)
  }


  candidateCodesList <- list()


  workingConcept <- concept %>%
    dplyr::filter(.data$domain_id == .env$domains)

  if (exists("conceptSynonym") == TRUE) {
    workingconceptSynonym <- conceptSynonym %>%
      dplyr::left_join(
        concept %>%
          dplyr::select("concept_id", "domain_id"),
        by = "concept_id"
      ) %>%
      dplyr::filter(.data$domain_id == .env$domains) %>%
      dplyr::select(!"domain_id")
  }


  # Start finding candidate codes
  # 1) first, get codes to exclude
  # and anti_join throughout to make sure these don't appear
  # exact matches only for codes to exclude (no fuzzy option)

  if (length(exclude) > 0) {
    if (verbose == TRUE) {
      message(glue::glue("{domains}: Getting concepts to exclude"))
    }
    # Get standard, condition concepts which include one of the exclusion words
    # always use exact matching
    excludeCodes <- getExactMatches(
      words = tidyWords(exclude),
      conceptDf = workingConcept
    )
  }

  # 2) Get standard, condition concepts which
  # include one of the keywords
  # note, fuzzy match will also get all exact matches
  # so run exact only if fuzzy = FALSE

  if (verbose == TRUE) {
    message(glue::glue("{domains}: Getting concepts to include"))
  }

  # 2a) on exact match
  if (fuzzyMatch == FALSE) {
    candidateCodes <- getExactMatches(
      words = tidyWords(keywords),
      conceptDf = workingConcept
    )
  }

  # 2b) or using fuzzy match
  if (fuzzyMatch == TRUE) {
    candidateCodes <- getFuzzyMatches(
      words = tidyWords(keywords),
      conceptDf = workingConcept,
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
        dplyr::anti_join(
          excludeCodes %>%
            dplyr::select("concept_id"),
          by = "concept_id"
        )
    }
  }

  # 3) also search in synonyms if option set to true
  # left join back to concept from workingConcept table
  if (searchInSynonyms == TRUE) {
    if (fuzzyMatch == FALSE) {
      candidateCodesInSynonyms <- getExactMatches(
        words = tidyWords(keywords),
        conceptDf = workingconceptSynonym %>%
          dplyr::rename("concept_name" = "concept_synonym_name")
      ) %>%
        dplyr::select("concept_id") %>%
        dplyr::distinct() %>%
        dplyr::left_join(workingConcept,
          by = "concept_id"
        )
    }

    if (fuzzyMatch == TRUE) {
      candidateCodesInSynonyms <- getFuzzyMatches(
        words = tidyWords(keywords),
        conceptDf = workingconceptSynonym %>%
          dplyr::rename("concept_name" = "concept_synonym_name"),
        mdCost = maxDistanceCost
      ) %>%
        dplyr::select("concept_id") %>%
        dplyr::distinct() %>%
        dplyr::left_join(workingConcept,
          by = "concept_id"
        )
    }

    candidateCodes <- dplyr::bind_rows(
      candidateCodes,
      candidateCodesInSynonyms %>%
        dplyr::mutate(found_from = "In synonyms")
    ) %>%
      dplyr::distinct()
  }

  # run exclusion
  if (length(exclude) > 0) {
    if (nrow(excludeCodes) > 0) {
      candidateCodes <- candidateCodes %>%
        dplyr::anti_join(
          excludeCodes %>%
            dplyr::select("concept_id"),
          by = "concept_id"
        )
    }
  }


  if (nrow(candidateCodes) > 0) {
    # 4) look for any standard, condition concepts with a synonym of the
    # codes found from the keywords
    if (searchViaSynonyms == TRUE && verbose == TRUE) {
      message(glue::glue("{domains}: Getting concepts to include from exact matches of synonyms"))
    }

    if (searchViaSynonyms == TRUE) {
      synonyms <- workingconceptSynonym %>%
        dplyr::inner_join(
          candidateCodes %>%
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
          conceptDf = workingConcept
        )
      }

      if (fuzzyMatch == TRUE) {
        synonymCodes <- getFuzzyMatches(
          words = tidyWords(synonyms),
          conceptDf = workingConcept,
          mdCost = maxDistanceCost
        )
      }

      candidateCodes <- dplyr::bind_rows(
        candidateCodes,
        synonymCodes %>%
          dplyr::mutate(found_from = "Via synonyms")
      ) %>%
        dplyr::distinct()
    }

    if (searchViaSynonyms == TRUE) {
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

  candidateCodesList[[domains]] <- candidateCodes



  candidateCodes <- dplyr::bind_rows(candidateCodesList)

  # 5) add any codes lower in the hierarchy
  if (includeDescendants == TRUE) {
    if (nrow(candidateCodes) > 0) {
      if (verbose == TRUE) {
        message(glue::glue("{domains}: Getting concepts to include from descendants"))
      }

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

      # run exclusion
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

  # 6) add any codes one level above in the hierarchy
  if (includeAncestor == TRUE) {
    if (nrow(candidateCodes) > 0) {
      if (verbose == TRUE) {
        message(glue::glue("{domains}: Getting concepts to include from direct ancestors of identified concepts"))
      }

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

      # run exclusion
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

  # 7) add codes from non-standard
  # nb we do this last so as to not include descendants
  # which can blow up candiate codelist when there
  # are multiple mappings
  if (searchNonStandard == TRUE) {
    if (verbose == TRUE) {
      message(glue::glue("{domains}: Getting concepts to include from non-standard concepts"))
    }

    conceptNs <- conceptDb %>%
      dplyr::filter(.data$domain_id %in% .env$domains) %>%
      dplyr::filter(.data$standard_concept == "non-standard") %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)

    if (fuzzyMatch == FALSE && nrow(conceptNs) > 0) {
      candidateCodesNs <- getExactMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs
      )
    }

    if (fuzzyMatch == TRUE && nrow(conceptNs) > 0) {
      candidateCodesNs <- getFuzzyMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs,
        mdCost = maxDistanceCost
      )
    }

    if (nrow(conceptNs) > 0) {
      candidateCodesNs <- candidateCodesNs %>%
        dplyr::select("concept_id") %>%
        dplyr::inner_join(
          conceptRelationshipDb %>%
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

    # run exclusion
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
  } else {
    # 8) Finish up
    # get original names back
    candidateCodes <- candidateCodes %>%
      dplyr::select("concept_id", "found_from") %>%
      dplyr::inner_join(concept,
        by = c("concept_id")
      ) %>%
      dplyr::distinct()

    # if domain = "drug", add drug_strength information and dose form

    if (domains == "drug") {
      candidateCodes <- candidateCodes %>%
        dplyr::left_join(
          drugStrength %>%
            dplyr::rename("concept_id" = "drug_concept_id") %>%
            dplyr::mutate("concept_id" = as.integer(.data$concept_id)) %>%
            dplyr::select(
              "concept_id",
              "ingredient_concept_id", "amount_value", "amount_unit_concept_id",
              "numerator_value", "numerator_unit_concept_id", "denominator_value",
              "denominator_unit_concept_id", "box_size"
            ),
          by = "concept_id"
        )

      candidateCodes <- candidateCodes %>%
        dplyr::select(
          "concept_id", "concept_name",
          "domain_id", "concept_class_id",
          "vocabulary_id", "found_from",
          "ingredient_concept_id", "amount_value", "amount_unit_concept_id",
          "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size"
        )

      drugConceptForm <- conceptRelationshipDb %>%
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") %>%
        dplyr::select("concept_id_1", "concept_id_2") %>%
        dplyr::rename("concept_id" = "concept_id_2") %>%
        dplyr::distinct() %>%
        dplyr::left_join(conceptDb, by = "concept_id") %>%
        dplyr::collect() %>%
        dplyr::select("concept_id_1", "concept_id", "concept_name") %>%
        dplyr::rename("dose_form_id" = "concept_id") %>%
        dplyr::rename("dose_form" = "concept_name") %>%
        dplyr::rename("concept_id" = "concept_id_1") %>%
        dplyr::select(!"dose_form_id")
      # can have multiple forms so pivot
      drugConceptForm <- drugConceptForm %>%
        dplyr::group_by(.data$concept_id) %>%
        dplyr::mutate(seq = dplyr::row_number()) %>%
        tidyr::pivot_wider(
          names_from = "seq",
          values_from = "dose_form"
        )
      if (nrow(drugConceptForm) > 0) {
        drugConceptForm <- drugConceptForm %>%
          tidyr::unite(
            col = "dose_form", 2:ncol(drugConceptForm), sep = "; ",
            na.rm = TRUE
          )
        candidateCodes <- candidateCodes %>%
          dplyr::left_join(
            drugConceptForm,
            by = "concept_id"
          )
      }

      if (!is.null(doseForm)) {
        candidateCodes <- candidateCodes %>%
          dplyr::filter(stringr::str_detect(
            string = tolower(.data$dose_form),
            pattern = paste(tolower(.env$doseForm),
              collapse = "|"
            )
          ))
      }
    } else {
      candidateCodes <- candidateCodes %>%
        dplyr::select(
          "concept_id", "concept_name",
          "domain_id", "concept_class_id",
          "vocabulary_id", "found_from"
        )
    }

    if (!is.null(conceptClassId)) {
      candidateCodes <- candidateCodes %>%
        dplyr::filter(.data$concept_class_id %in% .env$conceptClassId)
    }

    candidateCodes <- candidateCodes %>%
      dplyr::distinct()

    # remove duplicates (found in different ways)
    # keep first time it was found
    # for drug,  same concept_id with different ingredient_concept_id will be removed as well.
    candidateCodes <- candidateCodes %>%
      dplyr::group_by(.data$concept_id) %>%
      dplyr::mutate(seq = dplyr::row_number(.data$concept_id)) %>%
      dplyr::filter(seq == 1) %>%
      dplyr::select(-"seq") %>%
      dplyr::ungroup()

    return(candidateCodes)
  }
}


# helper functions for runSearch
tidyWords <- function(words) {
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(words, add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  # to avoid invalid UTF-8 error
  Encoding(words) <- "latin1"

  # some generic formatting
  workingWords <- trimws(words)
  workingWords <- stringr::str_replace_all(workingWords, "-", " ")
  workingWords <- stringr::str_replace_all(workingWords, "[[:punct:]]", "")
  workingWords <- stringr::str_remove_all(workingWords, "[^[\\da-zA-Z ]]")
  workingWords <- stringr::str_remove_all(workingWords, "[^\x01-\x7F]+")
  workingWords <- stringr::str_to_lower(workingWords)
  workingWords <- trimws(workingWords)

  return(workingWords)
}

getExactMatches <- function(words,
                            conceptDf) {
  conceptDf <- conceptDf %>% # start with all
    dplyr::mutate(concept_name = tidyWords(.data$concept_name))

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
    workingConcepts <- conceptDf # start with all

    for (j in seq_along(workingExclude)) {
      workingConcepts <- workingConcepts %>%
        dplyr::filter(stringr::str_detect(
          .data$concept_name,
          .env$workingExclude[j]
        ))
    }
    conceptsFound[[i]] <- workingConcepts
  }
  conceptsFound <- dplyr::bind_rows(conceptsFound) %>% dplyr::distinct()

  return(conceptsFound)
}

getFuzzyMatches <- function(words,
                            conceptDf,
                            mdCost) {
  conceptDf <- conceptDf %>% # start with all
    dplyr::mutate(concept_name = tidyWords(.data$concept_name))

  conceptsFound <- list()
  for (i in seq_along(words)) {
    workingKeywords <- unlist(strsplit(words[i], " "))
    # more than one character
    workingKeywords <- workingKeywords[
      stringr::str_count(workingKeywords) > 1
    ]
    workingConcepts <- conceptDf # start with all
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

  conceptsFound <- dplyr::bind_rows(conceptsFound) %>%
    dplyr::distinct()

  return(conceptsFound)
}

addDescendants <- function(workingCandidateCodes,
                           conceptAncestorDf,
                           conceptDf) {
  candidateCodeDescendants <- workingCandidateCodes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("ancestor_concept_id" = "concept_id") %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      conceptAncestorDf %>%
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

  return(candidateCodeDescendants)
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
    dplyr::anti_join(
      workingCandidateCodes %>%
        dplyr::select("concept_id"),
      by = "concept_id"
    ) %>%
    dplyr::left_join(conceptDf, by = "concept_id")

  return(candidateCodeAncestor)
}
