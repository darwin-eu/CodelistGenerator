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

runSearch <- function(keywords,
                      cdm,
                      exclude,
                      domains,
                      standardConcept,
                      searchInSynonyms,
                      searchNonStandard,
                      includeDescendants,
                      includeAncestor) {

  if(!is.null(attr(cdm, "dbcon"))){
    prefix <- paste0(sample(letters, 5, TRUE),
                     collapse = "")
  }

  # connect to relevant vocabulary tables
  # will return informative error if not found
  conceptDb <- cdm$concept
  conceptAncestorDb <- cdm$concept_ancestor
  conceptSynonymDb <- cdm$concept_synonym
  conceptRelationshipDb <- cdm$concept_relationship
  drugStrengthDb <- cdm$drug_strength

  ## domains, standardConcept vocab to lower
  domains <- tolower(domains)
  standardConcept <- tolower(standardConcept)
  # new name for clarity
  standardConceptFlags <- standardConcept

  # formatting of conceptDb variables
  conceptDb <- conceptDb |>
    dplyr::mutate(
      domain_id = tolower(.data$domain_id),
      standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard",
        .default = as.character(.data$standard_concept)
      )
    )

  cli::cli_inform("Limiting to domains of interest")
  concept <- conceptDb |>
    dplyr::filter(.data$standard_concept %in% .env$standardConceptFlags,
                  .data$domain_id %in% .env$domains) |>
    dplyr::compute()

  # will only collect conceptSynonym later if needed
  if (searchInSynonyms) {
    conceptSynonymDb <- conceptSynonymDb |>
      dplyr::left_join(
        conceptDb |>
          dplyr::select("concept_id", "domain_id", "standard_concept"),
        by = "concept_id"
      )

    conceptSynonymDb <- conceptSynonymDb |>
      dplyr::filter(.data$domain_id %in% .env$domains &
                      .data$standard_concept %in% .env$standardConceptFlags) |>
      dplyr::select(-c("domain_id", "standard_concept"))

    conceptSynonym <- conceptSynonymDb |>
      dplyr::rename_with(tolower)
  } else {
    conceptSynonym <- NULL
  }

  # collect the drug_strength table if drug
  if ("drug" %in% domains) {
    drugStrength <- drugStrengthDb |>
      dplyr::left_join(
        conceptDb |>
          dplyr::rename("drug_concept_id" = "concept_id") |>
          dplyr::select("drug_concept_id", "domain_id", "standard_concept"),
        by = "drug_concept_id"
      ) |>
      dplyr::filter(.data$domain_id %in% .env$domains &
                      .data$standard_concept %in% .env$standardConceptFlags) |>
      dplyr::rename_with(tolower)
  }


  candidateCodesList <- list()

  workingConcept <- concept |>
    dplyr::filter(.data$domain_id %in% .env$domains)

  if (!is.null(conceptSynonym)) {
    workingconceptSynonym <- conceptSynonym |>
      dplyr::left_join(
        concept |>
          dplyr::select("concept_id", "domain_id"),
        by = "concept_id"
      ) |>
      dplyr::filter(.data$domain_id %in% .env$domains) |>
      dplyr::select(!"domain_id")
  }


  # Start finding candidate codes
  # 1) first, get codes to exclude
  # and anti_join throughout to make sure these don't appear
  # exact matches

  if (length(exclude) > 0) {
    # Get standard, condition concepts which include one of the exclusion words
    # always use exact matching
    excludeCodes <- getMatches(
      words = exclude,
      conceptDf = workingConcept
    )
    if(length(excludeCodes) == 0){
      exclude <- NULL
    }
  }

  # 2) Get standard, condition concepts which
  # include one of the keywords
  cli::cli_inform("Getting concepts to include")
  candidateCodes <- getMatches(
    words = keywords,
    conceptDf = workingConcept
  )

  candidateCodes <- candidateCodes |>
    dplyr::mutate(found_from = "From initial search",
                  found_id = 1L) |>
    dplyr::compute()

  # run exclusion
  if (length(exclude) > 0) {
    if (excludeCodes |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      candidateCodes <- candidateCodes |>
        dplyr::anti_join(
          excludeCodes |>
            dplyr::select("concept_id"),
          by = "concept_id"
        )
    }
  }

  # 3) also search in synonyms if option set to true
  # left join back to concept from workingConcept table
  if (searchInSynonyms) {
    cli::cli_inform("Adding concepts using synonymns")
    candidateCodesInSynonyms <- getMatches(
      words = tidyWords(keywords),
      conceptDf = workingconceptSynonym |>
        dplyr::rename("concept_name" = "concept_synonym_name")
    ) |>
      dplyr::select("concept_id") |>
      dplyr::distinct() |>
      dplyr::left_join(workingConcept,
                       by = "concept_id"
      )

    candidateCodes <- dplyr::union_all(
      candidateCodes,
      candidateCodesInSynonyms |>
        dplyr::mutate(found_from = "In synonyms",
                      found_id = 2L)
    ) |>
      dplyr::distinct()
  }

  # run exclusion
  if (length(exclude) > 0) {
    if (excludeCodes |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      candidateCodes <- candidateCodes |>
        dplyr::anti_join(
          excludeCodes |>
            dplyr::select("concept_id"),
          by = "concept_id"
        ) |>
        dplyr::compute()
    }
  }

  candidateCodesList[[paste0(domains, collapse = ",")]] <- candidateCodes


  # 5) add any codes lower in the hierarchy
  if (includeDescendants) {
    if (candidateCodes |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      cli::cli_inform("Adding descendants")
      candidateCodeDescendants <- addDescendants(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestorDb,
        conceptDf = concept
      )

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodeDescendants |>
          dplyr::mutate(found_from = "From descendants",
                        found_id = 3L) |>
          dplyr::anti_join(candidateCodes |>
                             dplyr::select("concept_id"),
                           by = "concept_id")) |>
        dplyr::compute()

      # run exclusion
      if (length(exclude) > 0) {
        if (excludeCodes |>
            utils::head(10) |>
            dplyr::tally() |>
            dplyr::pull("n") > 0) {
          candidateCodes <- candidateCodes |>
            dplyr::anti_join(excludeCodes |>
                               dplyr::select("concept_id"),
                             by = "concept_id"
            )|>
            dplyr::compute()
        }
      }
    }
  }

  # 6) add any codes one level above in the hierarchy
  if (includeAncestor) {
    if (candidateCodes |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      cli::cli_inform("Adding ancestor")

      candidateCodeAncestor <- addAncestor(
        workingCandidateCodes = candidateCodes,
        conceptAncestorDf = conceptAncestorDb,
        conceptDf = conceptDb
      )

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodeAncestor |>
          dplyr::mutate(found_from = "From ancestor",
                        found_id = 4L)
      ) |>
        dplyr::distinct() |>
        dplyr::compute()

      # run exclusion
      if (length(exclude) > 0) {
        if (excludeCodes |>
            utils::head(10) |>
            dplyr::tally() |>
            dplyr::pull("n") > 0) {
          candidateCodes <- candidateCodes |>
            dplyr::anti_join(excludeCodes |> dplyr::select("concept_id"),
                             by = "concept_id"
            )|>
            dplyr::compute()
        }
      }
    }
  }

  # 7) add codes from non-standard
  # nb we do this last so as to not include descendants
  # which can blow up candiate codelist when there
  # are multiple mappings
  if (searchNonStandard) {
    cli::cli_inform("Adding codes from non-standard")
    conceptNs <- conceptDb |>
      dplyr::filter(.data$standard_concept == "non-standard") |>
      dplyr::rename_with(tolower)

    if (conceptNs |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      candidateCodesNs <- getMatches(
        words = tidyWords(keywords),
        conceptDf = conceptNs
      )
    }

    if (conceptNs |>
        utils::head(10) |>
        dplyr::tally() |>
        dplyr::pull("n") > 0) {
      candidateCodesNs <- candidateCodesNs |>
        dplyr::select("concept_id") |>
        dplyr::left_join(
          conceptRelationshipDb |>
            dplyr::filter(.data$relationship_id == "Mapped from") |>
            dplyr::rename_with(tolower),
          by = c("concept_id" = "concept_id_2")
        ) |>
        dplyr::select("concept_id"= "concept_id_1") |>
        dplyr::distinct() |>
        dplyr::left_join(concept,
                         by = "concept_id"
        ) |>
        dplyr::compute()

      candidateCodes <- dplyr::union_all(
        candidateCodes,
        candidateCodesNs |>
          dplyr::mutate(found_from = "From non-standard",
                        found_id = 5L)
      )  |>
        dplyr::compute()
    }

    # run exclusion
    if (length(exclude) > 0) {
      if (excludeCodes |>
          utils::head(10) |>
          dplyr::tally() |>
          dplyr::pull("n") > 0) {
        candidateCodes <- candidateCodes |>
          dplyr::anti_join(excludeCodes |> dplyr::select("concept_id"),
                           by = "concept_id"
          )|>
          dplyr::compute()
      }
    }
  }

  candidateCodes <- candidateCodes |>
    dplyr::select(c("concept_id", "found_from", "found_id")) |>
    dplyr::inner_join(cdm[["concept"]] |>
                        dplyr::select("concept_id", "concept_name",
                                      "domain_id", "vocabulary_id",
                                      "standard_concept"),
                      by = "concept_id") |>
    dplyr::distinct() |>
    dplyr::collect()

  if (nrow(candidateCodes) > 0) {
    # 8) Finish up
    cli::cli_inform("Search completed. Finishing up.")

    # remove duplicates (found in different ways)
    # keep first time it was found
    # for drug,  same concept_id with different ingredient_concept_id
    # will be removed as well (with only the first kept).
    candidateCodes <- candidateCodes |>
      dplyr::arrange(.data$found_id) |>
      dplyr::group_by(.data$concept_id)  |>
      dplyr::filter(dplyr::row_number(.data$concept_id) == 1) |>
      dplyr::ungroup() |>
      dplyr::select(!"found_id")

    # make sure we only have codes from the domain of interest
    candidateCodes <- candidateCodes |>
      dplyr::mutate(
        standard_concept1 = dplyr::case_when(
          is.na(.data$standard_concept) ~ "non-standard",
          .data$standard_concept == "C" ~ "classification",
          .data$standard_concept == "S" ~ "standard",
          .default = as.character(.data$standard_concept)
        )
      ) |>
      dplyr::filter(tolower(.data$domain_id) %in% tolower(.env$domains),
                    tolower(.data$standard_concept1) %in% .env$standardConceptFlags) |>
      dplyr::select(-"standard_concept1")
  }


  if(!is.null(attr(cdm, "dbcon"))){
    omopgenerics::dropSourceTable(cdm = cdm,
                            name = dplyr::starts_with(paste0("cg_",prefix)))
  }

  return(candidateCodes)

}


# helper functions for runSearch
tidyWords <- function(words, message = TRUE) {
  omopgenerics::assertCharacter(words)

  # to avoid invalid UTF-8 error
  words <- iconv(words, from = "", to = "UTF-8",sub="")

  # Remove / at the beginning and at the end for exact matching
  words <- stringr::str_replace(words, "^/(.*)/$", "\\1")
  words <- stringr::str_replace(words, "^/\b(.*)/\b$", "\\1")

  # throw a warning if there is one of these symbols
  if(isTRUE(message)){
  locs <- stringr::str_locate_all(words, "[^\\x20-\\x7E]|[[:punct:]]|[^\\da-zA-Z ]|[^\x01-\x7F]+")
  symbols <- purrr::map2(words, locs, ~{
    if (nrow(.y) == 0) return(character(0))
    stringr::str_sub(.x, .y[,1], .y[,2])
  })

  symbols <- unlist(symbols)
  dash <- stringr::str_detect(words, "-")

  msg <- as.character()
  if(length(symbols) > 0){
    symbols <- unique(symbols)
    msg <- "Symbols {.val {symbols}} will be ignored. "
  }
  if(any(dash)){
    msg <- append(msg, "`-` will be replaced by an empty space.")
  }
  if(length(msg) > 0){
    cli::cli_inform(msg, call = parent.frame())
  }
  }

  # some generic formatting
  workingWords <- stringr::str_remove_all(words, "[^\\x20-\\x7E]")
  workingWords <- stringi::stri_trans_nfkc(workingWords)
  workingWords <- trimws(workingWords)
  workingWords <- stringr::str_replace_all(workingWords, "-", " ")
  workingWords <- stringr::str_replace_all(workingWords, "[[:punct:]]", "")
  workingWords <- stringr::str_remove_all(workingWords, "[^[\\da-zA-Z ]]")
  workingWords <- stringr::str_remove_all(workingWords, "[^\x01-\x7F]+")
  workingWords <- stringr::str_to_lower(workingWords)
  workingWords <- trimws(workingWords)

  workingWords <- workingWords[workingWords != ""]

  return(workingWords)
}

getMatches <- function(words,
                       conceptDf) {
  conceptDf <- conceptDf |> # start with all
    dplyr::mutate(concept_name = tolower(.data$concept_name))

  # because there may be a lot of synonyms, get these from a loop
  # (stringr::str_detect slows considerably
  # as more options are added in a single call using "|")

  # note, where one term is multiple words (e.g "knee osteoarthritis"),
  # split up and search
  # so that they don´t need to be next to each other
  # (e.g. to find "osteoarthritis of knee")).
  # This only will be applied if the word is not between / (e.g., (1) "/knee osteoarthritis/
  # will only exclude those concepts that contain "knee osteoarthritis", so
  # "osteoarthritis of knee" would not be excluded, but "history of knee osteoarthritis"
  # would be, (2) /ee osteoarthritis/ will also include those concepts with "knee osteoarthritis".
  # If we want to apply exact matches accounting for words boundaries, we need to use "/\bknee osteoarthritis/\b".

  # Notice that this will only be applied for exclude argument, as it is the only one
  # that does not apply tidyWords before using getMatches() function.
  conceptsFound <- list()

  # Exact matches only (including word boundaries)
  exactMatchesBoundaries <- tidyWords(words[stringr::str_detect(words,"^/\b.*/\b$")])
  for(i in seq_along(exactMatchesBoundaries)){
    workingExclude <- exactMatchesBoundaries[i]
    workingConcepts <- conceptDf # start with all

    if (nchar(workingExclude) >= 1) {
      cToSearch <-  workingExclude
      workingConcepts <- workingConcepts |>
        dplyr::filter(stringr::str_like(.data$concept_name, .env$cToSearch))
    }

    conceptsFound[[i]] <- workingConcepts |>
      dplyr::compute()
  }

  exactMatches <- tidyWords(words[stringr::str_detect(words,"^/.*/$")])
  for (i in seq_along(exactMatches)) {
    workingExclude <- exactMatches[i]
    workingConcepts <- conceptDf # start with all

    if (nchar(workingExclude) >= 1) {
      cToSearch <-  paste0("%", workingExclude, "%")
      workingConcepts <- workingConcepts |>
        dplyr::filter(stringr::str_like(.data$concept_name, .env$cToSearch))
    }

    conceptsFound[[i+length(exactMatchesBoundaries)]] <- workingConcepts |>
      dplyr::compute()
  }

  # Flexible search only
  words <- tidyWords(words[!c(stringr::str_detect(words, "^/.*/$")|stringr::str_detect(words,"^/\b.*/\b$")) ])
  for (i in seq_along(words)) {
    workingExclude <- unlist(strsplit(words[i], " "))
    workingConcepts <- conceptDf # start with all

    for (j in seq_along(workingExclude)) {
      if (nchar(workingExclude[j]) >= 1) {

        cToSearch <-  paste0("%", workingExclude[j], "%")
        workingConcepts <- workingConcepts |>
          dplyr::filter(stringr::str_like(.data$concept_name, .env$cToSearch))

      }
    }
    conceptsFound[[i+length(exactMatchesBoundaries)+length(exactMatches)]] <- workingConcepts |>
      dplyr::compute()
  }

  if(length(conceptsFound) == 0){
    return()
  }

  if(length(conceptsFound)==1){
    conceptsFound <- conceptsFound[[1]] |> dplyr::distinct()
  } else {
    conceptsFoundList <- list()
    conceptsFoundList[[1]] <- conceptsFound[[1]]
    for(i in 1:(length(conceptsFound)-1)){
      conceptsFoundList[[1]] <- dplyr::union_all(conceptsFoundList[[1]],
                                                 conceptsFound[[i+1]])

    }
    conceptsFound <- conceptsFoundList[[1]] |> dplyr::distinct()
  }

  return(conceptsFound)
}

addDescendants <- function(workingCandidateCodes,
                           conceptAncestorDf,
                           conceptDf) {

  candidateCodeDescendants <- workingCandidateCodes |>
    dplyr::select("concept_id") |>
    dplyr::rename("ancestor_concept_id" = "concept_id") |>
    dplyr::left_join(
      conceptAncestorDf,
      by = "ancestor_concept_id"
    ) |>
    dplyr::select("concept_id" = "descendant_concept_id") |>
    dplyr::distinct() |>
    dplyr::compute()

  candidateCodeDescendants <- candidateCodeDescendants |>
    dplyr::left_join(conceptDf, by = "concept_id") |>
    dplyr::compute()

  return(candidateCodeDescendants)
}

addAncestor <- function(workingCandidateCodes,
                        conceptAncestorDf,
                        conceptDf) {
  candidateCodeAncestor <- workingCandidateCodes |>
    dplyr::select("concept_id") |>
    dplyr::rename("descendant_concept_id" = "concept_id") |>
    dplyr::left_join(conceptAncestorDf,
                     by = "descendant_concept_id"
    ) |>
    dplyr::filter(.data$min_levels_of_separation == "1") |>
    dplyr::select("ancestor_concept_id") |>
    dplyr::rename("concept_id" = "ancestor_concept_id") |>
    dplyr::left_join(conceptDf,
                     by = "concept_id"
    )

  # keep if not already in candidateCodes
  candidateCodeAncestor <- candidateCodeAncestor |>
    dplyr::anti_join(
      workingCandidateCodes |>
        dplyr::select("concept_id"),
      by = "concept_id"
    )

  return(candidateCodeAncestor)
}
