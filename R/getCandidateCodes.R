
#' Generate candidate codelist for the OMOP CDM
#'
#' @description
#' This function generates a set of codes that
#' can be considered for creating a phenotype
#' using the OMOP CDM.
#'
#' @param keywords Character vector of words to search for.
#' Where more than one word is given (e.g. "knee osteoarthritis"),
#' all combinations of those words should be identified
#' positions (e.g. "osteoarthritis of knee") should be identified.
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param conceptClassId Character vector with one or more concept class
#' of the Concept
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param searchSource Either TRUE or FALSE. If TRUE the code will also
#' search via source concepts.
#' @param fuzzyMatch Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param maxDistanceCost, The
#' maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
#' @param exclude  Character vector of words
#' to identify concepts to exclude.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#' @param db Database connection via DBI::dbConnect()
#' @param vocabularyDatabaseSchema Name of database
#' schema with vocab tables
#'
#' @return tibble
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here ")
#' vocabularyDatabaseSchema <- " Your vocabulary schema here "
#' getCandidateCodes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabularyDatabaseSchema = vocabularyDatabaseSchema
#' )
#' }
getCandidateCodes <- function(keywords,
                              domains = "Condition",
                              conceptClassId = NULL,
                              standardConcept = "Standard",
                              searchSynonyms = FALSE,
                              searchSource = FALSE,
                              fuzzyMatch = FALSE,
                              maxDistanceCost = 0.1,
                              exclude = NULL,
                              includeDescendants = TRUE,
                              includeAncestor = FALSE,
                              verbose = FALSE,
                              db,
                              vocabularyDatabaseSchema) {
  if (verbose == TRUE) {
    # to report time taken at the end
    start <- Sys.time()
  }

  if (verbose == TRUE) {
    message("Checking inputs")
  }

  ## domains and standardConcept to sentence case
  domains <- stringr::str_to_title(domains)
  standardConcept <- stringr::str_to_sentence(standardConcept)

  ## checks for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assertVector(keywords,
    add = errorMessage
  )
  checkmate::assertVector(conceptClassId,
    add = errorMessage,
    null.ok = TRUE
  )
  checkmate::assertVector(domains,
    add = errorMessage
  )
  checkmate::assertVector(standardConcept,
    add = errorMessage
  )
  standardConceptCheck <- all(standardConcept %in%
    c(
      "Standard",
      "Classification",
      "Non-standard"
    ))
  checkmate::assertTRUE(standardConceptCheck, add = errorMessage)

  checkmate::assert_logical(searchSynonyms, add = errorMessage)
  checkmate::assert_logical(searchSource, add = errorMessage)
  checkmate::assert_logical(fuzzyMatch,
    add = errorMessage
  )
  checkmate::assert_numeric(maxDistanceCost,
    add = errorMessage
  )
  checkmate::assertVector(exclude,
    null.ok = TRUE,
    add = errorMessage
  )
  checkmate::assert_logical(includeDescendants,
    add = errorMessage
  )
  checkmate::assert_logical(includeAncestor,
    add = errorMessage
  )
  checkmate::assert_logical(verbose,
    add = errorMessage
  )
  dbInheritsCheck <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(dbInheritsCheck,
    add = errorMessage
  )
  if (!isTRUE(dbInheritsCheck)) {
    errorMessage$push(
      "- db must be a database connection via DBI::dbConnect()"
    )
  }
  # report initial assertions
  checkmate::reportAssertions(collection = errorMessage)

  # connect to relevant vocabulary tables
  # will return informative error if not found
  conceptDb <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept"
  )))
  conceptAncestorDb <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_ancestor"
  )))
  conceptSynonymDb <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabularyDatabaseSchema}.concept_synonym"
  )))
  conceptRelationshipDb <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabularyDatabaseSchema,
    ".concept_relationship"
  )))

  # check variable names
  # concept table
  conceptDbNames <- c(
    "concept_id", "concept_name", "domain_id",
    "vocabulary_id", "standard_concept"
  )
  conceptDbNamesCheck <- all(conceptDbNames %in%
    names(conceptDb %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  checkmate::assertTRUE(conceptDbNamesCheck, add = errorMessage)

  # conceptAncestor table
  conceptAncestorDbNames <- c(
    "ancestor_concept_id", "descendant_concept_id",
    "min_levels_of_separation", "max_levels_of_separation"
  )
  cAncestorDbNamesCheck <- all(
    conceptAncestorDbNames %in%
      names(conceptAncestorDb %>%
        utils::head(1) %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower))
  )
  checkmate::assertTRUE(cAncestorDbNamesCheck,
    add = errorMessage
  )
  # conceptSynonym table
  conceptSynonymDbNames <- c(
    "concept_id",
    "concept_synonym_name"
  )
  conceptSynonymDbNamesCheck <- all(
    conceptSynonymDbNames %in%
      names(conceptSynonymDb %>%
        utils::head(1) %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower))
  )
  checkmate::assertTRUE(conceptSynonymDbNamesCheck,
    add = errorMessage
  )

  # conceptRelationshipDb table
  conceptRelationshipDbNames <- c(
    "concept_id_1", "concept_id_2",
    "relationship_id"
  )
  conceptRelDbNamesCheck <- all(
    conceptRelationshipDbNames %in%
      names(conceptRelationshipDb %>%
        utils::head(1) %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower))
  )
  checkmate::assertTRUE(conceptRelDbNamesCheck,
    add = errorMessage
  )
  # check domains in db
  domainsInDb <- conceptDb %>%
    dplyr::select(.data$domain_id) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()
  for (i in seq_along(domains)) {
    domainsCheck <- domains[i] %in% domainsInDb
    checkmate::assertTRUE(domainsCheck, add = errorMessage)
    if (!isTRUE(domainsCheck)) {
      errorMessage$push(
        glue::glue("- domain_id {domains[i]} not found in concept table")
      )
    }
  }

  # check conceptClassId in db
  conceptClassInDb <- conceptDb %>%
    dplyr::select(.data$concept_class_id) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()
  for (i in seq_along(conceptClassId)) {
    conceptClassCheck <- conceptClassId[i] %in% conceptClassInDb
    checkmate::assertTRUE(conceptClassCheck, add = errorMessage)
    if (!isTRUE(conceptClassCheck)) {
      errorMessage$push(
        glue::glue("- conceptClassId {conceptClassId[i]} not found in concept table")
      )
    }
  }

  # report all assertions
  checkmate::reportAssertions(collection = errorMessage)

  # standard_concept to format in concept table
  conceptDb <- conceptDb %>%
    dplyr::mutate(
      standard_concept =
        dplyr::case_when(
          is.na(standard_concept) ~ "Non-standard",
          standard_concept == "C" ~ "Classification",
          standard_concept == "S" ~ "Standard"
        )
    ) %>%
    dplyr::compute()
  # new name for readibility
  standardConceptFlags <- standardConcept

  # filter vocab tables to keep only relevant data
  if (verbose == TRUE) {
    message("Limiting to potential concepts of interest")
  }

  conceptDb <- conceptDb %>%
    dplyr::filter(.data$domain_id %in% domains)
  if (!is.null(conceptClassId)) {
    # first, check some combination exists
    # return error if not
    errorMessage <- checkmate::makeAssertCollection()
    combCheck <- conceptDb %>%
      dplyr::group_by(
        .data$domain_id,
        .data$concept_class_id,
        .data$standard_concept
      ) %>%
      dplyr::tally() %>%
      dplyr::filter(.data$domain_id %in% domains) %>%
      dplyr::filter(.data$standard_concept %in% standardConceptFlags) %>%
      dplyr::filter(.data$concept_class_id %in% conceptClassId)
    checkmate::assertTRUE(nrow(combCheck %>% dplyr::collect()) > 0, add = errorMessage)
    if (!isTRUE(nrow(combCheck %>% dplyr::collect()) > 0)) {
      errorMessage$push(
        glue::glue("- No combination of domains, standardConcept, and conceptClassId found in concept table")
      )
    }
    checkmate::reportAssertions(collection = errorMessage)
    # now filter
    conceptDb <- conceptDb %>%
      dplyr::filter(.data$concept_class_id %in% conceptClassId)
  }
  concept <- conceptDb %>%
    dplyr::filter(.data$standard_concept %in% standardConceptFlags) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  conceptAncestorDb <- conceptAncestorDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::rename("ancestor_concept_id" = "concept_id") %>%
      dplyr::select("ancestor_concept_id", "domain_id", "standard_concept"),
    by = "ancestor_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::compute()

  conceptAncestor <- conceptAncestorDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::rename("descendant_concept_id" = "concept_id") %>%
      dplyr::select("descendant_concept_id", "domain_id", "standard_concept"),
    by = "descendant_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standardConceptFlags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  # will only collect conceptSynonym later if needed
  conceptSynonymDb <- conceptSynonymDb %>%
    dplyr::left_join(conceptDb %>%
      dplyr::select("concept_id", "domain_id", "standard_concept"),
    by = "concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standardConceptFlags) %>%
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


  if (nrow(candidateCodes) == 0) {
    candidateCodes
    message("-- No codes found for given keywords")
  } else {

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

      candidateCodes <- dplyr::bind_rows(candidateCodes, synonymCodes) %>%
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
        candidateCodeDescendants
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
        candidateCodeAncestor
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

    # 6) add codes from source
    # nb we do this last so as to not include descendants
    # which can blow up candiate codelist when there
    # are multiple mappings
    if (searchSource == TRUE & verbose == TRUE) {
      message("Getting concepts to include from source concepts")
    }

    if (searchSource == TRUE) {
      conceptNs <- conceptDb %>%
        dplyr::filter(.data$domain_id %in% domains) %>%
        dplyr::filter(.data$standard_concept == "Non-standard") %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower)

      if (fuzzyMatch == FALSE) {
        candidateCodesNs <- getExactMatches(
          words = tidyWords(keywords),
          conceptDf = conceptNs
        )
      }

      if (fuzzyMatch == TRUE) {
        candidateCodesNs <- getFuzzyMatches(
          words = tidyWords(keywords),
          conceptDf = conceptNs,
          mdCost = maxDistanceCost
        )
      }

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
        candidateCodesNs
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



    # 7) Finish up
    # get original names back
    candidateCodes <- candidateCodes %>%
      dplyr::select(.data$concept_id) %>%
      dplyr::inner_join(concept,
        by = c("concept_id")
      ) %>%
      dplyr::distinct()

    candidateCodes <- candidateCodes %>%
      dplyr::select(
        "concept_id", "concept_name",
        "domain_id", "concept_class_id",
        "vocabulary_id"
      )

    if (verbose == TRUE) {
      duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
      message(glue::glue(
        "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
      ))
    }

    candidateCodes %>%
      dplyr::distinct() # return
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

  candidateCodeAncestor
}
