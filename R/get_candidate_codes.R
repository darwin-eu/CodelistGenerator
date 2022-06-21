
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
#' @param domains  Character vector with one or more of the OMOP CDM domain.
#' @param standard_concept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param search_synonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param search_source Either TRUE or FALSE. If TRUE the code will also
#' search via source concepts.
#' @param fuzzy_match Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param max_distance_cost, The
#' maximum number/fraction of match cost (generalized Levenshtein distance)
#' for fuzzy matching (see ??base::agrep for further details).
#' @param exclude  Character vector of words
#' to identify concepts to exclude.
#' @param include_descendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param include_ancestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#' @param db Database connection via DBI::dbConnect()
#' @param vocabulary_database_schema Name of database
#' schema with vocab tables
#'
#' @return Dataframe
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ### note, Eunomia, used in the example below,
#' ### does not include a full set of vocabularies.
#' ### The full set can be downloaded from https://athena.ohdsi.org
#' \dontrun{
#' library(DBI)
#' library(CodelistGenerator)
#' db <- DBI::dbConnect(" Your database connection here " )
#' vocabulary_database_schema <- " Your vocabulary schema here "
#' get_candidate_codes(
#'   keywords = "asthma",
#'   db = db,
#'   vocabulary_database_schema = vocabulary_database_schema
#' )
#' }
get_candidate_codes <- function(keywords,
                                domains = "Condition",
                                standard_concept = "Standard",
                                search_synonyms = FALSE,
                                search_source = FALSE,
                                fuzzy_match = FALSE,
                                max_distance_cost = 0.1,
                                exclude = NULL,
                                include_descendants = TRUE,
                                include_ancestor = FALSE,
                                verbose = FALSE,
                                db,
                                vocabulary_database_schema) {
  if (verbose == TRUE) {
    # to report time taken at the end
    start <- Sys.time()
  }

  if (verbose == TRUE) {
    message("Checking inputs")
  }

  ## domains and standard_concept to sentence case
  domains <- stringr::str_to_title(domains)
  standard_concept <- stringr::str_to_sentence(standard_concept)

  ## checks for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertVector(keywords,
    add = error_message
  )
  checkmate::assertVector(domains,
    add = error_message
  )
  checkmate::assertVector(standard_concept,
    add = error_message
  )
  standard_concept_check <- all(standard_concept %in%
                    c("Standard",
                      "Classification",
                      "Non-standard"))
  checkmate::assertTRUE(standard_concept_check, add = error_message)

  checkmate::assert_logical(search_synonyms, add = error_message)
  checkmate::assert_logical(search_source, add = error_message)
  checkmate::assert_logical(fuzzy_match,
    add = error_message
  )
  checkmate::assert_numeric(max_distance_cost,
    add = error_message
  )
  checkmate::assertVector(exclude,
    null.ok = TRUE,
    add = error_message
  )
  checkmate::assert_logical(include_descendants,
    add = error_message
  )
  checkmate::assert_logical(include_ancestor,
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  db_inherits_check <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- db must be a database connection via DBI::dbConnect()"
    )
  }
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  # connect to relevant vocabulary tables
  # will return informative error if not found
  concept_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabulary_database_schema}.concept"
  )))
  concept_ancestor_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabulary_database_schema}.concept_ancestor"
  )))
  concept_synonym_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {vocabulary_database_schema}.concept_synonym"
  )))
  concept_relationship_db <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
    ".concept_relationship"
  )))

  # check variable names
  # concept table
  concept_db_names <- c(
    "concept_id", "concept_name", "domain_id",
    "vocabulary_id", "standard_concept"
  )
  concept_db_names_check <- all(concept_db_names %in%
      names(concept_db %>%
       utils::head(1) %>%
       dplyr::collect() %>%
       dplyr::rename_with(tolower)))
  checkmate::assertTRUE(concept_db_names_check, add = error_message)

  # concept_ancestor table
  concept_ancestor_db_names <- c(
    "ancestor_concept_id", "descendant_concept_id",
    "min_levels_of_separation", "max_levels_of_separation"
  )
    c_ancestor_db_names_check <- all(
      concept_ancestor_db_names %in%
      names(concept_ancestor_db %>%
       utils::head(1) %>%
       dplyr::collect() %>%
       dplyr::rename_with(tolower)))
  checkmate::assertTRUE(c_ancestor_db_names_check,
                        add = error_message)
  # concept_synonym table
  concept_synonym_db_names <- c("concept_id",
                                "concept_synonym_name")
  concept_synonym_db_names_check <- all(
      concept_synonym_db_names %in%
      names(concept_synonym_db %>%
       utils::head(1) %>%
       dplyr::collect() %>%
       dplyr::rename_with(tolower)))
  checkmate::assertTRUE(concept_synonym_db_names_check,
                        add = error_message)

  # concept_relationship_db table
  concept_relationship_db_names <- c(
    "concept_id_1", "concept_id_2",
    "relationship_id"
  )
  concept_rel_db_names_check <- all(
      concept_relationship_db_names %in%
      names(concept_relationship_db %>%
       utils::head(1) %>%
       dplyr::collect() %>%
       dplyr::rename_with(tolower)))
  checkmate::assertTRUE(concept_rel_db_names_check,
                        add = error_message)
  # # check domains in db
  domains_in_db <- concept_db %>%
    dplyr::select(.data$domain_id) %>%
    dplyr::distinct() %>%
    dplyr::collect() %>%
    dplyr::pull()
  for (i in seq_along(domains)) {
    domains_check <- domains[i] %in% domains_in_db
    checkmate::assertTRUE(domains_check, add = error_message)
    if (!isTRUE(domains_check)) {
      error_message$push(
        glue::glue("- domain_id {domains[i]} not found in concept table")
      )
    }
  }

  # report all assertions
  checkmate::reportAssertions(collection = error_message)

  # standard_concept to format in concept table
  concept_db <- concept_db %>%
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
  standard_concept_flags <- standard_concept

  # filter vocab tables to keep only relevant data
  if (verbose == TRUE) {
    message("Limiting to potential concepts of interest")
  }
  concept <- concept_db %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standard_concept_flags) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  concept_ancestor_db <- concept_ancestor_db %>%
    dplyr::left_join(concept_db %>%
      dplyr::rename("ancestor_concept_id" = "concept_id") %>%
      dplyr::select("ancestor_concept_id", "domain_id", "standard_concept"),
    by = "ancestor_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standard_concept_flags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::compute()

  concept_ancestor <- concept_ancestor_db %>%
    dplyr::left_join(concept_db %>%
      dplyr::rename("descendant_concept_id" = "concept_id") %>%
      dplyr::select("descendant_concept_id", "domain_id", "standard_concept"),
    by = "descendant_concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standard_concept_flags) %>%
    dplyr::select(-c("domain_id", "standard_concept")) %>%
    dplyr::collect() %>%
    dplyr::rename_with(tolower)

  # will only collect concept_synonym later if needed
  concept_synonym_db <- concept_synonym_db %>%
    dplyr::left_join(concept_db %>%
      dplyr::select("concept_id", "domain_id", "standard_concept"),
    by = "concept_id"
    ) %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept %in% standard_concept_flags) %>%
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
    exclude_codes <- get_exact_matches(
      words = tidy_words(exclude),
      concept_df = concept
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
  if (fuzzy_match == FALSE) {
    candidate_codes <- get_exact_matches(
      words = tidy_words(keywords),
      concept_df = concept
    )
  }

  # 2b) or using fuzzy match
  if (fuzzy_match == TRUE) {
    candidate_codes <- get_fuzzy_matches(
      words = tidy_words(keywords),
      concept_df = concept,
      md_cost = max_distance_cost
    )

    candidate_codes <- candidate_codes %>%
      dplyr::distinct()
  }

  # run exclusion
  if (length(exclude) > 0) {
    if (nrow(exclude_codes) > 0) {
      candidate_codes <- candidate_codes %>%
        dplyr::anti_join(exclude_codes %>%
          dplyr::select("concept_id"),
        by = "concept_id"
        )
    }
  }


  if (nrow(candidate_codes) == 0) {
    candidate_codes
    message("-- No codes found for given keywords")
  } else {

    # 3) look for any standard, condition concepts with a synonym of the
    # codes found from the keywords
    if (search_synonyms == TRUE & verbose == TRUE) {
      message("Getting concepts to include from exact matches of synonyms")
    }

    if (search_synonyms == TRUE) {
      concept_synonym <- concept_synonym_db %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower)

      synonyms <- dtplyr::lazy_dt(concept_synonym) %>%
        dplyr::inner_join(dtplyr::lazy_dt(candidate_codes) %>%
          dplyr::select("concept_id"),
          by = "concept_id") %>%
        as.data.frame() %>%
        dplyr::select("concept_synonym_name") %>%
        dplyr::distinct() %>%
        dplyr::pull()

      # drop any long synonyms (more than 6 words)
      # looking for these adds a lot of run time
      # while being highly unlikely to have a match
      synonyms <- synonyms[stringr::str_count(synonyms, "\\S+") <= 6]
      synonyms <- unique(tidy_words(synonyms))
      # more than one character
      synonyms <- synonyms[stringr::str_count(synonyms) > 1]

      if (fuzzy_match == FALSE) {
        synonym_codes <- get_exact_matches(
          words = synonyms,
          concept_df = concept
        )
      }

      if (fuzzy_match == TRUE) {
        synonym_codes <- get_fuzzy_matches(
          words = tidy_words(synonyms),
          concept_df = concept,
          md_cost = max_distance_cost
        )
      }

      candidate_codes <- dplyr::bind_rows(candidate_codes, synonym_codes) %>%
        dplyr::distinct()
    }

    if (search_synonyms == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }


    # 4) add any codes lower in the hierachy
    if (include_descendants == TRUE & verbose == TRUE) {
      message("Getting concepts to include from descendants")
    }

    if (include_descendants == TRUE) {
      candidate_code_descendants <- add_descendants(
        working_candidate_codes = candidate_codes,
        concept_ancestor_df = concept_ancestor,
        concept_df = concept
      )

      candidate_codes <- dplyr::bind_rows(
        candidate_codes,
        candidate_code_descendants
      ) %>%
        dplyr::distinct()
    }

    if (include_descendants == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }

    # 5) add any codes one level above in the hierachy
    if (include_ancestor == TRUE & verbose == TRUE) {
      message("Getting concepts to include from
                direct ancestors of identified concepts")
    }

    if (include_ancestor == TRUE) {
      candidate_code_ancestor <- add_ancestor(
        working_candidate_codes = candidate_codes,
        concept_ancestor_df = concept_ancestor,
        concept_df = concept
      )

      candidate_codes <- dplyr::bind_rows(
        candidate_codes,
        candidate_code_ancestor
      ) %>%
        dplyr::distinct()
    }

    if (include_ancestor == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }

    # 6) add codes from source
    # nb do this last so as to not include descendants
    # which can blow up candiate codelist when tbere
    # are multiple mappins
    if (search_source == TRUE & verbose == TRUE) {
      message("Getting concepts to include from source concepts")
    }

    if (search_source == TRUE) {
      concept_ns <- concept_db %>%
        dplyr::filter(.data$domain_id %in% domains) %>%
        dplyr::filter(.data$standard_concept == "Non-standard") %>%
        dplyr::collect() %>%
        dplyr::rename_with(tolower)

      if (fuzzy_match == FALSE) {
        candidate_codes_ns <- get_exact_matches(
          words = tidy_words(keywords),
          concept_df = concept_ns
        )
      }

      if (fuzzy_match == TRUE) {
        candidate_codes_ns <- get_fuzzy_matches(
          words = tidy_words(keywords),
          concept_df = concept_ns,
          md_cost = max_distance_cost
        )
      }

      candidate_codes_ns <- candidate_codes_ns %>%
        dplyr::select("concept_id") %>%
        dplyr::inner_join(concept_relationship_db %>%
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
        dplyr::mutate(concept_name = tidy_words(.data$concept_name))

      candidate_codes <- dplyr::bind_rows(
        candidate_codes,
        candidate_codes_ns
      ) %>%
        dplyr::distinct()
    }

    if (search_synonyms == TRUE) {
      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }



    # 7) Finish up
    # get original names back
    candidate_codes <- candidate_codes %>%
      dplyr::select(.data$concept_id) %>%
      dplyr::inner_join(concept,
        by = c("concept_id")
      ) %>%
      dplyr::distinct()

    candidate_codes <- candidate_codes %>%
      dplyr::select(
        "concept_id", "concept_name",
        "domain_id", "vocabulary_id"
      )

    if (verbose == TRUE) {
      duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
      message(glue::glue(
        "Time: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
      ))
    }

    candidate_codes %>%
      dplyr::distinct() # return
  }
}


# helper functions for main get_candidate_codes function
get_exact_matches <- function(words,
                              concept_df) {

  # because there may be a lot of synonyms, get these from a loop
  # (stringr::str_detect slows considerably
  # as more options are added in a single call using "|")

  # note, where one term is multiple words (e.g "knee osteoarthritis"),
  # split up and search
  # so that they don´t need to be next to each other
  # (e.g. to find "osteoarthritis of knee"))

  concepts_found <- list()
  for (i in seq_along(words)) {
    working_exclude <- unlist(strsplit(words[i], " "))
    working_concepts <- concept_df %>% # start with all
      dplyr::mutate(concept_name = tidy_words(.data$concept_name))

    concepts_found[[i]] <- working_concepts %>%
      dplyr::filter(apply(sapply(
        X = working_exclude,
        FUN = grepl, working_concepts$concept_name
      ),
      MARGIN = 1, FUN = all
      )) %>%
      dplyr::distinct()
  }
  dplyr::bind_rows(concepts_found)
}

get_fuzzy_matches <- function(words,
                              concept_df,
                              md_cost) {
  concepts_found <- list()
  for (i in seq_along(words)) {
    working_keywords <- unlist(strsplit(words[i], " "))
    # more than one character
    working_keywords <- working_keywords[
                stringr::str_count(working_keywords) > 1]
    working_concepts <- concept_df %>% # start with all
      dplyr::mutate(concept_name = tidy_words(.data$concept_name))
    for (j in seq_along(working_keywords)) {
      # filter each term within the loop, one after the other
      indx <- agrep(working_keywords[j], working_concepts$concept_name,
        max.distance = list(
          cost = md_cost
        )
      )
      working_concepts <- working_concepts[indx, ]
    }

    concepts_found[[i]] <- working_concepts
  }

  dplyr::bind_rows(concepts_found) %>%
    dplyr::distinct()
}

add_descendants <- function(working_candidate_codes,
                            concept_ancestor_df,
                            concept_df) {
  candidate_code_descendants <- dtplyr::lazy_dt(working_candidate_codes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("ancestor_concept_id" = "concept_id") %>%
    dplyr::distinct()) %>%
    dplyr::left_join(dtplyr::lazy_dt(concept_ancestor_df %>%
      dplyr::filter("ancestor_concept_id" != "descendant_concept_id")),
    by = "ancestor_concept_id"
    ) %>%
    as.data.frame() %>%
    dplyr::select("descendant_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::rename("concept_id" = "descendant_concept_id")

  candidate_code_descendants <-
    dtplyr::lazy_dt(candidate_code_descendants) %>%
    dplyr::left_join(dtplyr::lazy_dt(concept_df), by = "concept_id") %>%
    as.data.frame() %>%
    dplyr::mutate(concept_name = tidy_words(.data$concept_name))

  candidate_code_descendants
}

add_ancestor <- function(working_candidate_codes,
                         concept_ancestor_df,
                         concept_df) {
  candidate_code_ancestor <- dtplyr::lazy_dt(working_candidate_codes %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("descendant_concept_id" = "concept_id")) %>%
    dplyr::left_join(dtplyr::lazy_dt(concept_ancestor_df),
      by = "descendant_concept_id"
    ) %>%
    dplyr::filter(.data$min_levels_of_separation == "1") %>%
    dplyr::select("ancestor_concept_id") %>%
    dplyr::rename("concept_id" = "ancestor_concept_id") %>%
    dplyr::left_join(dtplyr::lazy_dt(concept_df),
                     by = "concept_id") %>%
    as.data.frame() %>%
    dplyr::mutate(concept_name = tidy_words(.data$concept_name))

  # keep if not already in candidate_codes
  candidate_code_ancestor <- candidate_code_ancestor %>%
    dplyr::anti_join(working_candidate_codes %>%
      dplyr::select("concept_id"),
    by = "concept_id"
    ) %>%
    dplyr::left_join(concept_df, by = "concept_id")

  candidate_code_ancestor
}


.datatable.aware <- TRUE
