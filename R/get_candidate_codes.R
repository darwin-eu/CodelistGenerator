
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
#' @param search_synonyms Either TRUE or FALSE. If TRUE the code will also
#' search via the concept synonym table.
#' @param fuzzy_match Either TRUE or FALSE. If TRUE the fuzzy matching
#' will be used, with approximate matches identified.
#' @param max_distance_substitutions, The maximum distance
#' parameter of substitution for fuzzy matching
#' (see ??base::agrep for further details).
#' @param max_distance_insertions, The
#' maximum distance parameter of insertion for
#' fuzzy matching (see ??base::agrep for further details).
#' @param max_distance_deletions, The
#' maximum distance parameter of deletion
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
#' library(Eunomia)
#' library(DBI)
#' library(RSQLite)
#' untar(xzfile(system.file("sqlite", "cdm.tar.xz",
#'   package = "Eunomia"
#' ), open = "rb"),
#' exdir = tempdir()
#' )
#' db <- DBI::dbConnect(
#'   RSQLite::SQLite(),
#'   paste0(tempdir(), "\\cdm.sqlite")
#' )
#' get_candidate_codes(
#'   keywords = "asthma",
#'   search_synonyms = TRUE,
#'   fuzzy_match = TRUE,
#'   exclude = NULL,
#'   include_descendants = TRUE,
#'   include_ancestor = FALSE,
#'   db = db,
#'   vocabulary_database_schema = "main"
#' )
#' }
get_candidate_codes <- function(keywords,
                                domains = c(
                                  "Condition", "Drug",
                                  "Observation", "Procedure"
                                ),
                                search_synonyms = FALSE,
                                fuzzy_match = FALSE,
                                max_distance_substitutions = 0.1,
                                max_distance_deletions = 0.1,
                                max_distance_insertions = 0.1,
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

  ## domain to sentence case
  domains <- stringr::str_to_sentence(domains)

  ## checks for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertVector(keywords,
    add = error_message
  )
  checkmate::assertVector(domains,
    add = error_message
  )
  checkmate::assert_logical(search_synonyms, add = error_message)
  checkmate::assert_logical(fuzzy_match,
    add = error_message
  )
  checkmate::assert_numeric(max_distance_substitutions,
    add = error_message
  )
  checkmate::assert_numeric(max_distance_deletions,
    add = error_message
  )
  checkmate::assert_numeric(max_distance_insertions,
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

  # lowercase names
  concept_db <- dplyr::rename_with(concept_db, tolower) %>%
    dplyr::compute()
  concept_ancestor_db <- dplyr::rename_with(concept_ancestor_db, tolower) %>%
    dplyr::compute()
  concept_synonym_db <- dplyr::rename_with(concept_synonym_db, tolower) %>%
    dplyr::compute()

  # check variable names
  # concept table
  concept_db_names <- c(
    "concept_id", "concept_name", "domain_id",
    "vocabulary_id", "standard_concept"
  )
  for (i in seq_along(concept_db_names)) {
    variable_check <- exists(concept_db_names[i], where = concept_db %>%
      utils::head(1) %>%
      dplyr::collect())
    if (!isTRUE(variable_check)) {
      checkmate::assertTRUE(variable_check, add = error_message)
      error_message$push(glue::glue(
        "- Variable {concept_db_names[i]} not found in concept table"
      ))
    }
  }
  # concept_ancestor table
  concept_ancestor_db_names <- c(
    "ancestor_concept_id", "descendant_concept_id",
    "min_levels_of_separation", "max_levels_of_separation"
  )
  for (i in seq_along(concept_ancestor_db_names)) {
    variable_check <- exists(concept_ancestor_db_names[i],
      where = concept_ancestor_db %>%
        utils::head(1) %>%
        dplyr::collect()
    )
    if (!isTRUE(variable_check)) {
      checkmate::assertTRUE(variable_check, add = error_message)
      error_message$push(
        glue::glue(
          "- Variable {concept_ancestor_db_names[i]}
          not found in concept_ancestor table"
        )
      )
    }
  }
  # concept_synonym table
  concept_synonym_db_names <- c("concept_id", "concept_synonym_name")
  for (i in seq_along(concept_synonym_db_names)) {
    variable_check <- exists(concept_synonym_db_names[i],
      where = concept_synonym_db %>%
        utils::head(1) %>%
        dplyr::collect()
    )
    if (!isTRUE(variable_check)) {
      checkmate::assertTRUE(variable_check, add = error_message)
      error_message$push(
        glue::glue("- Variable {concept_synonym_db_names[i]}
                   not found in concept_synonym table")
      )
    }
  }

  # check domains in db
  domains_in_db <- concept_db %>%
    dplyr::group_by(.data$domain_id) %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::select(.data$domain_id) %>%
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


  # filter vocab tables to keep only relevant data
  if (verbose == TRUE) {
    message("Limiting to potential concepts of interest")
  }
  concept_db <- concept_db %>%
    dplyr::filter(.data$domain_id %in% domains) %>%
    dplyr::filter(.data$standard_concept == "S") %>%
    dplyr::compute()
  concept <- concept_db %>% dplyr::collect()

  concept_ancestor_db_ancestor <- concept_db %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("ancestor_concept_id" = "concept_id") %>%
    dplyr::inner_join(concept_ancestor_db,
      by = "ancestor_concept_id"
    ) %>%
    dplyr::compute()

  concept_ancestor_db_descendant <- concept_db %>%
    dplyr::select("concept_id") %>%
    dplyr::rename("descendant_concept_id" = "concept_id") %>%
    dplyr::inner_join(concept_ancestor_db,
      by = "descendant_concept_id"
    ) %>%
    dplyr::compute()

  concept_ancestor <- dplyr::bind_rows(
    concept_ancestor_db_ancestor %>% dplyr::collect(),
    concept_ancestor_db_descendant %>% dplyr::collect()
  ) %>%
    dplyr::distinct()

  concept_synonym_db <- concept_db %>%
    dplyr::select("concept_id") %>%
    dplyr::inner_join(concept_synonym_db, by = "concept_id") %>%
    dplyr::compute()

  # Start finding candidate codes
  # 1) first, get codes to exclude
  # and anti_join throughought to make sure these don't appear
  # note, based on exact matches only (no option for fuzzy exclusion)

  if (length(exclude) > 0) {
    if (verbose == TRUE) {
      message("Getting concepts to exclude")
    }
    # Get standard, condition concepts which include one of the exclusion words
    exclude_codes <- get_exact_matches(
      words = clean_words(exclude),
      concept_df = concept
    )
  }

  # 2) Get standard, condition concepts which
  # include one of the keywords on exact match
  if (verbose == TRUE) {
    message("Getting concepts to include from exact matches")
  }

  candidate_codes <- get_exact_matches(
    words = clean_words(keywords),
    concept_df = concept
  )

  if (length(exclude) > 0) {
    if (nrow(exclude_codes) > 0) {
      candidate_codes <- candidate_codes %>%
        dplyr::anti_join(exclude_codes %>%
          dplyr::select("concept_id"),
        by = "concept_id"
        )
    }
  }

  # 3) use fuzzy match to find additional codes
  if (fuzzy_match == TRUE) {
    if (verbose == TRUE) {
      message("Getting concepts to include from fuzzy matches")
    }

    candidate_codes_fuzzy <- get_fuzzy_matches(
      words = clean_words(keywords),
      concept_df = concept,
      md_substitutions = max_distance_substitutions,
      md_deletions = max_distance_deletions,
      md_insertions = max_distance_insertions
    )

    candidate_codes <- dplyr::bind_rows(
      candidate_codes,
      candidate_codes_fuzzy
    ) %>%
      dplyr::distinct()

    if (length(exclude) > 0) {
      if (nrow(exclude_codes) > 0) {
        candidate_codes <- candidate_codes %>%
          dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
            by = "concept_id"
          )
      }
    }
  }

  if (nrow(candidate_codes) == 0) {
    candidate_codes
    message("-- No codes found for given keywords")
  } else {
    # 4) look for any standard, condition concepts with a synonym of the
    # codes found from the keywords
    if (search_synonyms == TRUE) {
      if (verbose == TRUE) {
        message("Getting concepts to include from exact matches of synonyms")
      }

      concept_synonym <- concept_synonym_db %>% dplyr::collect()

      synonym_codes <- find_from_synonyms(
        working_candidate_codes = candidate_codes,
        concept_synonym_df = concept_synonym,
        concept_df = concept
      )


      candidate_codes <- dplyr::bind_rows(candidate_codes, synonym_codes) %>%
        dplyr::distinct()

      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }


    # 5) add any codes lower in the hierachy
    if (include_descendants == TRUE) {
      if (verbose == TRUE) {
        message("Getting concepts to include from descendants")
      }

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

      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }

    # 6) add any codes one level above in the hierachy
    if (include_ancestor == TRUE) {
      if (verbose == TRUE) {
        message("Getting concepts to include from
                direct ancestors of identified concepts")
      }

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

      if (length(exclude) > 0) {
        if (nrow(exclude_codes) > 0) {
          candidate_codes <- candidate_codes %>%
            dplyr::anti_join(exclude_codes %>% dplyr::select("concept_id"),
              by = "concept_id"
            )
        }
      }
    }

    # get original names back
    candidate_codes <- candidate_codes %>%
      dplyr::select(.data$concept_id) %>%
      dplyr::inner_join(concept,
        by = c("concept_id")
      ) %>%
      dplyr::distinct()

    candidate_codes <- candidate_codes %>%
      dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id")

    if (verbose == TRUE) {
      duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
      message(glue::glue(
        "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"))
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
      dplyr::mutate(concept_name = clean_words(.data$concept_name))

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
                              md_substitutions,
                              md_deletions,
                              md_insertions) {
  concepts_found <- list()
  for (i in seq_along(words)) {
    working_keywords <- unlist(strsplit(words[i], " "))
    working_concepts <- concept_df %>% # start with all
      dplyr::mutate(concept_name = clean_words(.data$concept_name))
    for (j in seq_along(working_keywords)) {
      # filter each term within the loop, one after the other
      indx <- agrep(working_keywords[j], working_concepts$concept_name,
        max.distance = list(
          substitutions = md_substitutions,
          deletions = md_deletions,
          insertions = md_insertions
        )
      )
      working_concepts <- working_concepts[indx, ]
    }

    concepts_found[[i]] <- working_concepts
  }

  dplyr::bind_rows(concepts_found) %>%
    dplyr::distinct()
}

find_from_synonyms <- function(working_candidate_codes,
                               concept_synonym_df,
                               concept_df) {
  synonyms <- dtplyr::lazy_dt(concept_synonym_df) %>%
    dplyr::inner_join(dtplyr::lazy_dt(working_candidate_codes) %>%
      dplyr::select("concept_id")) %>%
    as.data.frame() %>%
    dplyr::select("concept_synonym_name") %>%
    dplyr::distinct() %>%
    dplyr::pull()

  # drop any long synonyms (more than 6 words)
  # looking for these adds a lot of run time
  # while being highly unlikely to have a match
  synonyms <- synonyms[stringr::str_count(synonyms, "\\S+") <= 6]
  synonyms <- unique(clean_words(synonyms))
  # more than one character
  synonyms <- synonyms[stringr::str_count(synonyms) > 1]

  get_exact_matches(
    words = synonyms,
    concept_df = concept_df
  )
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
    dplyr::mutate(concept_name = clean_words(.data$concept_name))

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
    dplyr::left_join(dtplyr::lazy_dt(concept_df)) %>%
    as.data.frame() %>%
    dplyr::mutate(concept_name = clean_words(.data$concept_name))

  # keep if not already in candidate_codes
  candidate_code_ancestor <- candidate_code_ancestor %>%
    dplyr::anti_join(working_candidate_codes %>% dplyr::select("concept_id"),
      by = "concept_id"
    ) %>%
    dplyr::left_join(concept_df, by = "concept_id")

  candidate_code_ancestor
}


.datatable.aware <- TRUE
