
#' Prepare words for search
#' @noRd

generate_mock_db<-function(){

# tables
  concept <- data.frame(
    concept_id = 1:7,
    concept_name = c(
      "Musculoskeletal disorder",
      "Osteoarthrosis",
      "Arthritis",
      "Osteoarthritis of knee",
      "Osteoarthritis of hip",
      "Degenerative arthropathy",
      "Knee osteoarthritis"
    ),
    domain_id = "Condition",
    vocabulary_id = c(
      rep("SNOMED", 5),
      rep("Read", 2)
    ),
    standard_concept = c(
      rep("S", 5),
      rep(NA, 2)
    ),
    concept_code = NA
  )
  concept_ancestor <- dplyr::bind_rows(
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 2,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 3,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 4,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 1,
      descendant_concept_id = 5,
      min_levels_of_separation = 2,
      max_levels_of_separation = 2
    ),
    data.frame(
      ancestor_concept_id = 3,
      descendant_concept_id = 4,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    ),
    data.frame(
      ancestor_concept_id = 3,
      descendant_concept_id = 5,
      min_levels_of_separation = 1,
      max_levels_of_separation = 1
    )
  )
  concept_synonym <- data.frame(
    concept_id = 3,
    concept_synonym_name = "Osteoarthrosis"
  )
  concept_relationship <- dplyr::bind_rows(
    data.frame(
      concept_id_1 = 2,
      concept_id_2 = 6,
      relationship_id = "Mapped from"
    ),
    data.frame(
      concept_id_1 = 4,
      concept_id_2 = 7,
      relationship_id = "Mapped from"
    )
  )

  # into table
  db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept",
                 concept, overwrite = TRUE)
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_ancestor",
                 concept_ancestor, overwrite = TRUE)
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_synonym",
                 concept_synonym, overwrite = TRUE)
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_relationship",
                 concept_relationship, overwrite = TRUE)
  })

  return(db)

}
