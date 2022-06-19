test_that("tests with mock db", {
  library(DBI)
  library(RSQLite)
  library(dplyr)

# mock db
concept<-data.frame(concept_id=1:7,
                    concept_name=c("Musculoskeletal disorder",
                                   "Osteoarthrosis",
                                   "Arthritis",
                                   "Osteoarthritis of knee",
                                   "Osteoarthritis of hip",
                                   "Degenerative arthropathy",
                                   "Knee osteoarthritis"),
                    domain_id="Condition",
                    vocabulary_id=c(rep("SNOMED",5),
                                    rep("Read", 2)),
                    standard_concept=c(rep("S",5),
                                    rep(NA, 2)),
           concept_code=NA)
concept_ancestor<-bind_rows(
data.frame(ancestor_concept_id=1,
                             descendant_concept_id=2,
                             min_levels_of_separation=1,
                             max_levels_of_separation=1),
data.frame(ancestor_concept_id=1,
                             descendant_concept_id=3,
                             min_levels_of_separation=1,
                             max_levels_of_separation=1),
data.frame(ancestor_concept_id=1,
                             descendant_concept_id=4,
                             min_levels_of_separation=2,
                             max_levels_of_separation=2),
data.frame(ancestor_concept_id=1,
                             descendant_concept_id=5,
                             min_levels_of_separation=2,
                             max_levels_of_separation=2),
data.frame(ancestor_concept_id=3,
                             descendant_concept_id=4,
                             min_levels_of_separation=1,
                             max_levels_of_separation=1),
data.frame(ancestor_concept_id=3,
                             descendant_concept_id=5,
                             min_levels_of_separation=1,
                             max_levels_of_separation=1))
concept_synonym<-data.frame(concept_id=3,
                            concept_synonym_name="Osteoarthrosis")
concept_relationship <- bind_rows(
data.frame(concept_id_1=2,
           concept_id_2=6,
           relationship_id="Mapped from"),
data.frame(concept_id_1=4,
           concept_id_2=7,
           relationship_id="Mapped from"))
db <- dbConnect(RSQLite::SQLite(), ":memory:")
dbWithTransaction(db, {
  dbWriteTable(db, "concept", concept, overwrite =TRUE)
})
dbWithTransaction(db, {
  dbWriteTable(db, "concept_ancestor", concept_ancestor, overwrite =TRUE)
})
dbWithTransaction(db, {
  dbWriteTable(db, "concept_synonym", concept_synonym, overwrite =TRUE)
})
dbWithTransaction(db, {
  dbWriteTable(db, "concept_relationship", concept_relationship, overwrite =TRUE)
})


# tests
codes<-get_candidate_codes(
    keywords = "Musculoskeletal disorder",
    domains="Condition",
    include_descendants = TRUE,
    db = db,
    vocabulary_database_schema = "main")
mappings <- show_mappings(
    candidate_codelist = codes,
    db = db,
    vocabulary_database_schema = "main")
expect_true(
 any(mappings$`Standard concept_id name` %in% "Osteoarthrosis")
)
expect_true(
 any(mappings$`Source name` %in% "Degenerative arthropathy")
)
expect_true(
 any(mappings$`Standard concept_id name` %in% "Osteoarthritis of knee")
)
expect_true(
 any(mappings$`Source name` %in% "Knee osteoarthritis")
)

expect_true(all(c(
    "Standard concept_id (mapped to)",
    "Standard concept_id name",
    "Standard vocabulary",
    "Source concept_id (mapped from)",
    "Source name",
    "Source code",
    "Source vocabulary") %in%
    names(mappings)))

  # expect error if not dbi connection
  expect_error(show_mappings(
    candidate_codelist = codes,
    db = "a",
    vocabulary_database_schema = "main"
  ))


  DBI::dbDisconnect(db)
})
