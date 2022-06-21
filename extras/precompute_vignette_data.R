## precompute data for vignettes -----
# to avoid fails in continuous integration of vignettes
# (because of the use of external data)
# will create the data for vignettes here
library(readr)
library(DBI)
library(RSQLite)
library(here)
library(dplyr)
library(stringr)
library(DT)
library(kableExtra)
devtools::load_all()

db<-dbConnect(RSQLite::SQLite(), paste0(Sys.getenv("omop_cdm_vocab_path"),".sqlite"))
vocabulary_database_schema <- "main"



# intro vignette ----
vocab_version <- dplyr::tbl(db, dplyr::sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
    ".vocabulary"
    ))) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(.data$vocabulary_id == "None") %>%
    dplyr::select("vocabulary_version") %>%
    dplyr::collect() %>%
    dplyr::pull()
saveRDS(
  vocab_version,
  here("vignettes", "intro_vocab.RData")
)

codes_from_descendants <- tbl(db, sql(paste0(
  "SELECT * FROM ",
  vocabulary_database_schema,
  ".concept_ancestor"
))) %>%
  filter(ancestor_concept_id == "4182210") %>%
  select("descendant_concept_id") %>%
  rename("concept_id" = "descendant_concept_id") %>%
  left_join(tbl(db, sql(paste0(
    "SELECT * FROM ",
    vocabulary_database_schema,
    ".concept"
  )))) %>%
  select("concept_id", "concept_name", "domain_id", "vocabulary_id") %>%
  collect()
saveRDS(
  codes_from_descendants,
  here("vignettes", "intro_data_01.RData")
)

dementia_codes1 <- get_candidate_codes(
  keywords = "dementia",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = NULL,
  include_descendants = TRUE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  dementia_codes1,
  here("vignettes", "intro_data_02.RData")
)

code_comparison <- full_join(
  codes_from_descendants %>% mutate(type1 = "4182210 and descendants"),
  dementia_codes1 %>% mutate(type2 = "CodelistGenerator")
) %>%
  mutate(type = ifelse(!is.na(type1) & !is.na(type2),
    "Both",
    ifelse(!is.na(type1) & is.na(type2),
      "Only 4182210 and descendants",
      ifelse(is.na(type1) & !is.na(type2),
        "Only CodelistGenerator",
        NA
      )
    )
  )) %>%
  select(-c("type1", "type2"))
saveRDS(
  code_comparison,
  here("vignettes", "intro_data_03.RData")
)

icd_mappings <- show_mappings(
  candidate_codelist = dementia_codes1,
  source_vocabularies = "ICD10CM",
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  icd_mappings,
  here("vignettes", "intro_data_04.RData")
)

read_mappings <- show_mappings(
  candidate_codelist = dementia_codes1,
  source_vocabularies = "Read",
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  read_mappings,
  here("vignettes", "intro_data_05.RData")
)


# options vignette ------
oa_codes1 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes1,
  here("vignettes", "options_data_01.RData")
)

# include desc
oa_codes2 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = TRUE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes2,
  here("vignettes", "options_data_02.RData")
)

# include obs
oa_codes3 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = c("Condition", "Observation"),
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  max_distance_cost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes3,
  here("vignettes", "options_data_03.RData")
)

# search syn
oa_codes4 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = TRUE,
  fuzzy_match = FALSE,
  max_distance_cost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes4,
  here("vignettes", "options_data_04.RData")
)

# search source
oa_codes5 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_source = TRUE,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes5,
  here("vignettes", "options_data_04.RData")
)



# fuzzy search
oa_codes6 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = TRUE,
  max_distance_cost = 0.1,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes6,
  here("vignettes", "options_data_05.RData")
)

# fuzzy search 0.2
oa_codes7 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = TRUE,
  max_distance_cost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = FALSE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes7,
  here("vignettes", "options_data_06.RData")
)

# include ancestor
oa_codes8 <- get_candidate_codes(
  keywords = "osteoarthritis",
  domains = "Condition",
  search_synonyms = FALSE,
  fuzzy_match = FALSE,
  max_distance_cost = 0.2,
  exclude = c(
    "post-infection",
    "post-traumatic"
  ),
  include_descendants = FALSE,
  include_ancestor = TRUE,
  db = db,
  vocabulary_database_schema = vocabulary_database_schema
)
saveRDS(
  oa_codes8,
  here("vignettes", "options_data_07.RData")
)

# colonoscopy vignette ------
codes_from_descendants<-tbl(db,
  sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept_ancestor"))) %>%
  filter(ancestor_concept_id %in% c("4249893", "937652", "40480729")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(tbl(db, sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept"))))%>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codes_from_descendants,
  here("vignettes", "proc_data_01.RData")
)



colonoscopy_codes2<-get_candidate_codes(keywords="colonoscopy",
                    domains=c("Procedure", "Measurement"),
                    search_synonyms = FALSE,
                    fuzzy_match = FALSE,
                    exclude = NULL,
                    include_descendants = FALSE,
                    include_ancestor = FALSE,
                    verbose = TRUE ,
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
saveRDS(
  colonoscopy_codes2,
  here("vignettes", "proc_data_02.RData")
)



# medication vignette ------
codes_from_descendants<-tbl(db,
  sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept_ancestor"))) %>%
  filter(ancestor_concept_id %in% c("1503297")) %>%
  select("descendant_concept_id") %>%
  rename("concept_id"="descendant_concept_id") %>%
  inner_join(tbl(db, sql(paste0("SELECT * FROM ",
     vocabulary_database_schema,
     ".concept"))))%>%
  select("concept_id", "concept_name",
         "domain_id", "vocabulary_id") %>%
  collect()

saveRDS(
  codes_from_descendants,
  here("vignettes", "med_data_01.RData")
)



metformin_codes2<-get_candidate_codes(keywords="metformin",
                    domains=c("Drug"),
                    standard_concept=c("Standard", "Classification"),
                    search_synonyms = FALSE,
                    fuzzy_match = FALSE,
                    exclude = NULL,
                    include_descendants = TRUE,
                    include_ancestor = FALSE,
                    verbose = TRUE ,
                    db=db,
                    vocabulary_database_schema =  vocabulary_database_schema)
saveRDS(
  metformin_codes2,
  here("vignettes", "metformin_codes2.RData")
)



