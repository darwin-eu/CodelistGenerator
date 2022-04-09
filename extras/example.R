library(dplyr)
library(Eunomia)
library(stringr)
library(readr)
# devtools::load_all()
library(CodelistGenerator)


vocab.folder<-"E:/CdmVocab2" # directory of unzipped files
concept<-read_delim(paste0(vocab.folder,"/CONCEPT.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_relationship<-read_delim(paste0(vocab.folder,"/CONCEPT_RELATIONSHIP.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(relationship_id=="Mapped from")
concept_ancestor<-read_delim(paste0(vocab.folder,"/CONCEPT_ANCESTOR.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
concept_synonym<-read_delim(paste0(vocab.folder,"/CONCEPT_SYNONYM.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)
vocabulary<-read_delim(paste0(vocab.folder,"/VOCABULARY.csv"),
     "\t", escape_double = FALSE, trim_ws = TRUE)

db <- dbConnect(RSQLite::SQLite(), here::here("db1.sqlite"))
dbWriteTable(db, "concept", concept)
dbWriteTable(db, "concept_relationship", concept_relationship)
dbWriteTable(db, "concept_ancestor", concept_ancestor)
dbWriteTable(db, "concept_synonym", concept_synonym)
dbWriteTable(db, "vocabulary", vocabulary)
dbSendQuery(db, "CREATE UNIQUE INDEX idx_concept ON concept (concept_id);")
dbSendQuery(db, "CREATE UNIQUE INDEX idx_concept_ancestor ON concept_ancestor (ancestor_concept_id,descendant_concept_id );")
rm(concept,concept_relationship, concept_ancestor, concept_synonym)
vocabulary_database_schema<-"main"

get_candidate_codes(keywords="childhood asthma",
                     domains="Condition",
                    db=db,
                    vocabulary_database_schema =  "main")

profvis::profvis({
  dementia_codes<-get_candidate_codes(keywords="dementia",
                     domains="Condition",
                    db=db,
                    vocabulary_database_schema = "main")
})



dementia_codes<-get_candidate_codes(keywords="dementia",
                     domains="Condition",
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym)

profvis::profvis({
  dementia_codes<-get_candidate_codes(keywords="dementia",
                     domains="Condition",
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym)
})

show_mappings(dementia_codes,
              source_vocabularies="ICD10CM",
              concept = concept,
              concept_relationship = concept_relationship)
show_mappings(dementia_codes,
              source_vocabularies="ICD10",
              concept = concept,
              concept_relationship = concept_relationship)

###
kidney_stone<-get_candidate_codes(keywords="kidney stone",
                     domains="Condition",
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym)
get_candidate_codes(keywords="kidney stone",
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym,
                    concept_relationship = concept_relationship)
get_candidate_codes(keywords="asthma",
                     domains="Condition",
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym,
                    concept_relationship = concept_relationship)
asthma<-get_candidate_codes(keywords="asthma",
                     domains="Condition",
                    search.synonyms=TRUE,
                    include.descendants=TRUE,
                    include.ancestor=TRUE,
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym,
                    concept_relationship = concept_relationship)
get_candidate_codes(keywords="diabetes",
                     domains="Condition",
                    search.synonyms=TRUE,
                              fuzzy.match=TRUE,
                              fuzzy.match.max.distance=0.1,
                              include.descendants=TRUE,
                              include.ancestor=TRUE,
                    concept=concept,
                    concept_ancestor = concept_ancestor,
                    concept_synonym = concept_synonym,
                    concept_relationship = concept_relationship)




concept %>%
  group_by(vocabulary_id) %>%
  tally()

candidate_codelist<-kidney_stone[3,]
show_mappings(kidney_stone, concept = concept,
              concept_relationship = concept_relationship)

show_mappings(kidney_stone,
              source_vocabularies="Read",
              concept = concept,
              concept_relationship = concept_relationship)

show_mappings<-function(candidate_codelist,
                        source_vocabularies=c("ATC","ICD10CM","ICD10PCS" ,
                                              "ICD9CM",  "ICD9Proc" ,
                                              "LOINC","OPCS4","Read",
                                              "RxNorm" ,"RxNorm Extension",
                                              "SNOMED"),
                        concept,
                        concept_relationship){

maps_from<-concept_relationship %>%
  dplyr::filter(.data$relationship_id=="Mapped from")

mapped.codes<- candidate_codelist %>%
    dplyr::select("concept_id", "concept_name") %>%
    dplyr::rename("Standard concept name"="concept_name") %>%
    dplyr::rename("concept_id_1"="concept_id") %>%
    dplyr::left_join(maps_from, by = "concept_id_1") %>%
    dplyr::select("concept_id_1","Standard concept name", "concept_id_2")%>%
    dplyr::rename("concept_id"="concept_id_2")  %>%
    dplyr::left_join(concept %>%
                dplyr::select("concept_id", "concept_name", "vocabulary_id", "concept_code")) %>%
    dplyr::filter(.data$vocabulary_id %in% source_vocabularies) %>%
  dplyr::select(-"concept_code") %>%
  arrange(concept_id_1) %>%
  dplyr::distinct()

mapped.codes<-mapped.codes %>%
  dplyr::rename("Standed code (mapped to)"="concept_id_1")%>%
  dplyr::rename("Source code (mapped from)"="concept_id")%>%
  dplyr::rename("Source name"="concept_name")

mapped.codes

}

