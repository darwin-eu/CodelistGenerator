
#' Generate candidate codelist for the OMOP CDM
#'
#' @description
#' This function generates a set of codes that can be considered for creating a phenotype
#' using the OMOP CDM.
#'
#' @param keywords Character vector of words to search for. Where more than one word is given (e.g. "knee osteoarthritis"), all words will be identified but can be in different positions (e.g. "osteoarthritis of knee") should be identified.
#' @param domains  Character vector with one or more of the OMOP CDM domain (e.g. "Condition").
#' @param search.synonyms Either TRUE or FALSE. If TRUE the code will also search via the concept synonym table.
#' @param fuzzy.match Either TRUE or FALSE. If TRUE the fuzzy matches will be used, with approximate matches identified.
#' @param fuzzy.match.max.distance The max.distance parmeter for fuzzy matching (see ??base::agrep for further details).
#' @param exclude  Character vector of words to search for to identify concepts to exclude.
#' @param include.descendants Either TRUE or FALSE. If TRUE descendant concepts of identified concepts will be included in the candidate codelist.
#' @param include.ancestor Either TRUE or FALSE. If TRUE the direct ancestor concepts of identified concepts will be included in the candidate codelist.
#' @param db Database connection via DBI::dbConnect()
#' @param vocabulary_schema Name of database schema with vocab tables
#'
#' @return Dataframe
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' ### note, Eunomia, used in the example below, does not include a full set of vocabularies. The full set can be downloaded from https://athena.ohdsi.org
#'library(Eunomia)
#'library(DBI)
#'library(RSQLite)
#'untar(xzfile(system.file("sqlite", "cdm.tar.xz", package = "Eunomia"), open = "rb"),
#'        exdir =  tempdir())
#'db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(),"\\cdm.sqlite"))
#'vocabulary_database_schema<-"main"
#'get_candidate_codes(keywords="asthma",
#'                    db=db,
#'                    vocabulary_schema = "main")
#'get_candidate_codes(keywords="asthma",
#'                    search.synonyms=TRUE,
#'                              fuzzy.match=TRUE,
#'                              exclude=NULL,
#'                              include.descendants=TRUE,
#'                              include.ancestor=FALSE,
#'                    db=db,
#'                    vocabulary_database_schema = "main")
#'
get_candidate_codes<-function(keywords,
                              domains=c("Condition", "Drug" ,"Device", "Observation",
                                        "Procedure"),
                              search.synonyms=FALSE,
                              fuzzy.match=FALSE,
                              fuzzy.match.max.distance=0.1,
                              exclude=NULL,
                              include.descendants=TRUE,
                              include.ancestor=FALSE,
                              db,
                              vocabulary_database_schema){

start<-Sys.time()

errorMessage <- checkmate::makeAssertCollection()

checkmate::assertVector(keywords, add = errorMessage)
checkmate::assertVector(exclude,null.ok = TRUE, add = errorMessage)

# checkmate::assertDataFrame(concept, add = errorMessage)
# checkmate::assertDataFrame(concept_synonym, add = errorMessage)
# checkmate::assertDataFrame(concept_ancestor, add = errorMessage)

checkmate::reportAssertions(collection = errorMessage)

# connect to relevant vocabulary tables
concept_db<-dplyr::tbl(db, dplyr::sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept")))
concept_ancestor_db<-dplyr::tbl(db, dplyr::sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_ancestor")))
concept_synonym_db<-dplyr::tbl(db, dplyr::sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_synonym")))
# lowercase names
concept_db<-dplyr::rename_with(concept_db, tolower)
concept_ancestor_db<-dplyr::rename_with(concept_ancestor_db, tolower)
concept_synonym_db<-dplyr::rename_with(concept_synonym_db, tolower)

# filter to only relevant data
# will use dtplyr for these
print("Limiting to potential concepts of interest (database side)")
concept_db<-concept_db %>%
  dplyr::filter(.data$domain_id %in% domains) %>%
  dplyr::filter(.data$standard_concept=="S") %>%
  dplyr::compute()
concept_ancestor_db1<-concept_db %>%
                dplyr::select("concept_id") %>%
                dplyr::rename("ancestor_concept_id"="concept_id") %>%
                dplyr::inner_join(concept_ancestor_db,
                          by="ancestor_concept_id") %>%
  dplyr::compute()
concept_ancestor_db2<-concept_db %>%
                dplyr::select("concept_id") %>%
                dplyr::rename("descendant_concept_id"="concept_id") %>%
                dplyr::inner_join(concept_ancestor_db,
                          by="descendant_concept_id") %>%
  dplyr::compute()
concept_synonym_db<-concept_db %>%
                dplyr::select("concept_id") %>%
  dplyr::inner_join(concept_synonym_db, by="concept_id") %>%
  dplyr::compute()

print("Bringing filtered tables into memory")
concept<-concept_db %>% dplyr::collect()
concept_ancestor<-dplyr::bind_rows(concept_ancestor_db1 %>% dplyr::collect(),
                            concept_ancestor_db2 %>% dplyr::collect()) %>%
  dplyr::distinct()
concept_synonym<-concept_synonym_db %>% dplyr::collect()
rm(concept_db,concept_ancestor_db,concept_ancestor_db1,concept_ancestor_db2,concept_synonym_db)

# 1) codes to exclude
# will anti_join throughought to make sure these don't appear
# exact matches only

if(length(exclude)>0){
print("Getting concepts to exclude")
# Get standard, condition concepts which include one of the exclusion words
exclude<-clean_words(exclude)

exclude.codes<-lapply(seq_along(exclude), function(i) {
working.exclude<-unlist(strsplit(exclude[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

working.concepts %>%
  dplyr::filter(apply(sapply(X = working.exclude,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all)) %>%
  dplyr::distinct()
})
exclude.codes<-dplyr::bind_rows(exclude.codes)
}

# 2) Get standard, condition concepts which include one of the keywords
print("Getting concepts to include from exact matches")

keywords<-clean_words(keywords)

# because there may be a lot of synonyms, get these from a loop
# (stringr::str_detect slows considerably as more options are added in a single call using "|")
# where multiple words, split up and search (i.e. they don´t need to be next to each other)
candidate.codes.list<-list()
for(i in 1:length(keywords)){

working.keywords<-unlist(strsplit(keywords[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

candidate.codes.list[[i]]<-working.concepts %>%
  dplyr::filter(apply(sapply(X = working.keywords,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all))
}
candidate.codes<-dplyr::bind_rows(candidate.codes.list) %>%
  dplyr::distinct()

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

# 2) use fuzzy match to include
if(fuzzy.match==TRUE){
  print("Getting concepts to include from fuzzy matches")

candidate.codes.fuzzy<-list()
for(i in 1:length(keywords)){
working.keywords<-unlist(strsplit(keywords[i]," "))
working.concepts<-concept %>%  # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

for(j in 1:length(working.keywords)){ # dplyr::filter each term
indx<-agrep(working.keywords[j], working.concepts$concept_name, max.distance = fuzzy.match.max.distance)
working.concepts<-working.concepts[indx,]
}

candidate.codes.fuzzy[[i]]<-working.concepts
}
candidate.codes.fuzzy<-dplyr::bind_rows(candidate.codes.fuzzy) %>%
  dplyr::distinct()

candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.codes.fuzzy) %>%
  dplyr::distinct()

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}

if(nrow(candidate.codes)==0){
candidate.codes
message("-- No codes found for given keywords")
} else {
# 4) look for any standard, condition concepts with a synonym of the
# codes found from the keywords
if(search.synonyms==TRUE){
print("Getting concepts to include from exact matches of synonyms")

synonyms<-dtplyr::lazy_dt(concept_synonym) %>%
  dplyr::inner_join(dtplyr::lazy_dt(candidate.codes) %>%
                      dplyr::select("concept_id")) %>%
  as.data.frame() %>%
  dplyr::select("concept_synonym_name") %>%
  dplyr::distinct() %>%
  dplyr::pull()
# drop any long synonyms (more than 6 words) - add a lot of run time while being highly unlikely to have a match
synonyms<-synonyms[stringr::str_count(synonyms, "\\S+")<=6]
synonyms<-unique(clean_words(synonyms))

working.concepts<-concept %>% # start with all
  dplyr::mutate(concept_name=clean_words(.data$concept_name))

# get these from a loop
synonym.codes.list<-lapply(seq_along(synonyms), function(i) {
working.synonyms<-unlist(strsplit(synonyms[i]," "))
synonym.codes.list<-working.concepts %>%
  dplyr::filter(apply(sapply(X = working.synonyms,
       FUN = grepl, working.concepts$concept_name),
      MARGIN =  1, FUN = all))
})
synonym.codes<-dplyr::bind_rows(synonym.codes.list) %>% dplyr::distinct()

candidate.codes<-dplyr::bind_rows(candidate.codes, synonym.codes) %>%
  dplyr::distinct()
rm(synonyms,synonym.codes,synonym.codes.list)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}




# 5) add any codes lower in the hierachy (and deduplicate)
if(include.descendants==TRUE){
print("Getting concepts to include from descendants of identified concepts")

candidate.code.descendants <-  dtplyr::lazy_dt(candidate.codes %>%
   dplyr::select("concept_id") %>%
   dplyr::rename("ancestor_concept_id"="concept_id")  %>%
   dplyr::distinct()) %>%
   dplyr::left_join(dtplyr::lazy_dt(concept_ancestor %>%
                      dplyr::filter("ancestor_concept_id"!="descendant_concept_id")),
   by = "ancestor_concept_id") %>%
  as.data.frame() %>%
   dplyr::select("descendant_concept_id")  %>%
   dplyr::distinct() %>%
   dplyr::rename("concept_id"="descendant_concept_id")

candidate.code.descendants<-dtplyr::lazy_dt(candidate.code.descendants) %>%
   dplyr::left_join(dtplyr::lazy_dt(concept), by = "concept_id" ) %>%
   as.data.frame() %>%
   dplyr::mutate(concept_name=clean_words(.data$concept_name))

candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.code.descendants) %>%
  dplyr::distinct()
rm(candidate.code.descendants)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}

# 5) add any codes one level above in the hierachy (and deduplicate)
if(include.ancestor==TRUE){
print("Getting concepts to include from direct ancestors of identified concepts")

candidate.code.ancestor <- dtplyr::lazy_dt(candidate.codes) %>%
   dplyr::left_join( dtplyr::lazy_dt(concept_ancestor  %>%
   dplyr::filter(.data$min_levels_of_separation==1)  %>%
   dplyr::select("ancestor_concept_id")  %>%
   dplyr::rename("concept_id"="ancestor_concept_id") ) ,
   by = "concept_id") %>%
   as.data.frame() %>%
   dplyr::mutate(concept_name=clean_words(.data$concept_name))

# only if not already in candidate.codes
candidate.code.ancestor<-candidate.code.ancestor %>%
  dplyr::anti_join(candidate.codes %>% dplyr::select("concept_id"),
             by = "concept_id")%>%
   dplyr::left_join(concept, by = "concept_id")

candidate.codes<-dplyr::bind_rows(candidate.codes, candidate.code.ancestor) %>%
  dplyr::distinct()
rm(candidate.code.ancestor)

if(length(exclude)>0){
if(nrow(exclude.codes)>0){
candidate.codes<-candidate.codes %>%
  dplyr::anti_join(exclude.codes %>% dplyr::select("concept_id"),
                   by = "concept_id")
}}

}


# get original names back
candidate.codes<-candidate.codes %>%
  dplyr::select(.data$concept_id) %>%
  dplyr::left_join(concept,
            by= c("concept_id")) %>%
  dplyr::distinct()

candidate.codes<- candidate.codes %>%
  dplyr::select("concept_id", "concept_name", "domain_id", "vocabulary_id")

x <- abs(as.numeric(Sys.time()-start, units="secs"))
print(paste0("Getting candidate codelist took ",
             floor(x/60), " minutes and ",  x %% 60 %/% 1, " seconds"))
candidate.codes %>%
  dplyr::distinct() # return
}

}


.datatable.aware <- TRUE
