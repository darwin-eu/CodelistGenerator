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

#' Get the version of the vocabulary used in the cdm
#'
#' @inheritParams cdmDoc
#'
#' @return The vocabulary version being used in the cdm.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getVocabVersion(cdm = cdm)
#' }
getVocabVersion <- function(cdm) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  version <- as.character(cdm$vocabulary |>
    dplyr::rename_with(tolower) |>
    dplyr::filter(.data$vocabulary_id == "None") |>
    dplyr::select("vocabulary_version") |>
    dplyr::collect())
  return(version)
}

#' Get the domains available in the cdm
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#'
#' @return A vector with the domains of the cdm.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getDomains(cdm = cdm)
#' }
getDomains <- function(cdm,
                       standardConcept = "Standard") {

  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(standardConcept, c(
    "Standard",
    "Non-standard",
    "Classification"
    )
    )

  conceptDb <- cdm$concept

  standardConcept <- tolower(standardConcept)
  conceptDb <- conceptDb |>
    dplyr::mutate(
      standard_concept = ifelse(is.na(.data$standard_concept),
        "non-standard", .data$standard_concept
      )
    ) |>
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "C",
        "classification", .data$standard_concept
      )
    ) |>
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "S",
        "standard", .data$standard_concept
      )
    ) |>
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  domains <- conceptDb |>
    dplyr::select("domain_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull()

  return(domains)
}

#' Get the vocabularies available in the cdm
#'
#' @inheritParams cdmDoc
#'
#' @return Names of available vocabularies.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getVocabularies(cdm = cdm)
#' }

getVocabularies <- function(cdm) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)

  vocabs <- sort(cdm$concept |>
    dplyr::select("vocabulary_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull())

  return(vocabs)
}

#' Get the concept classes used in a given set of domains
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#' @inheritParams domainDoc
#'
#' @return The concept classes used for the requested domains.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getConceptClassId(cdm = cdm, domain = "drug")
#' }
getConceptClassId <- function(cdm,
                              standardConcept = "Standard",
                              domain = NULL) {
  #initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertChoice(standardConcept, c(
    "Standard",
    "Non-standard",
    "Classification"
  )
  )
  omopgenerics::assertCharacter(domain, null = T)

  # link to vocab table
  conceptDb <- cdm$concept

  if (!is.null(domain)) {
    conceptDb <- conceptDb |>
      dplyr::filter(tolower(.data$domain_id) == tolower(.env$domain))
  }

  standardConcept <- tolower(standardConcept)
  conceptDb <- conceptDb |>
    dplyr::mutate(
      standard_concept = ifelse(is.na(.data$standard_concept),
        "non-standard", .data$standard_concept
      )
    ) |>
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "C",
        "classification", .data$standard_concept
      )
    ) |>
    dplyr::mutate(
      standard_concept = ifelse(.data$standard_concept == "S",
        "standard", .data$standard_concept
      )
    ) |>
    dplyr::filter(.data$standard_concept %in% .env$standardConcept)

  # get overall version
  conceptClassId <- conceptDb |>
    dplyr::select("concept_class_id") |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull()

  conceptClassId <- sort(conceptClassId)

  return(conceptClassId)
}

#' Get the dose forms available for drug concepts
#'
#' @inheritParams cdmDoc
#'
#' @return The dose forms available for drug concepts.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getDoseForm(cdm = cdm)
#' }

getDoseForm <- function(cdm) {
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  rxDoseForm <- cdm$concept_relationship |>
    dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
    dplyr::select("concept_id_2") |>
    dplyr::rename("concept_id" = "concept_id_2") |>
    dplyr::distinct() |>
    dplyr::left_join(
      cdm$concept |>
        dplyr::select(
          "concept_id", "concept_name",
          "standard_concept"
        ),
      by = "concept_id"
    ) |>
    dplyr::collect() |>
    dplyr::pull("concept_name")

  rxDoseForm <- sort(rxDoseForm)

  return(rxDoseForm)
}

#' Get descendant codes for a given concept
#'
#' @inheritParams cdmDoc
#' @param conceptId concept_id to search
#' @param withAncestor If TRUE, return column with ancestor. In case of multiple
#' ancestors, concepts will be separated by ";".
#' @inheritParams ingredientRangeDoc
#' @inheritParams doseFormDoc
#'
#' @return The descendants of a given concept id.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getDescendants(cdm = cdm, conceptId = 1)
#' }

getDescendants <- function(cdm,
                           conceptId,
                           withAncestor = FALSE,
                           ingredientRange = c(0, Inf),
                           doseForm = NULL) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertNumeric(conceptId, integerish = T)
  omopgenerics::assertLogical(withAncestor)
  omopgenerics::assertNumeric(ingredientRange, length = 2, min = 0)
  omopgenerics::assertTrue(ingredientRange[1] <= ingredientRange[2])

  if(ingredientRange[2] == Inf){
    ingredientRange[2] <- 9999999
  }

if(isFALSE(withAncestor)){
  descendants <- getDescendantsOnly(cdm, conceptId, ingredientRange, doseForm)}

  if(isTRUE(withAncestor)){
    descendants <- getDescendantsAndAncestor(cdm, conceptId, ingredientRange, doseForm)}

  return(descendants)
}

getDescendantsOnly <- function(cdm, conceptId, ingredientRange, doseForm) {
  descendants <- cdm$concept_ancestor |>
    dplyr::filter(.data$ancestor_concept_id %in% .env$conceptId) |>
    dplyr::select("descendant_concept_id") |>
    dplyr::distinct() |>
    dplyr::rename("concept_id" = "descendant_concept_id") |>
    dplyr::left_join(cdm$concept,
                     by = "concept_id")

  if(ingredientRange[1] != 0 &&
     ingredientRange[2] != 9999999){
  descendants <- addIngredientCount(cdm = cdm, concepts = descendants) |>
    dplyr::filter(.data$ingredient_count >= !!.env$ingredientRange[1],
                  .data$ingredient_count <= !!.env$ingredientRange[2]) |>
    dplyr::select(!c("ingredient_count"))
  }

  if(!is.null(doseForm)){
    descendantDoseForms <- getPresentDoseForms(cdm, concepts = descendants)
  }

  descendants <- descendants  |>
    dplyr::collect()

  if(!is.null(doseForm)){
    descendants <-  filterOnDoseForm(concepts = descendants,
                       conceptDoseForms = descendantDoseForms,
                     doseForm = doseForm)
  }

  # nb conceptId will also be a descendant of itself (if we don't specify dose)
  return(descendants)
}

getDescendantsAndAncestor <- function(cdm, conceptId, ingredientRange, doseForm) {
  conceptIdDbTable <- omopgenerics::uniqueTableName()
  cdm <- omopgenerics::insertTable(cdm = cdm,
                            name = conceptIdDbTable,
                            table = dplyr::tibble(ancestor_concept_id = as.integer(conceptId)),
                            overwrite = TRUE)

  descendants <- cdm$concept_ancestor |>
    dplyr::inner_join(cdm[[conceptIdDbTable]],
                      by = "ancestor_concept_id") |>
    dplyr::rename("concept_id" = "descendant_concept_id") |>
    dplyr::left_join(cdm$concept,
                     by = "concept_id") |>
    dplyr::compute()

  descendants <- addIngredientCount(cdm = cdm, concepts = descendants) |>
    dplyr::filter(.data$ingredient_count >= !!ingredientRange[1],
                  .data$ingredient_count <= !!ingredientRange[2]) |>
    dplyr::select(!c("ingredient_count"))

  if(!is.null(doseForm) &&
     nrow(descendants |>
     utils::head(5) |>
     dplyr::tally() |>
     dplyr::collect()) > 0){
    descendantDoseForms <- getPresentDoseForms(cdm, concepts = descendants)
  }

  descendants <- descendants |>
    dplyr::collect() |>
    dplyr::mutate(name = paste0("concept_", .data$ancestor_concept_id))

    if(nrow(descendants)>0){
descendants <- descendants |>
        tidyr::pivot_wider(names_from = "name",
                           values_from = "ancestor_concept_id")

  # one row per concept, with ancestor (of which there may be multiple)
  working_cols <- stringr::str_subset(string = colnames(descendants),
                                      pattern = paste(c(colnames(cdm$concept),
                                                        colnames(cdm$concept_ancestor)),
                                                      collapse = "|"),
                                      negate = TRUE)

descendants <- descendants |>
     tidyr::unite(col="ancestor_concept_id",
                  dplyr::all_of(working_cols), sep=";")
# quicker to replace NAs afterwards rather than inside unite
# (especially when there are many columns)
descendants$ancestor_concept_id <- stringr::str_replace_all(
  string = descendants$ancestor_concept_id,
  pattern = ";NA|NA;",
  replacement = ""
)
    }

  if(!is.null(doseForm) &&
     nrow(descendants |>
          utils::head(5) |>
          dplyr::tally() |>
          dplyr::collect()) > 0){
    descendants <-  filterOnDoseForm(concepts = descendants,
                                     conceptDoseForms = descendantDoseForms,
                                     doseForm = doseForm)
  }

  CDMConnector::dropTable(cdm, conceptIdDbTable)

  # nb conceptId will also be a descendant of itself
  return(descendants)

}

getPresentDoseForms <- function(cdm, concepts){

  presentDoseForms <- concepts |>
    dplyr::left_join(
      cdm$concept_relationship |>
        dplyr::filter(.data$relationship_id == "RxNorm has dose form") |>
        dplyr::select("concept_id_1", "concept_id_2") |>
        dplyr::rename("concept_id" = "concept_id_2") |>
        dplyr::distinct() |>
        dplyr::left_join(cdm$concept, by = "concept_id") |>
        dplyr::select("concept_id_1", "concept_name") |>
        dplyr::rename("concept_id"="concept_id_1",
                      "dose_form"="concept_name")  ,
      by ="concept_id"
    ) |>
    dplyr::select("concept_id", "dose_form") |>
    dplyr::collect()

  presentDoseForms <- presentDoseForms |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::mutate(seq = dplyr::row_number()) |>
    tidyr::pivot_wider(
      names_from = "seq",
      values_from = "dose_form"
    )
  presentDoseForms <- presentDoseForms |>
    tidyr::unite(
      col = "dose_form", 2:ncol(presentDoseForms), sep = "; ",
      na.rm = TRUE
    )
  return(presentDoseForms)

}

filterOnDoseForm <- function(concepts, conceptDoseForms, doseForm){
  concepts <- concepts |>
    dplyr::inner_join(
      conceptDoseForms |>
        dplyr::filter(stringr::str_detect(
          string = tolower(.data$dose_form),
          pattern = paste(tolower(.env$doseForm),
                          collapse = "|"
          )
        )) |>
        dplyr::select("concept_id"),
      by = "concept_id")

  return(concepts)

}

addIngredientCount <- function(cdm, concepts) {
 ingredient_ancestor <- cdm$concept_ancestor |>
    dplyr::inner_join(cdm$concept |>
                        dplyr::filter(.data$concept_class_id == "Ingredient",
                                      .data$standard_concept == "S") |>
                        dplyr::select("concept_id"),
               by = c("ancestor_concept_id" = "concept_id"))

 ingredient_count <- concepts |>
   dplyr::select("concept_id") |>
   dplyr::distinct() |>
   dplyr::left_join(ingredient_ancestor,
              by = c("concept_id" = "descendant_concept_id")) |>
    dplyr::select("concept_id") |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::tally(name = "ingredient_count") |>
    dplyr::mutate(ingredient_count = as.integer(ingredient_count))

 concepts <- concepts |>
   dplyr::left_join(ingredient_count,
             by = "concept_id")
 if(!is.null(attr(cdm, "dbcon"))){
   concepts <- concepts |>
   dplyr::compute()}

 concepts
}


#' Get available relationships between concepts
#'
#' @inheritParams cdmDoc
#' @param standardConcept1  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param standardConcept2  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param domains1 Character vector with one or more of the OMOP CDM domain.
#' @param domains2 Character vector with one or more of the OMOP CDM domain.
#'
#' @return A character vector with unique concept relationship values.
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockVocabRef()
#' getRelationshipId(cdm = cdm)
#' }
getRelationshipId <- function(cdm,
                              standardConcept1 = "standard",
                              standardConcept2 = "standard",
                              domains1 = "condition",
                              domains2 = "condition") {

  if (!"cdm_reference" %in% class(cdm)) {
    cli::cli_abort("cdm must be a cdm_reference")
  }
  if (!all(tolower(standardConcept1) %in% c("standard", "non-standard", "classification"))) {
    cli::cli_abort(
      paste0(
        "standardConcept1 must be one or more of",
        "standard, non-standard, and classification"
      )
    )
  }
  if (!all(tolower(standardConcept2) %in% c("standard", "non-standard", "classification"))) {
    cli::cli_abort(
      paste0(
        "standardConcept2 must be one or more of",
        "standard, non-standard, and classification"
      )
    )
  }
  if (!is.character(domains1)) {
    cli::cli_abort("domains1 must be a character vector")
  }
  if (!is.character(domains2)) {
    cli::cli_abort("domains2 must be a character vector")
  }


  standardConcept1 <- tolower(standardConcept1)
  standardConcept2 <- tolower(standardConcept2)
  domains1 <- tolower(domains1)
  domains2 <- tolower(domains2)

  cdm[["concept"]] <- cdm[["concept"]] |>
    dplyr::mutate(
      domain_id = tolower(.data$domain_id),
      standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard",
        .default = as.character(.data$standard_concept)
      )
    )

  sort(
    cdm[["concept_relationship"]] |>
      dplyr::left_join(
        cdm[["concept"]]  |>
          dplyr::select(
            "concept_id",
            "domain_id_1" = "domain_id",
            "standard_concept_1" = "standard_concept"
          ),
        by = c("concept_id_1" = "concept_id")
      ) |>
      dplyr::left_join(
        cdm[["concept"]] |>
          dplyr::select(
            "concept_id",
            "domain_id_2" = "domain_id",
            "standard_concept_2" = "standard_concept"
          ),
        by = c("concept_id_2" = "concept_id")
      ) |>
      dplyr::filter(
        .data$standard_concept_1 %in% .env$standardConcept1,
        .data$standard_concept_2 %in% .env$standardConcept2,
        .data$domain_id_1 %in% .env$domains1,
        .data$domain_id_2 %in% .env$domains2
      ) |>
      dplyr::select("relationship_id") |>
      dplyr::distinct() |>
      dplyr::pull()
  )

}
