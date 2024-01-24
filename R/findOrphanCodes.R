
#' Find orphan codes related to a codelist
#'
#' @param x Codes for which to find codes related but not included (orphan
#' codes)
#' @param cdm cdm_reference via CDMConnector
#' @param domains Character vector with one or more of the OMOP CDM domain.
#' @param standardConcept  Character vector with one or more of "Standard",
#' "Classification", and "Non-standard". These correspond to the flags used
#' for the standard_concept field in the concept table of the cdm.
#' @param searchInSynonyms Either TRUE or FALSE. If TRUE the code will also
#' search using both the primary name in the concept table and synonyms from
#' the concept synonym table.
#' @param searchNonStandard Either TRUE or FALSE. If TRUE the code will also
#' search via non-standard concepts.
#' @param includeDescendants Either TRUE or FALSE.
#' If TRUE descendant concepts of identified concepts
#' will be included in the candidate codelist.
#' @param includeAncestor Either TRUE or FALSE.
#' If TRUE the direct ancestor concepts of identified concepts
#'  will be included in the candidate codelist.
#' @param minCellCount The minimum number of counts to reported, below which
#' results will be suppressed. If 0, all results will be reported.
#'
#' @return A codelist containing code related to (but not in) the target
#' codelist that are present used in the cdm
#' @export
#'
#' @examples
findOrphanCodes <- function(x,
                            cdm,
                            domains = "Condition",
                            standardConcept = "Standard",
                            searchInSynonyms = TRUE,
                            searchNonStandard = TRUE,
                            includeDescendants = TRUE,
                            includeAncestor = TRUE,
                            minCellCount = 5){


x <- addDetails(cdm = cdm, conceptList = x)

orphanConcepts <- list()
# rerun search
for(i in seq_along(x)){
candidateCodes <- getCandidateCodes(
    cdm = cdm,
    keywords = x[[i]]$concept_name,
    domains = domains,
    standardConcept = standardConcept,
    searchInSynonyms = searchInSynonyms,
    searchNonStandard = searchNonStandard,
    includeDescendants = includeDescendants,
    includeAncestor = includeAncestor)

# Exclude codes that are in the original set of codes
candidateCodes <- candidateCodes %>%
  dplyr::anti_join(x[[i]] %>%
                     dplyr::select("concept_id"),
                   by = "concept_id")
# Use achilles counts to summarise code use
orphanConcepts[[i]] <- achillesCodeUse(
  x = list("cs" = candidateCodes$concept_id),
  cdm = cdm,
  minCellCount = minCellCount
)
if(nrow(orphanConcepts[[i]]) >= 1 ){
  orphanConcepts[[i]] <- orphanConcepts[[i]] %>%
    dplyr::mutate(codelist = names(x)[i])
} else {
  cli::cli_inform("-- No orphan codes found for codelist {names(x)[i]}")
}
}

orphanConcepts <- dplyr::bind_rows(orphanConcepts)

orphanConcepts
}




