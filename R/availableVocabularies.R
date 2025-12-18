#' Get the available vocabularies available in the cdm
#'
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#' @inheritParams domainDoc
#'
#' @return Names of available vocabularies.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get all vocabularies available in the CDM
#' availableVocabularies(cdm)
#'
#' # Get all vocabularies available in the CDM for `Standard` and `Condition` concepts
#' availableVocabularies(cdm,
#'                       standardConcept = "Standard",
#'                       domain = "Condition")
#' }
availableVocabularies <- function(cdm,
                         standardConcept = "Standard",
                         domain = NULL) {

  # Initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  domain <- assertDomain(domain, cdm)
  standardConcept <- assertStandardConcept(standardConcept)

    vocabs <- cdm[["concept"]] |>
      dplyr::mutate(standard_concept = dplyr::case_when(
        is.na(.data$standard_concept) ~ "non-standard",
        .data$standard_concept == "C" ~ "classification",
        .data$standard_concept == "S" ~ "standard"
      )) |>
      dplyr::filter(.data$standard_concept %in% .env$standardConcept)

    if(!is.null(domain)){
      vocabs <- vocabs |>
        dplyr::filter(tolower(.data$domain_id) %in% .env$domain)
    }

    vocabs <- vocabs |>
      dplyr::select("vocabulary_id") |>
      dplyr::distinct() |>
      dplyr::pull("vocabulary_id") |>
      sort()

  return(vocabs)
}

#' Get the vocabularies associated with a codelist
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams standardConceptDoc
#' @inheritParams domainDoc
#'
#' @return Names of available vocabularies.
#' @export
#'
#' @examples
#' \donttest{
#' library(CodelistGenerator)
#' library(omock)
#'
#' # Create CDM object
#' cdm <- mockCdmReference()
#'
#' # Get all vocabularies from a codelist
#' codelist <- newCodelist(list("codes1" = c(35604877L, 35604394L),
#'                              "codes2" = c(4214687L)))
#' associatedVocabularies(cdm = cdm,
#'                       x = codelist)
#' }
associatedVocabularies <- function(x,
                                   cdm,
                                   standardConcept = "Standard",
                                   domain = NULL) {

  # Initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  domain <- assertDomain(domain, cdm)
  standardConcept <- assertStandardConcept(standardConcept)
  checkCodelist(x, allowConceptSetExpression = FALSE)

  # If codelist/codelist_with_details is provided
  vocabs <- addDetails(cdm = cdm,
                       conceptList = x)

  if(!is.null(domain)){
    vocabs <- purrr::map(vocabs, ~ dplyr::filter(.x, tolower(.data$domain_id) %in% .env$domain))
  }

  vocabs <- purrr::map(vocabs, ~ dplyr::filter(.x, .data$standard_concept %in% .env$standardConcept))

  vocabs <- purrr::map(vocabs, ~dplyr::pull(.x, "vocabulary_id") |> unique() |> sort())

  return(vocabs)
}

assertStandardConcept <- function(standardConcept, call = parent.frame()){
  omopgenerics::assertChoice(standardConcept,
                             c("Standard", "Non-standard", "Classification"),
                             call = call)
  return(invisible(tolower(standardConcept)))
}

assertDomain <- function(domains, cdm, call = parent.frame()){

  if(is.null(domains)){
    domains <- availableDomains(cdm)
  }else{
    omopgenerics::assertCharacter(domains, null = TRUE)
    domains <- stringr::str_to_sentence(domains)
    dif <- setdiff(domains, availableDomains(cdm))
    if(length(dif) > 0){
      cli::cli_warn("The domain{?s} {.val {dif}} {?is/are} not present in the cdm.", call = call)
    }
  }

  return(invisible(tolower(domains)))
}
