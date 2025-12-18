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

#' Summarise code use in patient-level data.
#'
#' @inheritParams xDoc
#' @inheritParams cdmDoc
#' @inheritParams countByDoc
#' @inheritParams byConceptDoc
#' @inheritParams bySexDoc
#' @inheritParams byYearDoc
#' @inheritParams ageGroupDoc
#' @param dateRange Two dates. The first indicating the earliest cohort start
#' date and the second indicating the latest possible cohort end date. If NULL
#' or the first date is set as missing, the earliest observation_start_date in
#' the observation_period table will be used for the former. If NULL or the
#' second date is set as missing, the latest observation_end_date in the
#' observation_period table will be used for the latter.
#' @param useSourceCodes Whether the codelist provided contains source codes (TRUE) or standard codes (FALSE).
#'
#' @return A tibble with count results overall and, if specified, by strata.
#' @export
#'
#' @examples
#' \dontrun{
#' library(omopgenerics)
#' library(CodelistGenerator)
#' con <- DBI::dbConnect(duckdb::duckdb(),
#'                       dbdir = CDMConnector::eunomiaDir())
#' cdm <- CDMConnector::cdmFromCon(con,
#'                                 cdmSchema = "main",
#'                                 writeSchema = "main")
#' acetiminophen <- c(1125315,  1127433, 40229134,
#'                   40231925, 40162522, 19133768,  1127078)
#' poliovirus_vaccine <- c(40213160)
#' cs <- newCodelist(list(acetiminophen = acetiminophen,
#'                        poliovirus_vaccine = poliovirus_vaccine))
#' results <- summariseCodeUse(cs,cdm = cdm)
#' results
#' CDMConnector::cdmDisconnect(cdm)
#'}
#'
summariseCodeUse <- function(x,
                             cdm,
                             countBy = c("record", "person"),
                             byConcept = TRUE,
                             byYear = FALSE,
                             bySex = FALSE,
                             ageGroup = NULL,
                             dateRange = as.Date(c(NA, NA)),
                             useSourceCodes = FALSE){

  checkCodelist(x, allowConceptSetExpression = FALSE)


  if(inherits(x, "codelist_with_details")){
    x <- asCodelist(x)
  }

  if(length(x) == 0){
    return(omopgenerics::emptySummarisedResult())
  }

  codeUse <- list()
  for(i in seq_along(x)){
    cli::cli_inform("Getting use of codes from {names(x)[i]} ({i} of {length(x)})")
    codeUse[[i]] <- getCodeUse(x[i],
                               cdm = cdm,
                               cohortTable = NULL,
                               cohortId = NULL,
                               timing = "any",
                               countBy = countBy,
                               byConcept = byConcept,
                               byYear = byYear,
                               bySex = bySex,
                               ageGroup = ageGroup,
                               dateRange = dateRange,
                               useSourceCodes = useSourceCodes)
  }
  codeUse <- dplyr::bind_rows(codeUse)

  if(nrow(codeUse) > 0) {

    if(is.na(dateRange[1])){
      dateRangeStart <- "none"
    } else {
      dateRangeStart <- as.character(dateRange[1])
    }
    if(is.na(dateRange[2])){
      dateRangeEnd <- "none"
    } else {
      dateRangeEnd <- as.character(dateRange[2])
    }

    codeUse <- codeUse |>
      dplyr::mutate(
        result_id = 1L,
        cdm_name = omopgenerics::cdmName(cdm)
      ) |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(
          result_id = 1L,
          result_type = "code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion("CodelistGenerator")),
          date_range_start = .env$dateRangeStart,
          date_range_end   = .env$dateRangeEnd
        )
      )
  } else {
    codeUse <- omopgenerics::emptySummarisedResult()
  }

  return(codeUse)

}

#' Summarise code use among a cohort in the cdm reference
#'
#' @inheritParams cdmDoc
#' @param cohortTable A cohort table from the cdm reference.
#' @inheritParams xDocCohort
#' @param cohortId A vector of cohort IDs to include
#' @param timing When to assess the code use relative cohort dates. This can
#' be "any"(code use any time by individuals in the cohort) or  "entry" (code
#' use on individuals' cohort start date).
#' @inheritParams countByDoc
#' @inheritParams byConceptDoc
#' @inheritParams bySexDoc
#' @inheritParams byYearDoc
#' @inheritParams ageGroupDoc
#' @param useSourceCodes Whether the codelist provided contains source codes
#' (TRUE) or standard codes (FALSE).
#'
#' @return A tibble with results overall and, if specified, by strata
#' @export
#'
#' @examples
#' \dontrun{
#' library(CodelistGenerator)
#' library(duckdb)
#' library(DBI)
#' library(CDMConnector)
#' con <- dbConnect(duckdb(),
#'                  dbdir = eunomiaDir())
#' cdm <- cdmFromCon(con,
#'                  cdmSchema = "main",
#'                  writeSchema = "main")
#' cdm <- generateConceptCohortSet(cdm = cdm,
#'                   conceptSet = list(a = 260139,
#'                                     b = 1127433),
#'                   name = "cohorts",
#'                   end = "observation_period_end_date",
#'                   overwrite = TRUE)
#'
#'results_cohort_mult <-
#'summariseCohortCodeUse(omopgenerics::newCodelist(list(cs = c(260139,19133873))),
#'                       cdm = cdm,
#'                       cohortTable = "cohorts",
#'                       timing = "entry")
#'
#'results_cohort_mult
#'CDMConnector::cdmDisconnect(cdm)
#'}
summariseCohortCodeUse <- function(cdm,
                                   cohortTable,
                                   x = NULL,
                                   cohortId = NULL,
                                   timing = "any",
                                   countBy = c("record", "person"),
                                   byConcept = TRUE,
                                   byYear = FALSE,
                                   bySex = FALSE,
                                   ageGroup = NULL,
                                   useSourceCodes = FALSE
                                   ) {

  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertTrue(inherits(cdm[[cohortTable]], "GeneratedCohortSet"))
  omopgenerics::assertTrue(all(c("cohort_definition_id", "subject_id", "cohort_start_date",
                               "cohort_end_date") %in% colnames(cdm[[cohortTable]])))

  if(is.null(cohortId)){
    cohortId <- sort(CDMConnector::settings(cdm[[cohortTable]]) |>
                       dplyr::pull("cohort_definition_id"))
  } else {
    cohortId <- omopgenerics::validateCohortIdArgument(cohortId = cohortId,
                                                       cohort = cdm[[cohortTable]])
    }

  checkCodelist(x, allowConceptSetExpression = FALSE)

  if(!is.null(x)){
    if(inherits(x, "codelist_with_details")){
      x <- asCodelist(x)
    }
    if(length(x) == 0){
      return(omopgenerics::emptySummarisedResult())
    }
    # if x is null we'll use cohort codelist attribute
    # otherwise will use codelist specified
    settings <- expand.grid(codelist_name = names(x),
                         cohort_definition_id = omopgenerics::settings(cdm[[cohortTable]]) |>
                           dplyr::pull("cohort_definition_id")) |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::left_join(omopgenerics::settings(cdm[[cohortTable]]) |>
                         dplyr::select("cohort_definition_id", "cohort_name"),
                       by = "cohort_definition_id") |>
      dplyr::arrange(.data$cohort_name) |>
      dplyr::mutate(codelist_name = as.character(.data$codelist_name))
  } else {
    x <- attr(cdm[[cohortTable]], "cohort_codelist") |>
      dplyr::select("codelist_name", "concept_id") |>
      dplyr::distinct()

    settings <- attr(cdm[[cohortTable]], "cohort_codelist") |>
      dplyr::select("cohort_definition_id", "codelist_name") |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::filter(.data$cohort_definition_id %in% .env$cohortId) |>
      dplyr::left_join(omopgenerics::settings(cdm[[cohortTable]]) |>
                         dplyr::select("cohort_definition_id", "cohort_name"),
                       by = "cohort_definition_id") |>
      dplyr::arrange(.data$cohort_name) |>
      dplyr::mutate(codelist_name = as.character(.data$codelist_name))
  }


  cohortCodeUse <- list()
  for(i in seq_along(settings$codelist_name)){
    workingCohortName <- settings$cohort_name[[i]]
    workingCohortId <- settings$cohort_definition_id[[i]]
    workingCodelistName <- settings$codelist_name[[i]]

    if(inherits(x, "cdm_table")){
      workingCodelist <- x |>
        dplyr::filter(.data$codelist_name == !!workingCodelistName) |>
        dplyr::select("concept_id")
      workingCodelist <- list(workingCodelist) |>
        stats::setNames(workingCodelistName)
    } else {
    workingCodelist <- x[workingCodelistName]
    }

    cli::cli_inform(" Getting counts of {names(workingCodelist)} codes for cohort {workingCohortName}")
     cohortCodeUse[[i]] <- getCodeUse(workingCodelist,
                                     cdm = cdm,
                                     cohortTable = cohortTable,
                                     cohortId = workingCohortId,
                                     timing = timing,
                                     countBy = countBy,
                                     byConcept = byConcept,
                                     byYear = byYear,
                                     bySex = bySex,
                                     ageGroup = ageGroup,
                                     dateRange = as.Date(c(NA,NA)),
                                     useSourceCodes = useSourceCodes)
    }
  cohortCodeUse <- dplyr::bind_rows(cohortCodeUse) |>
    dplyr::arrange(dplyr::across(!c("variable_level", "estimate_value")))

  if (nrow(cohortCodeUse) > 0) {
    cohortCodeUse <- cohortCodeUse |>
      dplyr::mutate(
        result_id = 1L,
        cdm_name = omopgenerics::cdmName(cdm)
      ) |>
      omopgenerics::newSummarisedResult(
        settings = dplyr::tibble(

          result_id = 1L,
          result_type = "cohort_code_use",
          package_name = "CodelistGenerator",
          package_version = as.character(utils::packageVersion("CodelistGenerator")),
          timing = timing
        )
      )
  } else {
    cohortCodeUse <- omopgenerics::emptySummarisedResult()
  }

  return(cohortCodeUse)
}

getCodeUse <- function(x,
                       cdm,
                       cohortTable,
                       cohortId,
                       timing,
                       countBy,
                       byConcept,
                       byYear,
                       bySex,
                       ageGroup,
                       dateRange,
                       useSourceCodes,
                       call = parent.frame()) {

  # initial checks
  cdm <- omopgenerics::validateCdmArgument(cdm = cdm)
  omopgenerics::assertCharacter(timing, length = 1)
  omopgenerics::assertChoice(timing, choices = c("any","entry"))
  omopgenerics::assertChoice(countBy, choices = c("record", "person"))
  if(!inherits(x[[1]], "cdm_table")){
    omopgenerics::assertList(x)
    omopgenerics::assertNumeric(x[[1]], integerish = TRUE)
  }
  omopgenerics::assertLogical(byConcept)
  omopgenerics::assertLogical(byYear)
  omopgenerics::assertLogical(bySex)
  checkAgeGroup(ageGroup = ageGroup)
  omopgenerics::assertDate(dateRange, length = 2, na = TRUE)

  if(is.null(attr(cdm, "write_schema"))){
    cli::cli_abort("cdm must have a write_schema specified",
                   call = call)
  }

  tableCodelist <- paste0(omopgenerics::uniqueTableName(),
                          omopgenerics::uniqueId())
  if(inherits(x[[1]], "cdm_table")){
    cdm[[tableCodelist]] <- x[[1]] |>
      dplyr::left_join(
        cdm[["concept"]] |> dplyr::select("concept_id", "domain_id"),
        by = "concept_id") |>
      dplyr::compute(name = tableCodelist,
                     overwrite = TRUE,
                     temporary = FALSE,
                     logPrefix = "CodelistGenerator.getCodeUse_joinConcept")
  } else {
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodelist,
                                     table = dplyr::tibble(concept_id = x[[1]]),
                                     overwrite = TRUE,
                                     temporary = FALSE)
    cdm[[tableCodelist]] <- cdm[[tableCodelist]] |>
      dplyr::left_join(
        cdm[["concept"]] |> dplyr::select("concept_id", "domain_id"),
        by = "concept_id")
  }
  tableDomainsData <- paste0(omopgenerics::uniqueTableName(),
                             omopgenerics::uniqueId())
  cdm <- omopgenerics::insertTable(cdm = cdm,
                                   name = tableDomainsData,
                                   table = conceptDomainsData,
                                   overwrite = TRUE,
                                   temporary = FALSE)

  cdm[[tableCodelist]] <- cdm[[tableCodelist]] |>
    dplyr::mutate(domain_id = tolower(.data$domain_id)) |>
    dplyr::left_join(cdm[[tableDomainsData]],
                     by = "domain_id") |>
    dplyr::compute(name = tableCodelist,
                   temporary = FALSE,
                   overwrite = TRUE,
                   logPrefix = "CodelistGenerator.getCodeUse_conceptsDomains")
  omopgenerics::dropSourceTable(cdm = cdm, name = tableDomainsData)
  cdm[[tableDomainsData]] <- NULL

  intermediateTable <- paste0(omopgenerics::uniqueTableName(),
                              omopgenerics::uniqueId())
  records <- getRelevantRecords(cdm = cdm,
                                tableCodelist = tableCodelist,
                                cohortTable = cohortTable,
                                cohortId = cohortId,
                                timing = timing,
                                intermediateTable = intermediateTable,
                                useSourceCodes = useSourceCodes)

  if(!is.null(records) &&
     (records |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") > 0)) {

    if((!is.na(dateRange[1])) | (!is.na(dateRange[2]))){
      records <- filterDateRange(records, cdm, dateRange)
    }

    if(bySex == TRUE | !is.null(ageGroup)){
      records <- records |>
        PatientProfiles::addDemographics(age = !is.null(ageGroup),
                                         ageGroup = ageGroup,
                                         sex = bySex,
                                         priorObservation = FALSE,
                                         futureObservation =  FALSE,
                                         indexDate = "start_date",
                                         name =  omopgenerics::tableName(records))
    }

    byAgeGroup <- !is.null(ageGroup)
    codeCounts <- getSummaryCounts(records = records,
                                   cdm = cdm,
                                   countBy = countBy,
                                   byConcept = byConcept,
                                   byYear = byYear,
                                   bySex = bySex,
                                   byAgeGroup = byAgeGroup)

    if (is.null(cohortTable)) {
      cohortName <- NA
    } else {
      cohortName <- omopgenerics::settings(cdm[[cohortTable]]) |>
        dplyr::filter(.data$cohort_definition_id == cohortId) |>
        dplyr::pull("cohort_name")
    }
    codeCounts <-  codeCounts |>
      dplyr::mutate(
        "codelist_name" := !!names(x),
        "cohort_name" = .env$cohortName,
        "estimate_type" = "integer",
        "variable_name" = dplyr::if_else(is.na(.data$standard_concept_name), "overall", .data$standard_concept_name),
        "variable_level" = as.character(.data$standard_concept_id)
      ) |>
      omopgenerics::uniteGroup(cols = c("cohort_name", "codelist_name")) |>
      omopgenerics::uniteAdditional(
        cols = c("source_concept_name", "source_concept_id",
                 "source_concept_value", "type_concept_id", "type_concept_name",
                 "domain_id", "table"),
        ignore = "overall"
      ) |>
      dplyr::select(
        "group_name", "group_level", "strata_name", "strata_level",
        "variable_name", "variable_level", "estimate_name", "estimate_type",
        "estimate_value", "additional_name", "additional_level"
      )

  } else {
    codeCounts <- omopgenerics::emptySummarisedResult()
    cli::cli_inform(c(
      "i" = "No records found in the cdm for the concepts provided."
    ))
  }

  omopgenerics::dropSourceTable(cdm = cdm,
                          name = tableCodelist)
  cdm[[tableCodelist]] <- NULL
  omopgenerics::dropSourceTable(
    cdm = cdm,
    name = dplyr::starts_with(intermediateTable)
  )

  return(codeCounts)
}


getRelevantRecords <- function(cdm,
                               tableCodelist,
                               cohortTable,
                               cohortId,
                               timing,
                               intermediateTable,
                               useSourceCodes){

  codes <- cdm[[tableCodelist]] |> dplyr::collect()

  tableName <- purrr::discard(unique(codes$table), is.na)
  standardConceptIdName <- purrr::discard(unique(codes$standard_concept), is.na)
  sourceConceptIdName <- purrr::discard(unique(codes$source_concept), is.na)
  sourceConceptValueName <- purrr::discard(unique(codes$source_concept_value), is.na)
  typeConceptIdName <- purrr::discard(unique(codes$type_concept), is.na)
  startDateName <- purrr::discard(unique(codes$start_date_name), is.na)
  endDateName <- purrr::discard(unique(codes$end_date_name), is.na)

  tmpTblName1 <- omopgenerics::uniqueTableName()
  tmpTblName2 <- omopgenerics::uniqueTableName()

  on.exit({
    omopgenerics::dropSourceTable(cdm = cdm,
                                        name = tmpTblName1);
    omopgenerics::dropSourceTable(cdm = cdm,
                                        name = tmpTblName2)
    })

  if(!is.null(cohortTable)){
    if(!is.null(cohortId) &
       needsIdFilter(cdm[[cohortTable]], cohortId)){
      cohortSubjects <- cdm[[cohortTable]] |>
        dplyr::filter(.data$cohort_definition_id %in% cohortId)
    } else {
      cohortSubjects <- cdm[[cohortTable]]
    }

    if(timing == "entry"){
      # will need person id and cohort start dates
      cohortSubjects <- cohortSubjects |>
        dplyr::select("subject_id", "cohort_start_date") |>
        dplyr::rename("person_id" = "subject_id") |>
        dplyr::distinct()
    } else {
      # otherwise any and we only need person ids
      cohortSubjects <- cohortSubjects |>
        dplyr::select("subject_id") |>
        dplyr::rename("person_id" = "subject_id") |>
        dplyr::distinct()
    }
  }

  if(length(tableName)>0){
    codeRecords <- cdm[[tableName[[1]]]] |>
      dplyr::select("person_id",
                    startDateName[[1]],
                    endDateName[[1]],
                    .env$standardConceptIdName[[1]],
                    .env$sourceConceptIdName[[1]],
                    .env$sourceConceptValueName[[1]],
                    .env$typeConceptIdName[[1]]) |>
      dplyr::mutate(table = !!omopgenerics::tableName(cdm[[tableName[[1]]]]))
    if(!is.null(cohortTable)){
      # keep only records of those in the cohorts of interest
      if(timing == "entry"){
        workingDateName <- startDateName[[1]]
        codeRecords <- codeRecords |>
          dplyr::inner_join(cohortSubjects,
                            dplyr::join_by("person_id",
                               !!dplyr::sym(workingDateName) == "cohort_start_date"))
      } else {
        codeRecords <- codeRecords |>
          dplyr::inner_join(cohortSubjects,
                            by = "person_id")
      }
    }

    codeRecords <- codeRecords |>
      dplyr::compute(name = tmpTblName1)

    if(is.null(codeRecords)){
      return(NULL)
    }

    tableCodes <- paste0(omopgenerics::uniqueTableName(),
                            omopgenerics::uniqueId())
    cdm <- omopgenerics::insertTable(cdm = cdm,
                                     name = tableCodes,
                                     table = codes |>
                                       dplyr::filter(.data$table == !!tableName[[1]]) |>
                                       dplyr::select("concept_id", "domain_id"),
                                     overwrite = TRUE,
                                     temporary = FALSE)
    codeRecords <- codeRecords |>
      dplyr::mutate(start_date = !!dplyr::sym(startDateName[[1]]),
                    end_date = !!dplyr::sym(endDateName[[1]])) |>
      dplyr::mutate(year = clock::get_year(.data$start_date)) |>
      dplyr::select(dplyr::all_of(c("person_id",
                                    standardConceptIdName[[1]],
                                    sourceConceptIdName[[1]],
                                    sourceConceptValueName[[1]],
                                    typeConceptIdName[[1]],
                                    "table",
                                    "start_date","end_date", "year"))) |>
      dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[1]],
                    "source_concept_id" = .env$sourceConceptIdName[[1]],
                    "source_concept_value" = .env$sourceConceptValueName[[1]],
                    "type_concept_id" = .env$typeConceptIdName[[1]]) |>
      dplyr::compute(
        name = paste0(intermediateTable,"_grr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE,
        logPrefix = "CodelistGenerator.getRelevantRecords_join"
      )

    if(isTRUE(useSourceCodes)){
      codeRecords <- codeRecords |>
        dplyr::inner_join(cdm[[tableCodes]],
                          by = c("source_concept_id"="concept_id")) |>
        dplyr::compute(
          name = paste0(intermediateTable,"_grr"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE,
          logPrefix = "CodelistGenerator.getRelevantRecords_filter"
        )
    }else{
      codeRecords <- codeRecords |>
        dplyr::inner_join(cdm[[tableCodes]],
                          by = c("standard_concept_id"="concept_id")) |>
        dplyr::compute(
          name = paste0(intermediateTable,"_grr"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE,
          logPrefix = "CodelistGenerator.getRelevantRecords_filter"
        )
    }
    omopgenerics::dropSourceTable(cdm = cdm, name = tableCodes)
    cdm[[tableCodes]] <- NULL

  } else {
    return(NULL)
  }

  # get for any additional domains and union
  if(length(tableName) > 1) {
    for(i in 1:(length(tableName)-1)) {
      workingRecords <-  cdm[[tableName[[i+1]]]]
      if(!is.null(cohortTable)){
        # keep only records of those in the cohorts of interest
        if(timing == "entry"){
          workingDateName <- startDateName[[i+1]]
          workingRecords <- workingRecords |>
            dplyr::inner_join(cohortSubjects,
                              dplyr::join_by("person_id",
                                             !!dplyr::sym(workingDateName) == "cohort_start_date"))
        } else {
          workingRecords <- workingRecords |>
            dplyr::inner_join(cohortSubjects,
                              by = "person_id")
        }
      }

      cdm <- omopgenerics::insertTable(cdm = cdm,
                                       name = tmpTblName2,
                                       table = codes |>
                                         dplyr::filter(.data$table == tableName[[i+1]]) |>
                                         dplyr::select("concept_id", "domain_id"),
                                       overwrite = TRUE,
                                       temporary = FALSE)
      workingRecords <-  workingRecords |>
        dplyr::mutate(start_date = !!dplyr::sym(startDateName[[i+1]])) |>
        dplyr::mutate(year = clock::get_year(.data$start_date),
                      table = !!omopgenerics::tableName(cdm[[tableName[[1]]]])) |>
        dplyr::select(dplyr::all_of(c("person_id",
                                      standardConceptIdName[[i+1]],
                                      sourceConceptIdName[[i+1]],
                                      typeConceptIdName[[i+1]],
                                      "start_date", "year"))) |>
        dplyr::rename("standard_concept_id" = .env$standardConceptIdName[[i+1]],
                      "source_concept_id" = .env$sourceConceptIdName[[i+1]],
                      "type_concept_id" = .env$typeConceptIdName[[i+1]]) |>
        dplyr::compute(
          name = paste0(intermediateTable,"_grr1"),
          temporary = FALSE,
          schema = attr(cdm, "write_schema"),
          overwrite = TRUE,
          logPrefix = "CodelistGenerator.getRelevantRecords_join1"
        )

      if(isTRUE(useSourceCodes)){
        workingRecords <- workingRecords |>
          dplyr::inner_join(cdm[[tmpTblName2]],
                            by = c("source_concept_id"="concept_id")) |>
          dplyr::compute(
            name = paste0(intermediateTable,"_grr1"),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE,
            logPrefix = "CodelistGenerator.getRelevantRecords_filter1"
          )
      }else{
        workingRecords <- workingRecords |>
          dplyr::inner_join(cdm[[tmpTblName2]],
                            by = c("standard_concept_id"="concept_id")) |>
          dplyr::compute(
            name = paste0(intermediateTable,"_grr1"),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE,
            logPrefix = "CodelistGenerator.getRelevantRecords_filter1"
          )
      }

      if(workingRecords |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") >0){
        codeRecords <- codeRecords |>
          dplyr::union_all(workingRecords)  |>
          dplyr::compute(
            name = paste0(intermediateTable,"_grr_i"),
            temporary = FALSE,
            schema = attr(cdm, "write_schema"),
            overwrite = TRUE,
            logPrefix = "CodelistGenerator.getRelevantRecords_unionAll"
          )
      }
   }
  }

  if(codeRecords |> utils::head(1) |> dplyr::tally() |> dplyr::pull("n") >0){
    codeRecords <- codeRecords |>
      PatientProfiles::addConceptName(column = "standard_concept_id",
                                      nameStyle = "standard_concept_name") |>
      PatientProfiles::addConceptName(column = "source_concept_id",
                                      nameStyle = "source_concept_name")|>
      PatientProfiles::addConceptName(column = "type_concept_id",
                                      nameStyle = "type_concept_name") |>
      dplyr::mutate(standard_concept_name = dplyr::if_else(is.na(.data$standard_concept_name),
                                                         "NA", .data$standard_concept_name),
                    source_concept_name = dplyr::if_else(is.na(.data$source_concept_name),
                                                         "NA", .data$source_concept_name),
                    type_concept_name = dplyr::if_else(is.na(.data$type_concept_name),
                                                         "NA", .data$type_concept_name)) |>
      dplyr::compute(
        name = paste0(intermediateTable,"_grr_cr"),
        temporary = FALSE,
        schema = attr(cdm, "write_schema"),
        overwrite = TRUE,
        logPrefix = "CodelistGenerator.getRelevantRecords_codeRecords"
      )
  }

  return(codeRecords)

}

filterDateRange <- function(records, cdm, dateRange){
  dateRange <- getDateRange(cdm, dateRange)
  records |>
    dplyr::filter(.data$start_date >= !!dateRange[1],
                  .data$start_date <= !!dateRange[2])
}

getSummaryCounts <- function(records,
                             cdm,
                             countBy,
                             byConcept,
                             byYear,
                             bySex,
                             byAgeGroup) {
  if ("record" %in% countBy) {
    recordSummary <- records |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value))

    if(isTRUE(byConcept)) {
      recordSummary <- dplyr::bind_rows(
        recordSummary,
        records |>
          dplyr::group_by(
            .data$standard_concept_id, .data$standard_concept_name,
            .data$source_concept_id, .data$source_concept_name,
            .data$source_concept_value,
            .data$type_concept_id,  .data$type_concept_name,
            .data$domain_id, .data$table
          ) |>
          dplyr::tally(name = "estimate_value") |>
          dplyr::collect() |>
          dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
          dplyr::mutate(estimate_value = as.character(.data$estimate_value))
      )
    }else{
      recordSummary <- recordSummary |>
        dplyr::mutate("standard_concept_id" = NA_integer_,
                      "standard_concept_name" = NA_character_,
                      "source_concept_id" = NA_integer_,
                      "source_concept_name" = NA_character_,
                      "source_concept_value" = NA_character_,
                      "type_concept_id" = NA_integer_,
                      "type_concept_name" =  NA_character_,
                      "domain_id" = NA_character_,
                      "table" = NA_character_)
    }
    recordSummary <- recordSummary |>
      dplyr::mutate(
        strata_name = "overall",
        strata_level = "overall",
        estimate_name = "record_count"
      )
  } else {
    recordSummary <- dplyr::tibble()
  }

  if ("person" %in% countBy) {
    personSummary <- records |>
      dplyr::select("person_id") |>
      dplyr::distinct() |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value))

    if (isTRUE(byConcept)) {
      personSummary <- dplyr::bind_rows(
        personSummary,
        records |>
          dplyr::select(
            "person_id", "standard_concept_id", "standard_concept_name",
            "source_concept_id", "source_concept_name",
            "source_concept_value", "type_concept_id", "type_concept_name",
            "domain_id", "table"
          ) |>
          dplyr::distinct() |>
          dplyr::group_by(
            .data$standard_concept_id, .data$standard_concept_name,
            .data$source_concept_id, .data$source_concept_name,
            .data$type_concept_name,
            .data$source_concept_value, .data$type_concept_id,
            .data$domain_id, .data$table
          ) |>
          dplyr::tally(name = "estimate_value") |>
          dplyr::collect() |>
          dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
          dplyr::mutate(estimate_value = as.character(.data$estimate_value))
      )
    }
    personSummary <- personSummary |>
      dplyr::mutate(
        strata_name = "overall",
        strata_level = "overall",
        estimate_name = "person_count")
  } else {
    personSummary <- dplyr::tibble()
  }

  if ("record" %in% countBy & byYear == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "year")
    )
  }
  if ("person" %in% countBy & byYear == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "year")
    )
  }
  if ("record" %in% countBy & bySex == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "sex")
    )
  }
  if ("person" %in% countBy & bySex == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "sex")
    )
  }
  if ("record" %in% countBy & byAgeGroup == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = "age_group")
    )
  }
  if ("person" %in% countBy & byAgeGroup == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = "age_group")
    )
  }
  if ("record" %in% countBy && byAgeGroup == TRUE && bySex == TRUE) {
    recordSummary <- dplyr::bind_rows(
      recordSummary,
      getGroupedRecordCount(records = records, cdm = cdm, groupBy = c("age_group", "sex"))
    )
  }
  if ("person" %in% countBy && byAgeGroup == TRUE && bySex == TRUE) {
    personSummary <- dplyr::bind_rows(
      personSummary,
      getGroupedPersonCount(records = records, cdm = cdm, groupBy = c("age_group", "sex"))
    )
  }
  summary <- dplyr::bind_rows(recordSummary, personSummary)
  return(summary)
}

getGroupedRecordCount <- function(records,
                                  cdm,
                                  groupBy){

  groupedCounts <- dplyr::bind_rows(
    records |>
      dplyr::group_by(dplyr::pick(.env$groupBy)) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)),
    records |>
      dplyr::group_by(dplyr::pick(.env$groupBy,
                                  "standard_concept_id", "standard_concept_name",
                                  "source_concept_id", "source_concept_name",
                                  "source_concept_value", "type_concept_id",
                                  "type_concept_name",
                                  "domain_id", "table")) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value))
    )  |>
    omopgenerics::uniteStrata(cols = groupBy) |>
    dplyr::mutate(estimate_name = "record_count")

  return(groupedCounts)

}

getGroupedPersonCount <- function(records,
                                  cdm,
                                  groupBy){

  groupedCounts <- dplyr::bind_rows(
    records |>
      dplyr::select(dplyr::all_of(c("person_id", .env$groupBy))) |>
      dplyr::distinct() |>
      dplyr::group_by(dplyr::pick(.env$groupBy)) |>
      dplyr::tally(name = "estimate_value") |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value)),
    records |>
      dplyr::select(dplyr::all_of(c(
        "person_id", "standard_concept_id", "standard_concept_name",
        "source_concept_id", "source_concept_name",
        "source_concept_value", "type_concept_id", "type_concept_name",
        "domain_id", "table", .env$groupBy
      ))) |>
      dplyr::distinct() |>
      dplyr::group_by(dplyr::pick(
        .env$groupBy, "standard_concept_id", "standard_concept_name",
        "source_concept_id", "source_concept_name",
        "source_concept_value", "type_concept_id", "type_concept_name",
        "domain_id", "table"
      )) |>
      dplyr::tally(name = "estimate_value")  |>
      dplyr::collect() |>
      dplyr::arrange(dplyr::desc(.data$estimate_value)) |>
      dplyr::mutate(estimate_value = as.character(.data$estimate_value))) |>
    omopgenerics::uniteStrata(cols = groupBy) |>
    dplyr::mutate(estimate_name = "person_count")

  return(groupedCounts)

}

checkCategory <- function(category, overlap = FALSE) {
  omopgenerics::assertList(category, unique = TRUE)

  if (is.null(names(category))) {
    names(category) <- rep("", length(category))
  }

  # check length
  category <- lapply(category, function(x) {
    if (length(x) == 1) {
      x <- c(x, x)
    } else if (length(x) > 2) {
      cli::cli_abort(
        paste0(
          "Categories should be formed by a lower bound and an upper bound, ",
          "no more than two elements should be provided."
        ),
        call. = FALSE
      )
    }
    return(x)
  })

  # check lower bound is smaller than upper bound
  checkLower <- unlist(lapply(category, function(x) {
    x[1] <= x[2]
  }))
  if (!(all(checkLower))) {
    cli::cli_abort("Lower bound should be equal or smaller than upper bound")
  }

  # built tibble
  result <- lapply(category, function(x) {
    dplyr::tibble(lower_bound = x[1], upper_bound = x[2])
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(category_label = names(.env$category)) |>
    dplyr::mutate(category_label = dplyr::if_else(
      .data$category_label == "",
      paste0(.data$lower_bound, " to ", .data$upper_bound),
      .data$category_label
    )) |>
    dplyr::arrange(.data$lower_bound)

  # check overlap
  if(!overlap) {
    if (nrow(result) > 1) {
      lower <- result$lower_bound[2:nrow(result)]
      upper <- result$upper_bound[1:(nrow(result) - 1)]
      if (!all(lower > upper)) {
        cli::cli_abort("There can not be overlap between categories")
      }
    }
  }

  return(result)
}

checkAgeGroup <- function(ageGroup, overlap = FALSE) {
  omopgenerics::assertList(ageGroup, null = TRUE)
  if (!is.null(ageGroup)) {
    if (is.numeric(ageGroup[[1]])) {
      ageGroup <- list("age_group" = ageGroup)
    }
    for (k in seq_along(ageGroup)) {
      invisible(checkCategory(ageGroup[[k]], overlap))
    }
    if (is.null(names(ageGroup))) {
      names(ageGroup) <- paste0("age_group_", seq_along(ageGroup))
    }
    if ("" %in% names(ageGroup)) {
      id <- which(names(ageGroup) == "")
      names(ageGroup)[id] <- paste0("age_group_", id)
    }
  }
  return(ageGroup)
}

getDateRange <- function(cdm, dateRange) {

  if(is.na(dateRange[1])){
    obs_date_range <- cdm[["observation_period"]] |>
      dplyr::summarise(
        min_start = as.Date(min(.data$observation_period_start_date, na.rm = TRUE))
      ) |>
      dplyr::collect()
    dateRange[1] <- obs_date_range$min_start
  }

  if(is.na(dateRange[2])){
    obs_date_range <- cdm[["observation_period"]] |>
      dplyr::summarise(
        max_end = as.Date(max(.data$observation_period_end_date, na.rm = TRUE))
      ) |>
      dplyr::collect()
    dateRange[2] <- obs_date_range$max_end
  }

  return(dateRange)
}
