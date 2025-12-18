# Testing against different database platforms

test_that("redshift", {
  skip_on_cran()

  testthat::skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")
  skip_if_offline()

  db <-  DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))

  cdm <- CDMConnector::cdmFromCon(con = db,
                                  cdmSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
                                  writeSchema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
                                  cdmVersion = "5.3")


  expect_no_error(routeCat <- availableRouteCategories(cdm))
  expect_true(all(routeCat %in%
                  c(doseFormToRoute$route_category, "unclassified_route")))
  # alphabetical order
  expect_identical(routeCat,
                   sort(availableRouteCategories(cdm)))

  cdm$concept <- cdm$concept |>
    dplyr::mutate(concept_id = as.integer64(concept_id)) |>
    dplyr::compute()

  # candidate code search
  expect_no_error(asthma<-getCandidateCodes(cdm,
                                            keywords = c("asthma",
                                                         "irritable airways",
                                                         "lung disease",
                                                         "respiratory abnormalities",
                                                         "sleep apnea",
                                                         "chronic obstructive lung disease",
                                                         "chronic obstructive lung disease"),

                       domains = c("condition", "observation"),
                       exclude = c("childhood", "juvenile"),
                       searchInSynonyms = TRUE,
                       searchNonStandard = TRUE,
                       includeDescendants = TRUE,
                       includeAncestor = TRUE))
  expect_true(nrow(asthma) > 0)

  # drug ingredients
  expect_no_error(metformin <- getDrugIngredientCodes(cdm, "metformin",
                                                      nameStyle = "{concept_name}"))
  expect_no_error(metformin <- getDrugIngredientCodes(cdm, 1503297,
                                                      nameStyle = "{concept_name}"))

  expect_true(inherits(metformin, "codelist"))
  expect_true("metformin" %in% names(metformin))

  expect_no_error(metformin_2 <- getDrugIngredientCodes(cdm, "metformin",
                                                      nameStyle = "{concept_code}"))
  expect_true("6809" %in% names(metformin_2))

  expect_no_error(metformin_3 <- getDrugIngredientCodes(cdm, "metformin",
                                                        nameStyle = "{concept_code}_{concept_name}"))
  expect_true("6809_metformin" %in% names(metformin_3))

  expect_error(getDrugIngredientCodes(cdm, "metformin",
                                         nameStyle = "something else"))


  # achilles
  cdm$achilles_results <- cdm$condition_occurrence |>
    dplyr::group_by(condition_concept_id) |>
    dplyr::tally(name = "count_value") |>
    dplyr::rename("stratum_1" = "condition_concept_id") |>
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 401) |>
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles |>
                 dplyr::filter(stringr::str_detect(variable_level, "317009"),
                               estimate_name == "record_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == 317009,
                               estimate_name == "record_count") |>
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles |>
                 dplyr::filter(stringr::str_detect(variable_level, "257581"),
                               estimate_name == "record_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == 257581,
                               estimate_name == "record_count") |>
                 dplyr::pull("estimate_value"))


  cdm$achilles_results <- cdm$condition_occurrence |>
    dplyr::group_by(person_id, condition_concept_id) |>
    dplyr::tally() |>
    dplyr::ungroup() |>
    dplyr::group_by(condition_concept_id) |>
    dplyr::tally(name = "count_value") |>
    dplyr::rename("stratum_1" = "condition_concept_id") |>
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 400) |>
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles |>
                 dplyr::filter(stringr::str_detect(variable_level, "317009"),
                               estimate_name == "person_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == 317009,
                               estimate_name == "person_count") |>
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles |>
                 dplyr::filter(stringr::str_detect(variable_level, "257581"),
                               estimate_name == "person_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == 257581,
                               estimate_name == "person_count") |>
                 dplyr::pull("estimate_value"))


  # edge cases
  # concept id not in achilles
  expect_message(summariseAchillesCodeUse(list(asthma = 123),
                                 cdm = cdm))

  # expected errors
  expect_error(summariseAchillesCodeUse(123, #not a named list
                               cdm = cdm))
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = "cdm")) # not a cdm
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = cdm,
                               countBy = "not an option"))

  # working concept set example with mock
  x <- codesFromConceptSet(
    cdm = cdm, path =  system.file(package = "CodelistGenerator",
                                   "concepts_dbms")
  )
  expect_true(x$oa_no_desc == 4079750)
  expect_true(!761485 %in% x$oa_no_desc)
  expect_true(761485 %in% x$oa_desc)


  CDMConnector::cdmDisconnect(cdm)
})

test_that("snowflake", {
  skip_on_cran()
  testthat::skip_if(Sys.getenv("SNOWFLAKE_SERVER") == "")
  skip_if_offline()

  con <- DBI::dbConnect(odbc::odbc(),
                        SERVER = Sys.getenv("SNOWFLAKE_SERVER"),
                        UID = Sys.getenv("SNOWFLAKE_USER"),
                        PWD = Sys.getenv("SNOWFLAKE_PASSWORD"),
                        DATABASE = Sys.getenv("SNOWFLAKE_DATABASE"),
                        WAREHOUSE = Sys.getenv("SNOWFLAKE_WAREHOUSE"),
                        DRIVER = Sys.getenv("SNOWFLAKE_DRIVER"))

  cdm_schema <- strsplit(Sys.getenv("SNOWFLAKE_CDM_SCHEMA"), "\\.")[[1]]
  write_schema <- strsplit(Sys.getenv("SNOWFLAKE_SCRATCH_SCHEMA"), "\\.")[[1]]

  cdm <- CDMConnector::cdmFromCon(con = con,
                                    cdmSchema = cdm_schema,
                                    writeSchema = write_schema,
                                    cdmName = "snowflake")


  # candidate code search
  expect_no_error(asthma<-getCandidateCodes(cdm,
                                            keywords = c("asthma",
                                                         "irritable airways",
                                                         "lung disease",
                                                         "respiratory abnormalities",
                                                         "sleep apnea",
                                                         "chronic obstructive lung disease",
                                                         "chronic obstructive lung disease"),

                                            domains = c("condition", "observation"),
                                            exclude = c("childhood", "juvenile"),
                                            searchInSynonyms = TRUE,
                                            searchNonStandard = TRUE,
                                            includeDescendants = TRUE,
                                            includeAncestor = TRUE))
  expect_true(nrow(asthma) > 0)

  # drug ingredients
  expect_no_error(getDrugIngredientCodes(cdm, "metformin"))

  # achilles
  cdm$achilles_results <- cdm$condition_occurrence |>
    dplyr::group_by(condition_concept_id) |>
    dplyr::tally(name = "count_value") |>
    dplyr::rename("stratum_1" = "condition_concept_id") |>
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 401) |>
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles |>
                            dplyr::filter(variable_level == "317009",
                                          variable_name == "record_count") |>
                            dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count ") |>
                 dplyr::pull("estimate_value"))

  expect_equal(result_achilles |>
                 dplyr::filter(variable_level == "257581",
                               variable_name == "record_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == "257581",
                               variable_name == "record_count ") |>
                 dplyr::pull("estimate_value"))

  cdm$achilles_results <- cdm$condition_occurrence |>
    dplyr::group_by(person_id, condition_concept_id) |>
    dplyr::tally() |>
    dplyr::ungroup() |>
    dplyr::group_by(condition_concept_id) |>
    dplyr::tally(name = "count_value") |>
    dplyr::rename("stratum_1" = "condition_concept_id") |>
    dplyr::mutate(stratum_2 = NA,
                  stratum_3 = NA,
                  analysis_id = 400) |>
    dplyr::compute()

  asthma <- list(asthma = c(317009, 257581))
  result_achilles <- summariseAchillesCodeUse(asthma,
                                     cdm = cdm)
  result_cdm <- summariseCodeUse(asthma, cdm = cdm)

  expect_equal(result_achilles |>
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(variable_level == "317009",
                               variable_name == "record_count ") |>
                 dplyr::pull("estimate_value"))



  expect_equal(result_achilles |>
                            dplyr::filter(group_level == "317009",
                                          variable_name == "person_count") |>
                            dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(group_level ==  "317009",
                               variable_name == "person_count") |>
                 dplyr::pull("estimate_value"))
  expect_equal(result_achilles |>
                 dplyr::filter(group_level == "257581",
                               variable_name == "person_count") |>
                 dplyr::pull("estimate_value"),
               result_cdm |>
                 dplyr::filter(group_level ==  "257581",
                               variable_name == "person_count") |>
                 dplyr::pull("estimate_value"))

  # edge cases
  # concept id not in achilles
  expect_message(summariseAchillesCodeUse(list(asthma = 123),
                                 cdm = cdm))

  # expected errors
  expect_error(summariseAchillesCodeUse(123, #not a named list
                               cdm = cdm))
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = "cdm")) # not a cdm
  expect_error(summariseAchillesCodeUse(asthma,
                               cdm = cdm,
                               countBy = "not an option"))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("postgres", {
  skip_on_cran()
  testthat::skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")
  skip_if_offline()

  db <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                       host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                       user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                       password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdm <- CDMConnector::cdmFromCon(
    con = db,
    cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
    writeSchema = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
    writePrefix = "incp_",
    achillesSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA")
  )

  # check orphan code use
  expect_no_error(summariseOrphanCodes(list("asthma"=317009L), cdm))

  asthma <- getCandidateCodes(cdm, "asthma", domains = c("condition",
                                                         "observation"))

  expect_identical(subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                 cdm = cdm,
                 domain = "condition"),
  omopgenerics::newCodelist(list("asthma" = sort(as.integer(asthma |>
    dplyr::filter(domain_id == "Condition") |>
    dplyr::pull("concept_id"))))))
  expect_identical(subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                                  cdm = cdm,
                                  domain = c("condition", "observation")),
                   omopgenerics::newCodelist(list("asthma" = sort(as.integer(asthma |>
                                                                               dplyr::filter(domain_id %in% c("Condition", "Observation")) |>
                                                                               dplyr::pull("concept_id"))))))

  expect_identical(
    subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                 cdm = cdm,
                 domain = c("procedure")),
  omopgenerics::emptyCodelist())

  asthma_orphan <- summariseOrphanCodes(list(asthma = asthma$concept_id),
                                        cdm, domain = c("condition"))

  # limit orphan codes to a domain
  # we won't have any orphan codes from drug
  asthma_oc <- summariseOrphanCodes(list("asthma"= c(317009)),
                       cdm,
                       domain = "drug")
  expect_true(nrow(asthma_oc) == 0)


  codes <- getDrugIngredientCodes(cdm, "metformin")
  codes[["asthma"]] <- 317009

  expect_no_error(stratifyByDoseUnit(codes, cdm))
  expect_no_error(stratifyByDoseUnit(codes, cdm, keepOriginal = TRUE))
  expect_no_error(stratifyByRouteCategory(codes, cdm))
  expect_no_error(stratifyByRouteCategory(codes, cdm, keepOriginal = TRUE))


  drug_codes <- getDrugIngredientCodes(cdm, name = c("metformin",
                                                     "diclofenac"))

  # we can stratify by each concept contained
  drug_codes_by_concept <- stratifyByConcept(drug_codes,
                                             cdm = cdm)

  drug_codes_by_concept_used <- subsetToCodesInUse(drug_codes_by_concept, cdm)

  # if we subset to oral both should still have codes
  expect_true(length(subsetOnRouteCategory(drug_codes, cdm,
                          routeCategory = "oral")) == 2)
  # only metformin has injectable route
  expect_true(length(subsetOnRouteCategory(drug_codes, cdm,
                                             routeCategory = "injectable")) == 1)
  # we can put multiple route categories
  # and we should get the same result if we subset up front or later
  drug_codes2 <- getDrugIngredientCodes(cdm,
                                        routeCategory = c("injectable",
                                                          "oral"),
                                        name = c("metformin","diclofenac"))
  drug_codes_subset <-  getDrugIngredientCodes(cdm,
                                               routeCategory = c("injectable",
                                                                 "oral"),
                                               name = c("metformin","diclofenac"))


  expect_true(length(drug_codes_subset) == 2)
  expect_identical(drug_codes_subset, drug_codes2)

  # can stratify by route
  expect_no_error(drug_codes_stratified_by_route <-  getDrugIngredientCodes(cdm,
                                                                            type = "codelist_with_details",
                                               name = c("metformin","diclofenac")) |>
    stratifyByRouteCategory(cdm = cdm))
  expect_true(inherits(drug_codes_stratified_by_route, "codelist_with_details"))



 # can subset and stratify by dose unit
  expect_no_error(availableDoseUnits(cdm))
  drugs <- getDrugIngredientCodes(cdm,
                                      name = c("metformin","diclofenac"))
  expect_no_error(subsetOnDoseUnit(drugs, cdm, c("milligram")))

  expect_no_error(drug_codes_stratified_by_unit <-  getDrugIngredientCodes(cdm,
                                                                            name = c("metformin","diclofenac")) |>
                    stratifyByDoseUnit(cdm = cdm))

  # we can also stratify by both route and unit
  expect_no_error(drug_codes_stratified_by_route_and_unit <-  getDrugIngredientCodes(cdm,
                                                                           name = c("metformin","diclofenac")) |>
                    stratifyByRouteCategory(cdm = cdm) |>
                    stratifyByDoseUnit(cdm = cdm))

 drugs_milligram_transdermal <-  getDrugIngredientCodes(cdm,
                         name = c("metformin","diclofenac"),
                         doseUnit = "milligram",
                         routeCategory = "transdermal")

 drugs_milligram_transdermal_2 <- getDrugIngredientCodes(cdm,
                                                  name = c("metformin","diclofenac")) |>
   subsetOnRouteCategory(cdm, "transdermal") |>
   subsetOnDoseUnit(cdm, "milligram")
 expect_identical(drugs_milligram_transdermal,
                  drugs_milligram_transdermal_2)


 atc <- getATCCodes(cdm, name = "alimentary tract and metabolism")
 expect_no_error(atc |>
   stratifyByRouteCategory(cdm = cdm) |>
   stratifyByDoseUnit(cdm = cdm))

  # make sure no extra domains added to the results
  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = c("at") ,
    domains = c("Condition", "Observation"),
    standardConcept = "Standard",
    searchInSynonyms = FALSE,
    searchNonStandard = FALSE,
    includeDescendants = TRUE,
    includeAncestor = FALSE
  )
  expect_true(length(unique(codes$domain_id)) <= 2)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("sql server", {
  skip_on_cran()
  testthat::skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")
  testthat::skip_if(Sys.getenv("SQL_SERVER_DRIVER") == "")
  skip_if_offline()

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))
  cdm <- CDMConnector::cdmFromCon(db,
                                    cdmSchema = c("CDMV54", "dbo"),
                                    achillesSchema = c("CDMV54", "dbo"),
                                    writeSchema = c("ohdsi", "dbo"))

  # check orphan code use
  expect_no_error(summariseOrphanCodes(list("asthma"=317009L), cdm))

  asthma <- getCandidateCodes(cdm, "asthma", domains = c("condition",
                                                         "observation"))

  expect_identical(subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                                  cdm = cdm,
                                  domain = "condition"),
                   omopgenerics::newCodelist(list("asthma" = sort(as.integer(asthma |>
                                                                               dplyr::filter(domain_id == "Condition") |>
                                                                               dplyr::pull("concept_id"))))))
  expect_identical(subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                                  cdm = cdm,
                                  domain = c("condition", "observation")),
                   omopgenerics::newCodelist(list("asthma" = sort(as.integer(asthma |>
                                                                               dplyr::filter(domain_id %in% c("Condition", "Observation")) |>
                                                                               dplyr::pull("concept_id"))))))

  expect_identical(
    subsetOnDomain(list("asthma" = as.integer(asthma$concept_id)),
                   cdm = cdm,
                   domain = c("procedure")),
    omopgenerics::emptyCodelist())

  asthma_orphan <- summariseOrphanCodes(list(asthma = asthma$concept_id),
                                        cdm, domain = c("condition"))

  # limit orphan codes to a domain
  # we won't have any orphan codes from drug
  asthma_oc <- summariseOrphanCodes(list("asthma"= c(317009)),
                                    cdm,
                                    domain = "drug")
  expect_true(nrow(asthma_oc) == 0)


  codes <- getDrugIngredientCodes(cdm, "metformin")
  codes[["asthma"]] <- 317009

  expect_no_error(stratifyByDoseUnit(codes, cdm))
  expect_no_error(stratifyByDoseUnit(codes, cdm, keepOriginal = TRUE))
  expect_no_error(stratifyByRouteCategory(codes, cdm))
  expect_no_error(stratifyByRouteCategory(codes, cdm, keepOriginal = TRUE))


  drug_codes <- getDrugIngredientCodes(cdm, name = c("metformin",
                                                     "diclofenac"))

  # we can stratify by each concept contained
  drug_codes_by_concept <- stratifyByConcept(drug_codes,
                                             cdm = cdm)

  drug_codes_by_concept_used <- subsetToCodesInUse(drug_codes_by_concept, cdm)

  # if we subset to oral both should still have codes
  expect_true(length(subsetOnRouteCategory(drug_codes, cdm,
                                           routeCategory = "oral")) == 2)
  # only metformin has injectable route
  expect_true(length(subsetOnRouteCategory(drug_codes, cdm,
                                           routeCategory = "injectable")) == 1)
  # we can put multiple route categories
  # and we should get the same result if we subset up front or later
  drug_codes2 <- getDrugIngredientCodes(cdm,
                                        routeCategory = c("injectable",
                                                          "oral"),
                                        name = c("metformin","diclofenac"))
  drug_codes_subset <-  getDrugIngredientCodes(cdm,
                                               routeCategory = c("injectable",
                                                                 "oral"),
                                               name = c("metformin","diclofenac"))


  expect_true(length(drug_codes_subset) == 2)
  expect_identical(drug_codes_subset, drug_codes2)

  # can stratify by route
  expect_no_error(drug_codes_stratified_by_route <-  getDrugIngredientCodes(cdm,
                                                                            type = "codelist_with_details",
                                                                            name = c("metformin","diclofenac")) |>
                    stratifyByRouteCategory(cdm = cdm))
  expect_true(inherits(drug_codes_stratified_by_route, "codelist_with_details"))



  # can subset and stratify by dose unit
  expect_no_error(availableDoseUnits(cdm))
  drugs <- getDrugIngredientCodes(cdm,
                                  name = c("metformin","diclofenac"))
  expect_no_error(subsetOnDoseUnit(drugs, cdm, c("milligram")))

  expect_no_error(drug_codes_stratified_by_unit <-  getDrugIngredientCodes(cdm,
                                                                           name = c("metformin","diclofenac")) |>
                    stratifyByDoseUnit(cdm = cdm))

  # we can also stratify by both route and unit
  expect_no_error(drug_codes_stratified_by_route_and_unit <-  getDrugIngredientCodes(cdm,
                                                                                     name = c("metformin","diclofenac")) |>
                    stratifyByRouteCategory(cdm = cdm) |>
                    stratifyByDoseUnit(cdm = cdm))

  drugs_milligram_transdermal <-  getDrugIngredientCodes(cdm,
                                                         name = c("metformin","diclofenac"),
                                                         doseUnit = "milligram",
                                                         routeCategory = "transdermal")

  drugs_milligram_transdermal_2 <- getDrugIngredientCodes(cdm,
                                                          name = c("metformin","diclofenac")) |>
    subsetOnRouteCategory(cdm, "transdermal") |>
    subsetOnDoseUnit(cdm, "milligram")
  expect_identical(drugs_milligram_transdermal,
                   drugs_milligram_transdermal_2)


  atc <- getATCCodes(cdm, name = "alimentary tract and metabolism")
  expect_no_error(atc |>
                    stratifyByRouteCategory(cdm = cdm) |>
                    stratifyByDoseUnit(cdm = cdm))

  # make sure no extra domains added to the results
  codes <- getCandidateCodes(
    cdm = cdm,
    keywords = c("at") ,
    domains = c("Condition", "Observation"),
    standardConcept = "Standard",
    searchInSynonyms = FALSE,
    searchNonStandard = FALSE,
    includeDescendants = TRUE,
    includeAncestor = FALSE
  )
  expect_true(length(unique(codes$domain_id)) <= 2)

  CDMConnector::cdmDisconnect(cdm)
})
