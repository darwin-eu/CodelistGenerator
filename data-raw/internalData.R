conceptDomainsData <- dplyr::tibble(domain_id = c("drug","condition",
                                                  "procedure",  "observation",
                                                  "measurement", "visit",
                                                  "device")) |>
  dplyr::mutate(table =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_occurrence",
                    stringr::str_detect(domain_id,"drug") ~ "drug_exposure",
                    stringr::str_detect(domain_id,"observation") ~ "observation",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement",
                    stringr::str_detect(domain_id,"visit") ~ "visit_occurrence",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_occurrence",
                    stringr::str_detect(domain_id,"device") ~ "device_exposure"
                  )
  ) |>
  dplyr::mutate(standard_concept =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_concept_id",
                    stringr::str_detect(domain_id,"drug") ~ "drug_concept_id",
                    stringr::str_detect(domain_id,"observation") ~ "observation_concept_id",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_concept_id",
                    stringr::str_detect(domain_id,"visit") ~ "visit_concept_id",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_concept_id",
                    stringr::str_detect(domain_id,"device") ~ "device_concept_id"
                  )
  ) |>
  dplyr::mutate(source_concept =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_source_concept_id",
                    stringr::str_detect(domain_id,"drug") ~ "drug_source_concept_id",
                    stringr::str_detect(domain_id,"observation") ~ "observation_source_concept_id",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_source_concept_id",
                    stringr::str_detect(domain_id,"visit") ~ "visit_source_concept_id",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_source_concept_id",
                    stringr::str_detect(domain_id,"device") ~ "device_source_concept_id"
                  )
  ) |>
  dplyr::mutate(source_concept_value =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_source_value",
                    stringr::str_detect(domain_id,"drug") ~ "drug_source_value",
                    stringr::str_detect(domain_id,"observation") ~ "observation_source_value",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_source_value",
                    stringr::str_detect(domain_id,"visit") ~ "visit_source_value",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_source_value",
                    stringr::str_detect(domain_id,"device") ~ "device_source_value"
                  )
  ) |>
  dplyr::mutate(type_concept =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_type_concept_id",
                    stringr::str_detect(domain_id,"drug") ~ "drug_type_concept_id",
                    stringr::str_detect(domain_id,"observation") ~ "observation_type_concept_id",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_type_concept_id",
                    stringr::str_detect(domain_id,"visit") ~ "visit_type_concept_id",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_type_concept_id",
                    stringr::str_detect(domain_id,"device") ~ "device_type_concept_id"
                  )
  ) |>
  dplyr::mutate(start_date_name =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_start_date",
                    stringr::str_detect(domain_id,"drug") ~ "drug_exposure_start_date",
                    stringr::str_detect(domain_id,"observation") ~ "observation_date",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_date",
                    stringr::str_detect(domain_id,"visit") ~ "visit_start_date",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_date",
                    stringr::str_detect(domain_id,"device") ~ "device_exposure_start_date"
                  )
  ) |>
  dplyr::mutate(end_date_name =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_end_date",
                    stringr::str_detect(domain_id,"drug") ~ "drug_exposure_end_date",
                    stringr::str_detect(domain_id,"observation") ~ "observation_date",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_date",
                    stringr::str_detect(domain_id,"visit") ~ "visit_end_date",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_date",
                    stringr::str_detect(domain_id,"device") ~ "device_exposure_end_date"
                  )
  )

achillesAnalisisDetails <- dplyr::tibble(
  analysis_id = as.integer(c(
    200, 201, 225, 400, 401, 425, 600, 601, 625, 700, 701, 725, 800, 801, 825,
    1800, 1801, 1825, 2100, 2101, 2125
  )),
  distribution = 0L,
  distributed_field = NA_character_,
  analysis_name = c(
    "Number of persons with at least one visit occurrence, by visit_concept_id",
    "Number of visit occurrence records, by visit_concept_id",
    "Number of visit_occurrence records by visit_source_concept_id",
    "Number of persons with at least one condition occurrence, by condition_concept_id",
    "Number of condition occurrence records, by condition_concept_id",
    "Number of condition_occurrence records by condition_source_concept_id",
    "Number of persons with at least one procedure occurrence, by procedure_concept_id",
    "Number of procedure occurrence records, by procedure_concept_id",
    "Number of procedure_occurrence records by procedure_source_concept_id",
    "Number of persons with at least one drug exposure, by drug_concept_id",
    "Number of drug exposure records, by drug_concept_id",
    "Number of drug_exposure records by drug_source_concept_id",
    "Number of persons with at least one observation occurrence, by observation_concept_id",
    "Number of observation occurrence records, by observation_concept_id",
    "Number of observation records by observation_source_concept_id",
    "Number of persons with at least one measurement occurrence, by measurement_concept_id",
    "Number of measurement occurrence records, by measurement_concept_id",
    "Number of measurement records by measurement_source_concept_id",
    "Number of persons with at least one device exposure, by device_concept_id",
    "Number of device exposure records, by device_concept_id",
    "Number of device_exposure records by device_source_concept_id"
  ),
  stratum_1_name = c(
    "visit_concept_id", "visit_concept_id", "visit_source_concept_id",
    "condition_concept_id", "condition_concept_id",
    "condition_source_concept_id", "procedure_concept_id",
    "procedure_concept_id", "procedure_source_concept_id", "drug_concept_id",
    "drug_concept_id", "drug_source_concept_id", "observation_concept_id",
    "observation_concept_id", "observation_source_concept_id",
    "measurement_concept_id", "measurement_concept_id",
    "measurement_source_concept_id", "device_concept_id", "device_concept_id",
    "device_source_concept_id"
  ),
  stratum_2_name = NA_character_,
  stratum_3_name = NA_character_,
  stratum_4_name = NA_character_,
  stratum_5_name = NA_character_,
  is_default = TRUE,
  category = c(
    "Visit Occurrence", "Visit Occurrence", "Visit Occurrence",
    "Condition Occurrence", "Condition Occurrence", "Condition Occurrence",
    "Procedure Occurrence", "Procedure Occurrence", "Procedure Occurrence",
    "Drug Exposure", "Drug Exposure", "Drug Exposure", "Observation",
    "Observation", "Observation", "Measurement", "Measurement", "Measurement",
    "Completeness", "Device Exposure", "Device Exposure"
  ),
  table = c(
    rep("visit_occurrence", 3), rep("condition_occurrence", 3),
    rep("procedure_occurrence", 3), rep("drug_exposure", 3),
    rep("observation", 3), rep("measurement", 3), rep("device_exposure", 3)
  ),
  type = rep(c("person_count", "record_count", "record_count"), 7)
)

codeSerachColumns <- dplyr::tribble(
  ~table, ~column, ~type, ~convert,
  "search_strategy", "strategy_id", "integer", "as.integer",
  "search_strategy", "strategy_name", "character", "as.character",
  "search_strategy", "strategy_value", "character", "as.character",
  "codes", "concept_id", "integer", "as.integer",
  "codes", "found_from", "character", "as.character",
  "codes", "concept_name", "character", "as.character",
  "codes", "vocabulary_version", "character", "as.character",
  "codes", "domain_id", "character", "as.character",
  "codes", "vocabulary_id", "character", "as.character",
  "codes", "concept_class_id", "character", "as.character",
  "codes", "standard_concept", "character", "as.character",
  "codes", "concept_code", "character", "as.character",
  "codes", "valid_start_date", "date", "as.Date",
  "codes", "valid_end_date", "date", "as.Date",
  "codes", "invalid_reason", "character", "as.character",
)

usethis::use_data(
  conceptDomainsData, achillesAnalisisDetails, codeSerachColumns,
  overwrite = TRUE, internal = TRUE
)
