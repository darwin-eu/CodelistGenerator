
conceptDomainsData <- dplyr::tibble(domain_id = c("drug","condition",
                                                  "procedure",  "observation",
                                                  "measurement", "visit",
                                                  "device")) %>%
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
  ) %>%
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
  ) %>%
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
  ) %>%
  dplyr::mutate(date_name =
                  dplyr::case_when(
                    stringr::str_detect(domain_id,"condition") ~ "condition_start_date",
                    stringr::str_detect(domain_id,"drug") ~ "drug_exposure_start_date",
                    stringr::str_detect(domain_id,"observation") ~ "observation_date",
                    stringr::str_detect(domain_id,"measurement") ~ "measurement_date",
                    stringr::str_detect(domain_id,"visit") ~ "visit_start_date",
                    stringr::str_detect(domain_id,"procedure") ~ "procedure_date",
                    stringr::str_detect(domain_id,"device") ~ "device_exposure_start_date"
                  )
  )

usethis::use_data(conceptDomainsData, overwrite = TRUE)

usethis::use_data(conceptDomainsData, overwrite = TRUE)
