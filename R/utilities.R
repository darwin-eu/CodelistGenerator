needsIdFilter <- function(cohort, cohortId){
  !identical(omopgenerics::settings(cohort)$cohort_definition_id, cohortId)
}
