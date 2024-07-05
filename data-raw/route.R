
doseFormToRoute <- readr::read_csv(
  here::here("data-raw", "dose_form_to_route.csv"),
  comment = "",
  col_types = list(
    route_category = "character",
    dose_form_concept_id = "numeric"
  )
) |>
  dplyr::select("dose_form_concept_id", "route_category")

usethis::use_data(doseFormToRoute, internal = FALSE, overwrite = TRUE)

