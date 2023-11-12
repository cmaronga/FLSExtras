#' Tabulating staff minutes for all fracture sites
#'
#' @param fract_site A character value for fracture site. `hip`, `spine` or `other`
#'
#' @return A data.frame
#' @export
#'
#' @examples
staff_mins_time <- function(fract_site = "hip") {
  googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1CA4y892IxzgU2S9Z5qbqBt-aG8dULt8B6uQIjQGAN-U/edit?usp=sharing",
    sheet = cost_sheet_name,
    range = "A41:D200",
    col_types = "c"
  ) |> dplyr::select(-1) |>
    dplyr::rename(activity = 1,
           label = 2,
           value = 3) |>
    dplyr::filter(!is.na(activity),
           stringr::str_detect(
             string = activity,
             pattern = paste0("\\b.mins.", fract_site, "\\b")
           )) |>
    dplyr::mutate(
      current_practice = stringr::str_detect(activity, pattern = "no.fls."),
      # what kind of staff?
      staff = dplyr::case_when(
        stringr::str_detect(activity, pattern = ".administrator.") ~ "Administrator",
        stringr::str_detect(activity, pattern = ".fls_coordinator.") ~ "FLS Coordinator",
        stringr::str_detect(activity, pattern = ".nurse.") ~ "Nurse",
        stringr::str_detect(activity, pattern = ".doctor.") ~ "Doctor",
        stringr::str_detect(activity, pattern = ".radiographer.") ~ "Radiographer",
        stringr::str_detect(activity, pattern = ".allied_health.") ~ "Allied Health",
        stringr::str_detect(activity, pattern = ".other") ~ "Other",
        TRUE ~ NA_character_
      ),
      # what kind of activity?
      actv_label = dplyr::case_when(
        stringr::str_detect(activity, pattern = ".identification.") ~ "Identification",
        stringr::str_detect(activity, pattern = ".assessment.") ~ "Assessment",
        stringr::str_detect(activity, pattern = ".recommendation.") ~ "Treatment",
        stringr::str_detect(activity, pattern = ".monitoring.") ~ "Monitoring"
      ),

      staff = factor(
        staff,
        levels = c(
          "Administrator",
          "FLS Coordinator",
          "Nurse",
          "Doctor",
          "Radiographer",
          "Allied Health",
          "Other"
        )
      )
    ) |> dplyr::mutate(dplyr::across(value, as.numeric))
}






