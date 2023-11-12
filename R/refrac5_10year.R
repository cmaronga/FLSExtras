#' Contingency tables for 5 & 10 year re-fracture risk model inputs
#'
#' @param dframe A data.frame, model specific inputs from googlesheet
#' @param index Fracture site, one of `hip`, `spine`, `other`
#'
#' @return A data.frame
#' @export
#'
#' @examples
refrac5_10year <- function(dframe = NULL,
                           index = NULL) {
  dframe |>
    dplyr::filter(stringr::str_detect(name, pattern = paste0("_after_", index, "_"))) |>  # detect patern of interest
    dplyr::mutate(
      # fracture site
      `Index fracture site` = dplyr::if_else(
        index == "other",
        paste("Other fragility fracture"),
        stringr::str_to_sentence(index)
      ),
      Refracture = dplyr::case_when(
        stringr::str_detect(name, "risk_other") ~ "Other fragility fracture",
        stringr::str_detect(name, "risk_hip") ~ "Hip",
        stringr::str_detect(name, "risk_spine") ~ "Spine"
      ),
      Value = scales::percent(Value, accuracy = 0.1)
    ) |> dplyr::select(-name) |> tidyr::pivot_wider(names_from = Refracture, values_from = Value)
}
