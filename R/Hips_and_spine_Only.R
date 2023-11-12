#' Preparing FLS Hips_and_spine_Only model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
Hips_and_spine_Only <- function(df_sheet) {

  # a couple of rows, values change to zero
  hip_spine_vars <- c("risk_other_fracture_after_hip_10y.male",
                      "risk_other_fracture_after_hip_5y.male",

                      "risk_other_fracture_after_spine_10y.male",
                      "risk_other_fracture_after_spine_5y.male",

                      "risk_hip_fracture_after_other_10y.male",
                      "risk_hip_fracture_after_other_5y.male",

                      "risk_spine_fracture_after_other_10y.male",
                      "risk_spine_fracture_after_other_5y.male",

                      "risk_other_fracture_after_other_10y.male",
                      "risk_other_fracture_after_other_5y.male",

                      "risk_other_fracture_after_hip_10y.female",
                      "risk_other_fracture_after_hip_5y.female",

                      "risk_other_fracture_after_spine_10y.female",
                      "risk_other_fracture_after_spine_5y.female",

                      "risk_hip_fracture_after_other_10y.female",
                      "risk_hip_fracture_after_other_5y.female",

                      "risk_spine_fracture_after_other_10y.female",
                      "risk_spine_fracture_after_other_5y.female",

                      "risk_other_fracture_after_other_10y.female",
                      "risk_other_fracture_after_other_5y.female")

  df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% hip_spine_vars), 0),
      Source = replace(Source, which(name %in% hip_spine_vars), "Hips and Spine only"))

  # -- End
}
