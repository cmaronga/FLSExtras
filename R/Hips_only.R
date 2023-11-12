#' Preparing FLS Hips_only model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
Hips_only <- function(df_sheet){

  # a few rows, the value column changes to 0
  hips_only <- c(
    "risk_spine_fracture_after_hip_10y.male",
    "risk_spine_fracture_after_hip_5y.male",

    "risk_other_fracture_after_hip_10y.male",
    "risk_other_fracture_after_hip_5y.male",

    "risk_hip_fracture_after_spine_10y.male",
    "risk_spine_fracture_after_spine_5y.male",

    "risk_other_fracture_after_spine_10y.male",
    "risk_other_fracture_after_spine_5y.male",

    "risk_hip_fracture_after_other_10y.male",
    "risk_hip_fracture_after_other_5y.male",

    "risk_spine_fracture_after_other_10y.male",
    "risk_spine_fracture_after_other_5y.male",

    "risk_other_fracture_after_other_10y.male",
    "risk_other_fracture_after_other_5y.male",

    "risk_spine_fracture_after_hip_10y.female",
    "risk_spine_fracture_after_hip_5y.female",

    "risk_other_fracture_after_hip_10y.female",
    "risk_other_fracture_after_hip_5y.female",

    "risk_hip_fracture_after_spine_10y.female",
    "risk_spine_fracture_after_spine_5y.female",

    "risk_other_fracture_after_spine_10y.female",
    "risk_other_fracture_after_spine_5y.female",

    "risk_hip_fracture_after_other_10y.female",
    "risk_hip_fracture_after_other_5y.female",

    "risk_spine_fracture_after_other_10y.female",
    "risk_spine_fracture_after_other_5y.female",

    "risk_other_fracture_after_other_10y.female",
    "risk_other_fracture_after_other_5y.female"
  )

  df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% hips_only), 0),
      Source = replace(Source, which(name %in% hips_only), "Hips only"))

  # -- End

}
