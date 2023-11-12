#' Preparing FLS_Treatment_initiation_one_month model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Treatment_initiation_one_month <- function(df_sheet){
  # Affected variables: fls.time_to_treat.hip, fls.time_to_treat.spine, fls.time_to_treat.other
  # -- they all get a value of 1 and source: Scenario "FLS Treatment initiation = 1 month"

  vars_to_alter <- c("fls.time_to_treat.hip",
                     "fls.time_to_treat.spine",
                     "fls.time_to_treat.other") # define variables to alter for this scenarion

  df_sheet |>
    dplyr::mutate(
      # replace value
      Value = replace(Value, which(name %in% vars_to_alter), 1),

      # replace source
      Source = replace(Source, which(name %in% vars_to_alter), "Scenario FLS treatment initiation 1 month")
    )
}
