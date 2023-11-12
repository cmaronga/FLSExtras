#' Preparing FLS_Monitoring_100 model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Monitoring_100 <- function(df_sheet){

  # All the variables get a value of 1 and source: Scenario FLS monitoring = 100%

  vars_to_alter <- c("fls.monitored.spine.4m.male", "fls.monitored.hip.4m.male",
                     "fls.monitored.other.4m.male", "fls.monitored.spine.12m.male",
                     "fls.monitored.hip.12m.male", "fls.monitored.other.12m.male",
                     # female
                     "fls.monitored.spine.4m.female", "fls.monitored.hip.4m.female",
                     "fls.monitored.other.4m.female", "fls.monitored.spine.12m.female",
                     "fls.monitored.hip.12m.female", "fls.monitored.other.12m.female")

  # mutate and replace
  df_sheet |>
    dplyr::mutate(
      # replace value
      Value = replace(Value, which(name %in% vars_to_alter), 1),

      # replace source
      Source = replace(Source, which(name %in% vars_to_alter), "Scenario FLS monitoring = 100%")
    )
}
