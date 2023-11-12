#' Preparing FLS_Identification_100 model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Identification_100 <- function(df_sheet){
  # Affected variables:-- fls.prob_identification.hip, fls.prob_identification.spine and fls.prob_identification.other
  # All take value(replaced with) of 1 and source Scenario: "FLS Identification = 100%"

  vars_to_alter <- c("fls.prob_identification.hip",
                     "fls.prob_identification.other",
                     "fls.prob_identification.spine") # define vector of variables to alter
  df_sheet |>
    dplyr::mutate(
      # replace value
      Value = replace(Value, which(name %in% vars_to_alter), 1),

      # replace source
      Source = replace(Source, which(name %in% vars_to_alter),
                       "Scenario FLS Identification = 100%")
    )
}
