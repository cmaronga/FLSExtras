
#' Pivot wider treatment practice input data
#'
#' @param df A data.frame, treatment practice inputs
#'
#' @return A long format data.frame
#' @export
#'
#' @examples
df_longate <- function(df) {  # data tranforming function (function to pivot wider, treatment practice data)
  df |>
    tidyr::pivot_longer(
      cols = c(Spine, Hip, Other),
      names_to = "site",
      values_to = "prop"
    )
}
