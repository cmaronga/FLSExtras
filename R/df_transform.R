#' Transform or pivot FLS tables
#'
#' @param d_frame A data.frame or table from FLS model run, e.g `summary.second.fx.t60.csv`
#' @param names_col A character to be used in pivot_longer as column header
#'
#' @return A data.frame, which is a presentation ready table
#' @export
#'
#' @examples
#'

df_transform <- function(d_frame,              # table from PFC model
                         names_col = "metric") {
  # takes a table and double-transforms it (longer, then wider)
  d_frame |>
    # first pivot longer
    tidyr::pivot_longer(cols = -1,
                 names_to = {
                   {
                     names_col
                   }
                 },
                 values_to = "vals") |>
    # then pivot wider
    tidyr::pivot_wider(names_from = 1,
                values_from = 3)
}
