#' Output long numeric values nicely e.g 10,000 instead of 10000
#'
#' @param dframe A data.frame or tibble
#'
#' @return A data frame
#' @export
#'
#' @examples
nice_numOut <- function(dframe) {
  dframe |>
    dplyr::mutate_if(is.numeric, ~ comma(.)) |>
    dplyr::mutate_at(dplyr::vars(ncol(dframe)), ~ sub("\\.\\d+$", "", .)) # just to make sure last column has no decimal points
}
