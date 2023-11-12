#' Replace CP table values in FLS adherence and perfect FLS
#'
#' @param table_read FLS model output table i.e `summary.discounted.qol.t60.csv`
#' @param scenario_type FLS scenarion run
#'
#' @return A data.frame
#' @export
#'
#' @examples
replace_current_practice <-function(table_read = NULL, scenario_type = scenario_data.path) {

  # check if scenario is either Adherence 100 or perfect fls
  if (scenario_type %in% c("FLS_Adherence_100", "FLS_Perfect_FLS")) {

    ## this only happens for FLS adherence and perfect FLS
    sum_tbl1 <- readr::read_csv(here::here(default_path, table_read)) |>
      dplyr::arrange(intervention)

    # import dataset from base run
    sum_tbl2 <- readr::read_csv(here::here(current_prac_path, paste0("base_", table_read))) |>
      dplyr::arrange(intervention)

    # Replace the current practice in `sum_tbl1`(FLS adherence or Perfect FLS) with current practice in `sum_tbl2` (base_case)
    sum_tbl1 <- sum_tbl1 |>
      dplyr::slice(-1) |>                  # Remove the row to be replaced
      dplyr::bind_rows(sum_tbl2[1, ]) |>   # Append the row from base case Current practice
      dplyr::arrange(intervention)

  } else {
    # other scenarios
    sum_tbl1 <- readr::read_csv(here::here(default_path, table_read)) |>
      dplyr::arrange(intervention)
  }

}
