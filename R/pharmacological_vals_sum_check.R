#' Sum 1 check for select pharmacological values
#'
#' @param df_sheet Scenario value inputs
#' @param base_vars A vector of pharmacological values
#' @param gender Either `male` or `female`
#' @param site One of `hip`, `spine` or `other`
#' @param arm One of `fls` or `no.fls`
#'
#' @return A vector sum 1's
#' @export
#'
#' @examples
pharmacological_vals_sum_check <- function(df_sheet, base_vars = NULL,
  gender = NULL,
  site = NULL,
  arm = NULL){

  # vector of pre-select pharmalogical values
  if(is.null(base_vars)) {
    base_vars <- c(
      "arm.trt.alendronate.site.gender",
      "arm.trt.risedronate.site.gender",
      "arm.trt.strontium.site.gender",
      "arm.trt.ibandronate.site.gender",
      "arm.trt.raloxifene.site.gender",
      "arm.trt.denosumab.site.gender",
      "arm.trt.zoledronate.site.gender",
      "arm.trt.teriparatide.site.gender",
      "arm.trt.abaloparatide.site.gender",
      "arm.trt.romo.site.gender"
    )
  }

  # now, create vector of variable names
  var_list <- stringr::str_replace_all(base_vars,
                              pattern = "gender",
                              replacement = gender) |>  # replace gender
    stringr::str_replace_all(pattern = "site",
                    replacement = site) |> # replace site
    stringr::str_replace_all(pattern = "arm",
                    replacement = arm) # replace arm

  # filter from dataset and get the sum
  var_sum <- df_sheet |>
    dplyr::filter(name %in% var_list) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.double() |> sum()

  message("The SUM is ", var_sum)

  # if (var_sum != 1) stop("10 rows of 'After ", site, "-", arm, "-", gender, " Values MUST sum to 1:",
  #                        "-", var_sum,"-")

}
