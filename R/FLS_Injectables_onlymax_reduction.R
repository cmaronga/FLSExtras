#' Preparing FLS_Injectables_onlymax_reduction model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Injectables_onlymax_reduction <- function(df_sheet){

  # rows B63toB73
  # b64_b70_b73 == 0 and b71 == 0.67, b72 == 0.33
  b64_b70.b73 <- c("fls.trt.alendronate.spine.male",
                   "fls.trt.risedronate.spine.male",
                   "fls.trt.strontium.spine.male",
                   "fls.trt.ibandronate.spine.male",
                   "fls.trt.raloxifene.spine.male") # these change to zero

  # sum check for the optimal available injectable
  df_col_check <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.spine.male", "fls.trt.abaloparatide.spine.male")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )

  colnames.1 <- df_col_check |> dplyr::select(name) |> dplyr::pull(name)


  # If both of them are un-available (sum is zero)
  if (sum(df_col_check$col_check) == 0) stop("Both the optimal injectable are not available, contact Rafa: ",
                                             df_col_check$name)

  # The three inflatables to get a zero if the first condition is TRUE
  inject_reset <- c("fls.trt.denosumab.spine.male",
                    "fls.trt.zoledronate.spine.male",
                    "fls.trt.romo.spine.male")

  # The actual change based on conditions
  if (sum(df_col_check$col_check) == 2){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b64_b70.b73, inject_reset)), 0),
        Source = replace(Source, which(name %in% inject_reset), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b64_b70.b73), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.teriparatide.spine.male"), 0.67),
        Value = replace(Value, which(name == "fls.trt.abaloparatide.spine.male"), 0.33),
        Source = replace(Source, which(name %in% colnames.1),
                         "Optimal injectable for this cohort")
      )
  } else if (sum(df_col_check$col_check) == 1){

    # Column that has a 1
    col_val.one <- df_col_check |>dplyr:: filter(col_check == 1) |>
      dplyr::pull(name)

    # Column that has a 0
    col_val.zero <- df_col_check |> dplyr::filter(col_check == 0) |>
      dplyr::pull(name)

    # Modify dataset
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b64_b70.b73, inject_reset, col_val.zero)), 0),
        Source = replace(Source, which(name %in% inject_reset), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% col_val.zero), "Optimal injectable for this cohort but not used by FLSs"),
        Source = replace(Source, which(name %in% b64_b70.b73), "Not injectable, hence = 0"),
        Value = replace(Value, which(name %in% col_val.one), 1),
        Source = replace(Source, which(name %in% col_val.one), "Optimal injectable for this cohort")
      )
  }



  # rows b86 to B95
  # changing to zero; b86_b92 and b95
  # b93 == 0.67 and b94 == 0.33

  b86_b92.b95 <- c("fls.trt.alendronate.hip.male",
                   "fls.trt.risedronate.hip.male",
                   "fls.trt.strontium.hip.male",
                   "fls.trt.ibandronate.hip.male",
                   "fls.trt.raloxifene.hip.male")

  # checking availability of optimal injectable
  df_col_check2 <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.hip.male", "fls.trt.abaloparatide.hip.male")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )

  colnames.2 <- df_col_check2 |> dplyr::select(name) |> dplyr::pull(name)

  # If both of them are un-available
  if (sum(df_col_check2$col_check) == 0 & !in_country %in% c("Netherlands", "France")) stop("Both the optimal injectable are not available, contact Rafa: ",
                                                                                            df_col_check2$name)

  # The three inflatables to get a zero if the first condition is TRUE
  inject_reset2 <- c("fls.trt.denosumab.hip.male",
                     "fls.trt.zoledronate.hip.male",
                     "fls.trt.romo.hip.male")

  if (in_country == "Netherlands"){
    # For Netherlands, this cohort do not have the optimal injectable
    # Using values provided by Rafa {Romo = 0.60, Deno = 0.30 and Zole = 0.10}

    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b86_b92.b95, df_col_check2$name)), 0), # teri and abalo {already 0 value}
        Source = replace(Source, which(name %in% df_col_check2$name), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b86_b92.b95), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.romo.hip.male"), 0.6),
        Value = replace(Value, which(name == "fls.trt.denosumab.hip.male"), 0.3),
        Value = replace(Value, which(name == "fls.trt.zoledronate.hip.male"), 0.1),
        Source = replace(Source, which(name %in% inject_reset2),
                         "Optimal injectable for this cohort(values by Rafa)")
      )
  } else if (in_country == "France"){

    # For France, this cohort do not have the optimal injectable
    # Using values provide by Rafa { Deno = 0.75 and Zole = 0.25}

    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b86_b92.b95, df_col_check2$name)), 0), # teri and abalo {already 0 value}
        Source = replace(Source, which(name %in% df_col_check2$name), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b86_b92.b95), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.romo.hip.male"), 0),
        Value = replace(Value, which(name == "fls.trt.denosumab.hip.male"), 0.75),
        Value = replace(Value, which(name == "fls.trt.zoledronate.hip.male"), 0.25),
        Source = replace(Source, which(name %in% c("fls.trt.denosumab.hip.male", "fls.trt.zoledronate.hip.male")),
                         "Optimal injectable for this cohort(values by Rafa)")
      )
  } else {

    if(sum(df_col_check2$col_check) == 2){
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b86_b92.b95, inject_reset2)), 0),
          Source = replace(Source, which(name %in% inject_reset2), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% b86_b92.b95), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == "fls.trt.teriparatide.hip.male"), 0.67),
          Value = replace(Value, which(name == "fls.trt.abaloparatide.hip.male"), 0.33),
          Source = replace(Source, which(name %in% colnames.2),
                           "Optimal injectable for this cohort")
        )
    } else if (sum(df_col_check2$col_check) == 1){
      # Column that has a 1
      col_val.one.2 <- df_col_check2 |> dplyr::filter(col_check == 1) |>
        dplyr::pull(name)

      # Column that has a 0
      col_val.zero.2 <- df_col_check2 |> dplyr::filter(col_check == 0) |>
        dplyr::pull(name)

      # Modify dataset
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b86_b92.b95, inject_reset2, col_val.zero.2)), 0),
          Source = replace(Source, which(name %in% inject_reset2), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% col_val.zero.2), "Optimal injectable for this cohort but not used by FLSs"),
          Source = replace(Source, which(name %in% b86_b92.b95), "Not injectable, hence = 0"),
          Value = replace(Value, which(name %in% col_val.one.2), 1),
          Source = replace(Source, which(name %in% col_val.one.2), "Optimal injectable for this cohort")
        )
    }

  }


  # rows b108 to b117:
  # changing to zero; b108_b114 and b117
  # b115 == 067, b116 == 0.33

  b108_b114.b117 <- c(
    "fls.trt.alendronate.other.male",
    "fls.trt.risedronate.other.male",
    "fls.trt.strontium.other.male",
    "fls.trt.ibandronate.other.male",
    "fls.trt.raloxifene.other.male")

  df_col_check3 <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.other.male", "fls.trt.abaloparatide.other.male")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )

  colnames.3 <- df_col_check3 |> dplyr::select(name) |> dplyr::pull(name)

  # If both of them are un-available
  if (sum(df_col_check3$col_check) == 0 & in_country != "France") stop("Both the optimal injectable are not available, contact Rafa: ",
                                                                       df_col_check3$name)

  # The three inflatables to get a ero if the first condition is TRUE
  inject_reset3 <- c("fls.trt.denosumab.other.male",
                     "fls.trt.zoledronate.other.male",
                     "fls.trt.romo.other.male")

  if (in_country == "France"){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b108_b114.b117, df_col_check3$name)), 0), # teri and abalo {already 0 value}
        Source = replace(Source, which(name %in% df_col_check3$name), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b108_b114.b117), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.romo.other.male"), 0),
        Value = replace(Value, which(name == "fls.trt.denosumab.other.male"), 0.75),
        Value = replace(Value, which(name == "fls.trt.zoledronate.other.male"), 0.25),
        Source = replace(Source, which(name %in% c("fls.trt.denosumab.other.male", "fls.trt.zoledronate.other.male")),
                         "Optimal injectable for this cohort(values by Rafa)")
      )
  } else {

    if (sum(df_col_check3$col_check) == 2){
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b108_b114.b117, inject_reset3)), 0),
          Source = replace(Source, which(name %in% inject_reset3), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% b108_b114.b117), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == "fls.trt.teriparatide.other.male"), 0.67),
          Value = replace(Value, which(name == "fls.trt.abaloparatide.other.male"), 0.33),
          Source = replace(Source, which(name %in% colnames.3),
                           "Optimal injectable for this cohort")
        )
    } else if (sum(df_col_check3$col_check) == 1){
      # Column that has a 1
      col_val.one.3 <- df_col_check3 |> dplyr::filter(col_check == 1) |>
        dplyr::pull(name)

      # Column that has a 0
      col_val.zero.3 <- df_col_check3 |> dplyr::filter(col_check == 0) |>
        dplyr::pull(name)

      # Modify dataset
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b108_b114.b117, inject_reset3, col_val.zero.3)), 0),
          Source = replace(Source, which(name %in% inject_reset3), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% col_val.zero.3), "Optimal injectable for this cohort but not used by FLSs"),
          Source = replace(Source, which(name %in% b108_b114.b117), "Not injectable, hence = 0"),
          Value = replace(Value, which(name %in% col_val.one.3), 1),
          Source = replace(Source, which(name %in% col_val.one.3), "Optimal injectable for this cohort")
        )
    }
  }


  # rows b130 to b139
  # changes to zero:b130_b136 and b139

  b130_b136.b139 <- c(
    "fls.trt.alendronate.spine.female",
    "fls.trt.risedronate.spine.female",
    "fls.trt.strontium.spine.female",
    "fls.trt.ibandronate.spine.female",
    "fls.trt.raloxifene.spine.female")

  df_col_check4 <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.spine.female", "fls.trt.abaloparatide.spine.female")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )
  colnames.4 <- df_col_check4 |> dplyr::select(name) |> dplyr::pull(name)

  # If both of them are un-available
  if (sum(df_col_check4$col_check) == 0) stop("Both the optimal injectable are not available, contact Rafa: ",
                                              df_col_check4$name)

  # The three inflatables to get a ero if the first condition is TRUE
  inject_reset4 <- c("fls.trt.denosumab.spine.female",
                     "fls.trt.zoledronate.spine.female",
                     "fls.trt.romo.spine.female")


  if (sum(df_col_check4$col_check) == 2){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b130_b136.b139, inject_reset4)), 0),
        Source = replace(Source, which(name %in% inject_reset4), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b130_b136.b139), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.teriparatide.spine.female"), 0.67),
        Value = replace(Value, which(name == "fls.trt.abaloparatide.spine.female"), 0.33),
        Source = replace(Source, which(name %in% colnames.4),
                         "Optimal injectable for this cohort")
      )
  } else if (sum(df_col_check4$col_check) == 1){
    # Column that has a 1
    col_val.one.4 <- df_col_check4 |> dplyr::filter(col_check == 1) |>
      dplyr::pull(name)

    # Column that has a 0
    col_val.zero.4 <- df_col_check4 |> dplyr::filter(col_check == 0) |>
      dplyr::pull(name)

    # Modify dataset
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b130_b136.b139, inject_reset4, col_val.zero.4)), 0),
        Source = replace(Source, which(name %in% inject_reset4), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% col_val.zero.4), "Optimal injectable for this cohort but not used by FLSs"),
        Source = replace(Source, which(name %in% b130_b136.b139), "Not injectable, hence = 0"),
        Value = replace(Value, which(name %in% col_val.one.4), 1),
        Source = replace(Source, which(name %in% col_val.one.4), "Optimal injectable for this cohort")
      )
  }

  # rows b152 to b161
  # changes to zero b152_b158
  # b160, b161 all 0.33 and b159 == 0.34

  b152_b158 <- c("fls.trt.alendronate.hip.female",
                 "fls.trt.risedronate.hip.female",
                 "fls.trt.strontium.hip.female",
                 "fls.trt.ibandronate.hip.female",
                 "fls.trt.raloxifene.hip.female")

  df_col_check5 <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.hip.female", "fls.trt.abaloparatide.hip.female",
                       "fls.trt.romo.hip.female")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )

  # colnames
  colnames.5 <- df_col_check5 |> dplyr::select(name) |> dplyr::pull(name)

  # If both of them are un-available
  if (sum(df_col_check5$col_check) == 0 & in_country != "France") stop("Both the optimal injectable are not available, contact Rafa: ",
                                                                       df_col_check5$name)

  # The three inflatables to get a ero if the first condition is TRUE
  inject_reset5 <- c("fls.trt.denosumab.hip.female",
                     "fls.trt.zoledronate.hip.female")


  if (in_country == "France"){
    # For France, this cohort do not have the optimal injectable
    # Using values provide by Rafa { Deno = 0.75 and Zole = 0.25}

    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b152_b158, df_col_check5$name)), 0), # teri and abalo {already 0 value}
        Source = replace(Source, which(name %in% df_col_check5$name), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b152_b158), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.romo.hip.female"), 0),
        Value = replace(Value, which(name == "fls.trt.denosumab.hip.female"), 0.75),
        Value = replace(Value, which(name == "fls.trt.zoledronate.hip.female"), 0.25),
        Source = replace(Source, which(name %in% c("fls.trt.denosumab.hip.female", "fls.trt.zoledronate.hip.female")),
                         "Optimal injectable for this cohort(values by Rafa)")
      )


  } else {

    if (sum(df_col_check5$col_check) == 3){
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b152_b158, inject_reset5)), 0),
          Source = replace(Source, which(name %in% inject_reset5), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% b152_b158), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == "fls.trt.teriparatide.hip.female"), 0.34),
          Value = replace(Value, which(name == "fls.trt.abaloparatide.hip.female"), 0.33),
          Value = replace(Value, which(name == "fls.trt.romo.hip.female"), 0.33),
          Source = replace(Source, which(name %in% colnames.5),
                           "Optimal injectable for this cohort")
        )
    } else if (sum(df_col_check5$col_check) == 2){
      # Column that has a 1
      col_val.one.5 <- df_col_check5 |> dplyr::filter(col_check == 1) |>
        dplyr::pull(name)

      # Column that has a 0
      col_val.zero.5 <- df_col_check5 |> dplyr::filter(col_check == 0) |>
        dplyr::pull(name)

      # Modify dataset
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b152_b158, inject_reset5, col_val.zero.5)), 0),
          Source = replace(Source, which(name %in% inject_reset5), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% col_val.zero.5), "Optimal injectable for this cohort but not used by FLSs"),
          Source = replace(Source, which(name %in% b152_b158), "Not injectable, hence = 0"),
          Value = replace(Value, which(name %in% col_val.one.5), 0.5),
          Source = replace(Source, which(name %in% col_val.one.5), "Optimal injectable for this cohort")
        )


    } else if (sum(df_col_check5$col_check) == 1){
      # Column that has a 1
      col_val.one.5 <- df_col_check5 |> dplyr::filter(col_check == 1) |>
        dplyr::pull(name)

      # Column that has a 0
      col_val.zero.5 <- df_col_check5 |> dplyr::filter(col_check == 0) |>
        dplyr::pull(name)

      # Modify dataset
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b152_b158, inject_reset5, col_val.zero.5)), 0),
          Source = replace(Source, which(name %in% inject_reset5), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% col_val.zero.5), "Optimal injectable for this cohort but not used by FLSs"),
          Source = replace(Source, which(name %in% b152_b158), "Not injectable, hence = 0"),
          Value = replace(Value, which(name %in% col_val.one.5), 1),
          Source = replace(Source, which(name %in% col_val.one.5), "Optimal injectable for this cohort")
        )
    }
  }


  # rows b174 to b183
  # changes to 0; b174_b180 and b183
  # b181 == 0.67
  # b182 == 0.33

  b174_b180.b183 <- c("fls.trt.alendronate.other.female",
                      "fls.trt.risedronate.other.female",
                      "fls.trt.strontium.other.female",
                      "fls.trt.ibandronate.other.female",
                      "fls.trt.raloxifene.other.female")

  df_col_check6 <- df_sheet |>
    dplyr::filter(name %in% c("fls.trt.teriparatide.other.female", "fls.trt.abaloparatide.other.female")) |>
    dplyr::select(name, Value) |>
    dplyr::mutate(
      col_check = dplyr::if_else(Value > 0, 1, 0)
    )

  colnames.6 <- df_col_check6 |> dplyr::select(name) |> dplyr::pull(name)

  # If both of them are un-available
  if (sum(df_col_check6$col_check) == 0 & in_country != "France") stop("Both the optimal injectable are not available, contact Rafa: ",
                                                                       df_col_check6$name)

  # The three inflatables to get a ero if the first condition is TRUE
  inject_reset6 <- c("fls.trt.denosumab.other.female",
                     "fls.trt.zoledronate.other.female",
                     "fls.trt.romo.other.female")

  if (in_country == "France"){

    # For France, this cohort do not have the optimal injectable
    # Using values provide by Rafa { Deno = 0.75 and Zole = 0.25}

    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% c(b174_b180.b183, df_col_check6$name)), 0), # teri and abalo {already 0 value}
        Source = replace(Source, which(name %in% df_col_check6$name), "Not the optimal injectable for this cohort"),
        Source = replace(Source, which(name %in% b174_b180.b183), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == "fls.trt.romo.other.female"), 0),
        Value = replace(Value, which(name == "fls.trt.denosumab.other.female"), 0.75),
        Value = replace(Value, which(name == "fls.trt.zoledronate.other.female"), 0.25),
        Source = replace(Source, which(name %in% c("fls.trt.denosumab.other.female", "fls.trt.zoledronate.other.female")),
                         "Optimal injectable for this cohort(values by Rafa)")
      )

  } else {
    if (sum(df_col_check6$col_check) == 2){
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b174_b180.b183, inject_reset6)), 0),
          Source = replace(Source, which(name %in% inject_reset6), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% b174_b180.b183), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == "fls.trt.teriparatide.other.female"), 0.67),
          Value = replace(Value, which(name == "fls.trt.abaloparatide.other.female"), 0.33),
          Source = replace(Source, which(name %in% colnames.6),
                           "Optimal injectable for this cohort")
        )
    } else if (sum(df_col_check6$col_check) == 1){
      # Column that has a 1
      col_val.one.6 <- df_col_check6 |> dplyr::filter(col_check == 1) |>
        dplyr::pull(name)

      # Column that has a 0
      col_val.zero.6 <- df_col_check6 |> dplyr::filter(col_check == 0) |>
        dplyr::pull(name)

      # Modify dataset
      df_sheet <- df_sheet |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% c(b174_b180.b183, inject_reset6, col_val.zero.6)), 0),
          Source = replace(Source, which(name %in% inject_reset6), "Not the optimal injectable for this cohort"),
          Source = replace(Source, which(name %in% col_val.zero.6), "Optimal injectable for this cohort but not used by FLSs"),
          Source = replace(Source, which(name %in% b174_b180.b183), "Not injectable, hence = 0"),
          Value = replace(Value, which(name %in% col_val.one.6), 1),
          Source = replace(Source, which(name %in% col_val.one.6), "Optimal injectable for this cohort")
        )
    }
  }

  # -------------------------------------------------------------------------

  # rows 202 to 208
  # always force b201 == 1, the rest 0
  b202_b208 <- c("romo.to.alendronate.fls.male",
                 "romo.to.risedronate.fls.male",
                 "romo.to.strontium.fls.male",
                 "romo.to.ibandronate.fls.male",
                 "romo.to.raloxifene.fls.male",
                 "romo.to.denosumab.fls.male",
                 "romo.to.zoledronate.fls.male")

  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b202_b208), 0),
      Source = replace(Source, which(name %in% b202_b208), "Not injectable, hence = 0"),
      Value = replace(Value, which(name == "romo.to.nothing.fls.male"), 1)
    )

  # rows  210 to b216
  # always force b209 == 0, b210_b214 == 0
  # b215 == 0.5
  # b216 == 0.5

  b209_b214 <- c("romo.to.nothing.fls.female",
                 "romo.to.alendronate.fls.female",
                 "romo.to.risedronate.fls.female",
                 "romo.to.strontium.fls.female",
                 "romo.to.ibandronate.fls.female",
                 "romo.to.raloxifene.fls.female")
  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b209_b214), 0),
      Source = replace(Source, which(name %in% b209_b214), "Not injectable, hence = 0"),
      Value = replace(Value, which(name %in% c("romo.to.denosumab.fls.female",
                                               "romo.to.zoledronate.fls.female")), 0.5)
    )

  # rows 234 to 241
  # always force b234 == 1
  # the rest b235_b241 == 0

  b235_b241 <- c("abaloparatide.to.alendronate.fls.male",
                 "abaloparatide.to.risedronate.fls.male",
                 "abaloparatide.to.strontium.fls.male",
                 "abaloparatide.to.ibandronate.fls.male",
                 "abaloparatide.to.raloxifene.fls.male",
                 "abaloparatide.to.denosumab.fls.male",
                 "abaloparatide.to.zoledronate.fls.male")

  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b235_b241), 0),
      Source = replace(Source, which(name %in% b235_b241), "Not injectable, hence = 0"),
      Value = replace(Value, which(name == "abaloparatide.to.nothing.fls.male"), 1)
    )

  # rows b242 to b249
  # b243_b247 - changes to 0
  # b248 and b249 == 0.5 each

  b243_b247 <- c("abaloparatide.to.alendronate.fls.female",
                 "abaloparatide.to.risedronate.fls.female",
                 "abaloparatide.to.strontium.fls.female",
                 "abaloparatide.to.ibandronate.fls.female",
                 "abaloparatide.to.raloxifene.fls.female")


  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b243_b247), 0),
      Source = replace(Source, which(name %in% b243_b247), "Not injectable, hence = 0"),
      Value = replace(Value, which(name %in% c("abaloparatide.to.denosumab.fls.female",
                                               "abaloparatide.to.zoledronate.fls.female")), 0.5)
    )


  # rows 267 to rows 274
  # b268_b272 == 0
  # b273 and b274 == 0.5 each

  b268_b272 <- c("teriparatide.to.alendronate.fls.male",
                 "teriparatide.to.risedronate.fls.male",
                 "teriparatide.to.strontium.fls.male",
                 "teriparatide.to.ibandronate.fls.male",
                 "teriparatide.to.raloxifene.fls.male")

  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b268_b272), 0),
      Source = replace(Source, which(name %in% b268_b272), "Not injectable, hence = 0"),
      Value = replace(Value, which(name %in% c("teriparatide.to.denosumab.fls.male",
                                               "teriparatide.to.zoledronate.fls.male")), 0.5)
    )

  # rows 275 to rows 282
  # b276_b280 == 0
  # b281, b282 == 0.5 each

  b276_b280 <- c("teriparatide.to.alendronate.fls.female",
                 "teriparatide.to.risedronate.fls.female",
                 "teriparatide.to.strontium.fls.female",
                 "teriparatide.to.ibandronate.fls.female",
                 "teriparatide.to.raloxifene.fls.female")


  df_sheet <- df_sheet |>
    dplyr::mutate(
      Value = replace(Value, which(name %in% b276_b280), 0),
      Source = replace(Source, which(name %in% b276_b280), "Not injectable, hence = 0"),
      Value = replace(Value, which(name %in% c("teriparatide.to.denosumab.fls.female",
                                               "teriparatide.to.zoledronate.fls.female")), 0.5)
    )
  # -- End

}
