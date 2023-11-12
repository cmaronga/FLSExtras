#' Preparing FLS_Alendronate_only model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Alendronate_only <- function(df_sheet){

  # 1. After spine (fls) - male
  vars_B65_B73 <- c("fls.trt.risedronate.spine.male",
                    "fls.trt.strontium.spine.male",
                    "fls.trt.ibandronate.spine.male",
                    "fls.trt.raloxifene.spine.male",
                    "fls.trt.denosumab.spine.male",
                    "fls.trt.zoledronate.spine.male",
                    "fls.trt.teriparatide.spine.male",
                    "fls.trt.abaloparatide.spine.male",
                    "fls.trt.romo.spine.male")

  # test value/cell
  B65_B73_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.spine.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (B65_B73_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B65_B73), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.spine.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # 2. After hip (fls) - male - male
  vars_B87_B95 <- c("fls.trt.risedronate.hip.male",
                    "fls.trt.strontium.hip.male",
                    "fls.trt.ibandronate.hip.male",
                    "fls.trt.raloxifene.hip.male",
                    "fls.trt.denosumab.hip.male",
                    "fls.trt.zoledronate.hip.male",
                    "fls.trt.teriparatide.hip.male",
                    "fls.trt.abaloparatide.hip.male",
                    "fls.trt.romo.hip.male")

  # test value/cell
  vars_B87_B95_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.hip.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B87_B95_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B87_B95), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.hip.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # - 3. After other (fls) - male
  vars_B109_B117 <- c("fls.trt.risedronate.other.male",
                      "fls.trt.strontium.other.male",
                      "fls.trt.ibandronate.other.male",
                      "fls.trt.raloxifene.other.male",
                      "fls.trt.denosumab.other.male",
                      "fls.trt.zoledronate.other.male",
                      "fls.trt.teriparatide.other.male",
                      "fls.trt.abaloparatide.other.male",
                      "fls.trt.romo.other.male")

  # test value/cell
  vars_B109_B117_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.other.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B109_B117_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B109_B117), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.other.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # - 4. After spine (fls) - female --
  vars_B131_B139 <- c("fls.trt.risedronate.spine.female",
                      "fls.trt.strontium.spine.female",
                      "fls.trt.ibandronate.spine.female",
                      "fls.trt.raloxifene.spine.female",
                      "fls.trt.denosumab.spine.female",
                      "fls.trt.zoledronate.spine.female",
                      "fls.trt.teriparatide.spine.female",
                      "fls.trt.abaloparatide.spine.female",
                      "fls.trt.romo.spine.female")

  # test value/cell
  vars_B131_B139_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.spine.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B131_B139_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B131_B139), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.spine.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # 5 - After hip (fls) - female
  vars_B153_B161 <- c("fls.trt.risedronate.hip.female",
                      "fls.trt.strontium.hip.female",
                      "fls.trt.ibandronate.hip.female",
                      "fls.trt.raloxifene.hip.female",
                      "fls.trt.denosumab.hip.female",
                      "fls.trt.zoledronate.hip.female",
                      "fls.trt.teriparatide.hip.female",
                      "fls.trt.abaloparatide.hip.female",
                      "fls.trt.romo.hip.female")

  # test value/cell
  vars_B153_B161_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.hip.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B153_B161_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B153_B161), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.hip.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # 6. After other (fls) - female
  vars_B175_B183 <- c("fls.trt.risedronate.other.female",
                      "fls.trt.strontium.other.female",
                      "fls.trt.ibandronate.other.female",
                      "fls.trt.raloxifene.other.female",
                      "fls.trt.denosumab.other.female",
                      "fls.trt.zoledronate.other.female",
                      "fls.trt.teriparatide.other.female",
                      "fls.trt.abaloparatide.other.female",
                      "fls.trt.romo.other.female")

  # test value/cell
  vars_B175_B183_testval <- df_sheet |>
    dplyr::filter(name == "fls.trt.other.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B175_B183_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B175_B183), 0), # replace all with zero
        Value = replace(Value, which(name == "fls.trt.alendronate.other.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  ## FLS - male
  vars_B203_B208 <- c("romo.to.risedronate.fls.male",
                      "romo.to.strontium.fls.male",
                      "romo.to.ibandronate.fls.male",
                      "romo.to.raloxifene.fls.male",
                      "romo.to.denosumab.fls.male",
                      "romo.to.zoledronate.fls.male")

  # test value/cell
  vars_B203_B208_testval <- df_sheet |>
    dplyr::filter(name == "romo.to.nothing.fls.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B203_B208_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B203_B208), 0), # replace all with zero
        Value = replace(Value, which(name == "romo.to.alendronate.fls.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  ## FLS - female
  vars_B211_B216 <- c("romo.to.risedronate.fls.female",
                      "romo.to.strontium.fls.female",
                      "romo.to.ibandronate.fls.female",
                      "romo.to.raloxifene.fls.female",
                      "romo.to.denosumab.fls.female",
                      "romo.to.zoledronate.fls.female")

  # test value/cell
  vars_B211_B216_testval <- df_sheet |>
    dplyr::filter(name == "romo.to.nothing.fls.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B211_B216_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B211_B216), 0), # replace all with zero
        Value = replace(Value, which(name == "romo.to.alendronate.fls.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  #-- FLS, male
  vars_B236_B241 <- c("abaloparatide.to.risedronate.fls.male",
                      "abaloparatide.to.strontium.fls.male",
                      "abaloparatide.to.ibandronate.fls.male",
                      "abaloparatide.to.raloxifene.fls.male",
                      "abaloparatide.to.denosumab.fls.male",
                      "abaloparatide.to.zoledronate.fls.male")

  # test value/cell
  vars_B236_B241_testval <- df_sheet |>
    dplyr::filter(name == "abaloparatide.to.nothing.fls.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B236_B241_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B236_B241), 0), # replace all with zero
        Value = replace(Value, which(name == "abaloparatide.to.alendronate.fls.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  #-- FLS, female
  vars_B244_B249 <- c("abaloparatide.to.risedronate.fls.female",
                      "abaloparatide.to.strontium.fls.female",
                      "abaloparatide.to.ibandronate.fls.female",
                      "abaloparatide.to.raloxifene.fls.female",
                      "abaloparatide.to.denosumab.fls.female",
                      "abaloparatide.to.zoledronate.fls.female")

  # test value/cell
  vars_B244_B249_testval <- df_sheet |>
    dplyr::filter(name == "abaloparatide.to.nothing.fls.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B244_B249_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B244_B249), 0), # replace all with zero
        Value = replace(Value, which(name == "abaloparatide.to.alendronate.fls.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # cells 268-274
  vars_B269_B274 <- c("teriparatide.to.risedronate.fls.male",
                      "teriparatide.to.strontium.fls.male",
                      "teriparatide.to.ibandronate.fls.male",
                      "teriparatide.to.raloxifene.fls.male",
                      "teriparatide.to.denosumab.fls.male",
                      "teriparatide.to.zoledronate.fls.male")

  # test value/cell
  vars_B269_B274_testval <- df_sheet |>
    dplyr::filter(name == "teriparatide.to.nothing.fls.male") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B269_B274_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B269_B274), 0), # replace all with zero
        Value = replace(Value, which(name == "teriparatide.to.alendronate.fls.male"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }

  # cells 277 - 282
  vars_B277_B282 <- c("teriparatide.to.risedronate.fls.female",
                      "teriparatide.to.strontium.fls.female",
                      "teriparatide.to.ibandronate.fls.female",
                      "teriparatide.to.raloxifene.fls.female",
                      "teriparatide.to.denosumab.fls.female",
                      "teriparatide.to.zoledronate.fls.female")

  # test value/cell
  vars_B277_B282_testval <- df_sheet |>
    dplyr::filter(name == "teriparatide.to.nothing.fls.female") |>
    dplyr::select(Value) |> dplyr::pull() |> as.numeric()

  # based on the test value, implement the change
  if (vars_B277_B282_testval < 1){
    df_sheet <- df_sheet |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% vars_B277_B282), 0), # replace all with zero
        Value = replace(Value, which(name == "teriparatide.to.alendronate.fls.female"), 1)
      )
  } else {
    df_sheet <- df_sheet
  }
}
