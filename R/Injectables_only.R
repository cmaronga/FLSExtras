#' Preparing FLS Hips_only model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
Injectables_only <- function(df_sheet){
  # A.
  # b64 to b68 gets a zero
  b64_b68 <- c("fls.trt.alendronate.spine.male",
               "fls.trt.risedronate.spine.male",
               "fls.trt.strontium.spine.male",
               "fls.trt.ibandronate.spine.male",
               "fls.trt.raloxifene.spine.male")

  # common sum (b69-b73)
  b69_b73 <- c("fls.trt.denosumab.spine.male",
               "fls.trt.zoledronate.spine.male",
               "fls.trt.teriparatide.spine.male",
               "fls.trt.abaloparatide.spine.male",
               "fls.trt.romo.spine.male")
  # sumA
  sum_a <- df_sheet |>
    dplyr::filter(name %in% b69_b73) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()
  # actual change
  # 1. make check for the sum

  if (sum_a == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b64_b68), 0),
        Source = replace(Source, which(name %in% b64_b68), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b69_b73[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b69_b73[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b64_b68), 0),
        Source = replace(Source, which(name %in% b64_b68), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b69_b73, (Value/sum_a), Value)
      )

  }

  # Scenario B
  # rows b86-b90 all get a value of 0
  b86_b90 <- c("fls.trt.alendronate.hip.male",
               "fls.trt.risedronate.hip.male",
               "fls.trt.strontium.hip.male",
               "fls.trt.ibandronate.hip.male",
               "fls.trt.raloxifene.hip.male")

  # variables for common sum
  # b91-b95
  b91_b95 <- c("fls.trt.denosumab.hip.male",
               "fls.trt.zoledronate.hip.male",
               "fls.trt.teriparatide.hip.male",
               "fls.trt.abaloparatide.hip.male",
               "fls.trt.romo.hip.male")

  # the actual sum
  sum_b <- df_sheet |>
    dplyr::filter(name %in% b91_b95) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

  # The actual change conditional on the sum
  if (sum_b == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b86_b90), 0),
        Source = replace(Source, which(name %in% b86_b90), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b91_b95[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b91_b95[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b86_b90), 0),
        Source = replace(Source, which(name %in% b86_b90), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b91_b95, (Value/sum_b), Value)
      )
  }

  # scenario set C
  # rows b108 to b112 gets a 0
  b108_b112 <- c("fls.trt.alendronate.other.male",
                 "fls.trt.risedronate.other.male",
                 "fls.trt.strontium.other.male",
                 "fls.trt.ibandronate.other.male",
                 "fls.trt.raloxifene.other.male")

  # rows containing common sum
  b113_b117 <- c("fls.trt.denosumab.other.male",
                 "fls.trt.zoledronate.other.male",
                 "fls.trt.teriparatide.other.male",
                 "fls.trt.abaloparatide.other.male",
                 "fls.trt.romo.other.male")

  # compute the sum(common denominator)
  sum_c <- df_sheet |>
    dplyr::filter(name %in% b113_b117) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()



  # The actual change conditional on the sum
  if (sum_c == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b108_b112), 0),
        Source = replace(Source, which(name %in% b108_b112), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b113_b117[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b113_b117[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b108_b112), 0),
        Source = replace(Source, which(name %in% b108_b112), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b113_b117, (Value/sum_c), Value)
      )
  }


  # scenario set D
  # rows b130 to b134 gets a 0
  b130_b134 <- c("fls.trt.alendronate.spine.female",
                 "fls.trt.risedronate.spine.female",
                 "fls.trt.strontium.spine.female",
                 "fls.trt.ibandronate.spine.female",
                 "fls.trt.raloxifene.spine.female")

  # rows containing common sum
  b135_b139 <- c("fls.trt.denosumab.spine.female",
                 "fls.trt.zoledronate.spine.female",
                 "fls.trt.teriparatide.spine.female",
                 "fls.trt.abaloparatide.spine.female",
                 "fls.trt.romo.spine.female")

  # compute the sum(common denominator)
  sum_d <- df_sheet |>
    dplyr::filter(name %in% b135_b139) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()



  # The actual change conditional on the sum
  if (sum_d == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b130_b134), 0),
        Source = replace(Source, which(name %in% b130_b134), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b135_b139[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b135_b139[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b130_b134), 0),
        Source = replace(Source, which(name %in% b130_b134), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b135_b139, (Value/sum_d), Value)
      )
  }


  # scenario set E
  # rows b152 to b156 gets a 0
  b152_b156 <- c("fls.trt.alendronate.hip.female",
                 "fls.trt.risedronate.hip.female",
                 "fls.trt.strontium.hip.female",
                 "fls.trt.ibandronate.hip.female",
                 "fls.trt.raloxifene.hip.female")

  # rows containing common sum
  b157_b161 <- c("fls.trt.denosumab.hip.female",
                 "fls.trt.zoledronate.hip.female",
                 "fls.trt.teriparatide.hip.female",
                 "fls.trt.abaloparatide.hip.female",
                 "fls.trt.romo.hip.female")

  # compute the sum(common denominator)
  sum_e <- df_sheet |>
    dplyr::filter(name %in% b157_b161) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()



  # The actual change conditional on the sum
  if (sum_e == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b152_b156), 0),
        Source = replace(Source, which(name %in% b152_b156), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b157_b161[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b157_b161[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b152_b156), 0),
        Source = replace(Source, which(name %in% b152_b156), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b157_b161, (Value/sum_e), Value)
      )
  }



  # scenario set F
  # rows b174 to b178 gets a 0
  b174_b178 <- c("fls.trt.alendronate.other.female",
                 "fls.trt.risedronate.other.female",
                 "fls.trt.strontium.other.female",
                 "fls.trt.ibandronate.other.female",
                 "fls.trt.raloxifene.other.female")


  # rows containing common sum
  b179_b183 <- c("fls.trt.denosumab.other.female",
                 "fls.trt.zoledronate.other.female",
                 "fls.trt.teriparatide.other.female",
                 "fls.trt.abaloparatide.other.female",
                 "fls.trt.romo.other.female")

  # compute the sum(common denominator)
  sum_f <- df_sheet |>
    dplyr::filter(name %in% b179_b183) |>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()



  # The actual change conditional on the sum
  if (sum_f == 0){
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b174_b178), 0),
        Source = replace(Source, which(name %in% b174_b178), "Not injectable, hence = 0"),
        Value = replace(Value, which(name == b179_b183[1]), 1), # denosumab gets a 1
        Value = replace(Value, which(name %in% b179_b183[-1]), 0) # the rest get a 0
      )

  } else {
    df_sheet <- df_sheet |>
      dplyr::mutate(dplyr::across(Value, as.numeric)) |>
      dplyr::mutate(
        Value = replace(Value, which(name %in% b174_b178), 0),
        Source = replace(Source, which(name %in% b174_b178), "Not injectable, hence = 0"),
        Value = dplyr::if_else(name %in% b179_b183, (Value/sum_f), Value)
      )
  }

  # scenario set G:
  # check the value of b201

  b201 <- df_sheet |>
    dplyr::filter(name == "romo.to.nothing.fls.male")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b202_b206 <- c("romo.to.alendronate.fls.male",
                 "romo.to.risedronate.fls.male",
                 "romo.to.strontium.fls.male",
                 "romo.to.ibandronate.fls.male",
                 "romo.to.raloxifene.fls.male")

  # rows that contain common sum
  b207_b208 <- c("romo.to.denosumab.fls.male",
                 "romo.to.zoledronate.fls.male")

  # conditional on b201
  if (b201 < 1){

    # compute common denominator
    sum_g <- df_sheet |>
      dplyr::filter(name %in% b207_b208) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_g == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b202_b206), 0),
          Source = replace(Source, which(name %in% b202_b206), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b207_b208[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b207_b208[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b202_b206), 0),
          Source = replace(Source, which(name %in% b202_b206), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b207_b208, (Value/sum_g), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }


  # scenario set H:
  # check the value of b209

  b209 <- df_sheet |>
    dplyr::filter(name == "romo.to.nothing.fls.female")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b210_b214 <- c("romo.to.alendronate.fls.female",
                 "romo.to.risedronate.fls.female",
                 "romo.to.strontium.fls.female",
                 "romo.to.ibandronate.fls.female",
                 "romo.to.raloxifene.fls.female")

  # rows that contain common sum
  b215_b216 <- c("romo.to.denosumab.fls.female",
                 "romo.to.zoledronate.fls.female")

  # conditional on b209
  if (b209 < 1){

    # compute common denominator
    sum_h <- df_sheet |>
      dplyr::filter(name %in% b215_b216) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_h == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b210_b214), 0),
          Source = replace(Source, which(name %in% b210_b214), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b215_b216[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b215_b216[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b210_b214), 0),
          Source = replace(Source, which(name %in% b210_b214), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b215_b216, (Value/sum_h), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }


  # scenario set I:
  # check the value of b234

  b234 <- df_sheet |>
    dplyr::filter(name == "abaloparatide.to.nothing.fls.male")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b235_b239 <- c("abaloparatide.to.alendronate.fls.male",
                 "abaloparatide.to.risedronate.fls.male",
                 "abaloparatide.to.strontium.fls.male",
                 "abaloparatide.to.ibandronate.fls.male",
                 "abaloparatide.to.raloxifene.fls.male")

  # rows that contain common sum
  b240_b241 <- c("abaloparatide.to.denosumab.fls.male",
                 "abaloparatide.to.zoledronate.fls.male")

  # conditional on b234
  if (b234 < 1){

    # compute common denominator
    sum_i <- df_sheet |>
      dplyr::filter(name %in% b240_b241) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_i == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b235_b239), 0),
          Source = replace(Source, which(name %in% b235_b239), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b240_b241[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b240_b241[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b235_b239), 0),
          Source = replace(Source, which(name %in% b235_b239), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b240_b241, (Value/sum_i), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }



  # scenario set J:
  # check the value of b242

  b242 <- df_sheet |>
    dplyr::filter(name == "abaloparatide.to.nothing.fls.female")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b243_b247 <- c("abaloparatide.to.alendronate.fls.female",
                 "abaloparatide.to.risedronate.fls.female",
                 "abaloparatide.to.strontium.fls.female",
                 "abaloparatide.to.ibandronate.fls.female",
                 "abaloparatide.to.raloxifene.fls.female")

  # rows that contain common sum
  b248_b249 <- c("abaloparatide.to.denosumab.fls.female",
                 "abaloparatide.to.zoledronate.fls.female")

  # conditional on b242
  if (b242 < 1){

    # compute common denominator
    sum_j <- df_sheet |>
      dplyr::filter(name %in% b248_b249) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_j == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b243_b247), 0),
          Source = replace(Source, which(name %in% b243_b247), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b248_b249[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b248_b249[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b243_b247), 0),
          Source = replace(Source, which(name %in% b243_b247), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b248_b249, (Value/sum_j), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }

  # scenario set K:
  # check the value of b267

  b267 <- df_sheet |>
    dplyr::filter(name == "teriparatide.to.nothing.fls.male")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b268_b272 <- c("teriparatide.to.alendronate.fls.male",
                 "teriparatide.to.risedronate.fls.male",
                 "teriparatide.to.strontium.fls.male",
                 "teriparatide.to.ibandronate.fls.male",
                 "teriparatide.to.raloxifene.fls.male")

  # rows that contain common sum
  b273_b274 <- c("teriparatide.to.denosumab.fls.male",
                 "teriparatide.to.zoledronate.fls.male")

  # conditional on b267
  if (b267 < 1){

    # compute common denominator
    sum_k <- df_sheet |>
      dplyr::filter(name %in% b273_b274) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_k == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b268_b272), 0),
          Source = replace(Source, which(name %in% b268_b272), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b273_b274[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b273_b274[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b268_b272), 0),
          Source = replace(Source, which(name %in% b268_b272), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b273_b274, (Value/sum_k), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }

  # scenario set L:
  # check the value of b275

  b275 <- df_sheet |>
    dplyr::filter(name == "teriparatide.to.nothing.fls.female")|>
    dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric()

  # rows that get replaced with zero
  b276_b280 <- c("teriparatide.to.alendronate.fls.female",
                 "teriparatide.to.risedronate.fls.female",
                 "teriparatide.to.strontium.fls.female",
                 "teriparatide.to.ibandronate.fls.female",
                 "teriparatide.to.raloxifene.fls.female")

  # rows that contain common sum
  b281_b28 <- c("teriparatide.to.denosumab.fls.female",
                "teriparatide.to.zoledronate.fls.female")

  # conditional on b275
  if (b275 < 1){

    # compute common denominator
    sum_l <- df_sheet |>
      dplyr::filter(name %in% b281_b28) |>
      dplyr::select(Value) |> dplyr::pull(Value) |> as.numeric() |> sum()

    # The actual change conditional on the sum
    if (sum_l == 0){
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b276_b280), 0),
          Source = replace(Source, which(name %in% b276_b280), "Not injectable, hence = 0"),
          Value = replace(Value, which(name == b281_b28[1]), 1), # denosumab gets a 1
          Value = replace(Value, which(name %in% b281_b28[-1]), 0) # the rest get a 0
        )

    } else {
      df_sheet <- df_sheet |>
        dplyr::mutate(dplyr::across(Value, as.numeric)) |>
        dplyr::mutate(
          Value = replace(Value, which(name %in% b276_b280), 0),
          Source = replace(Source, which(name %in% b276_b280), "Not injectable, hence = 0"),
          Value = dplyr::if_else(name %in% b281_b28, (Value/sum_l), Value)
        )
    }

  } else {
    df_sheet <- df_sheet
  }

  # round values to 3 decimal place and convert back to character
  df_sheet <- df_sheet |>
    # mutate(across(Value, ~round(., 4))) |>
    dplyr::mutate(dplyr::across(Value, ~as.character(.)))


}
