#' Preparing FLS_Adherence_100 model inputs
#'
#' @param df_sheet A data.frame, googlesheet equivalent of base case
#'
#' @return A data.frame
#' @export
#'
#' @examples
FLS_Adherence_100 <- function(df_sheet){
  # set 1 vars
  # replace value by 1 and source by : "Assuming perfect adherence during first 4 months"
  rows_389_448 <- c(
    "primary.adh_alendronate.spine.fls.male",
    "primary.adh_risedronate.spine.fls.male",
    "primary.adh_strontium.spine.fls.male",
    "primary.adh_ibandronate.spine.fls.male",
    "primary.adh_raloxifene.spine.fls.male",
    "primary.adh_denosumab.spine.fls.male",
    "primary.adh_zoledronate.spine.fls.male",
    "primary.adh_teriparatide.spine.fls.male",
    "primary.adh_abaloparatide.spine.fls.male",
    "primary.adh_romo.spine.fls.male",
    "primary.adh_alendronate.hip.fls.male",
    "primary.adh_risedronate.hip.fls.male",
    "primary.adh_strontium.hip.fls.male",
    "primary.adh_ibandronate.hip.fls.male",
    "primary.adh_raloxifene.hip.fls.male",
    "primary.adh_denosumab.hip.fls.male",
    "primary.adh_zoledronate.hip.fls.male",
    "primary.adh_teriparatide.hip.fls.male",
    "primary.adh_abaloparatide.hip.fls.male",
    "primary.adh_romo.hip.fls.male",
    "primary.adh_alendronate.other.fls.male",
    "primary.adh_risedronate.other.fls.male",
    "primary.adh_strontium.other.fls.male",
    "primary.adh_ibandronate.other.fls.male",
    "primary.adh_raloxifene.other.fls.male",
    "primary.adh_denosumab.other.fls.male",
    "primary.adh_zoledronate.other.fls.male",
    "primary.adh_teriparatide.other.fls.male",
    "primary.adh_abaloparatide.other.fls.male",
    "primary.adh_romo.other.fls.male",
    "primary.adh_alendronate.spine.fls.female",
    "primary.adh_risedronate.spine.fls.female",
    "primary.adh_strontium.spine.fls.female",
    "primary.adh_ibandronate.spine.fls.female",
    "primary.adh_raloxifene.spine.fls.female",
    "primary.adh_denosumab.spine.fls.female",
    "primary.adh_zoledronate.spine.fls.female",
    "primary.adh_teriparatide.spine.fls.female",
    "primary.adh_abaloparatide.spine.fls.female",
    "primary.adh_romo.spine.fls.female",
    "primary.adh_alendronate.hip.fls.female",
    "primary.adh_risedronate.hip.fls.female",
    "primary.adh_strontium.hip.fls.female",
    "primary.adh_ibandronate.hip.fls.female",
    "primary.adh_raloxifene.hip.fls.female",
    "primary.adh_denosumab.hip.fls.female",
    "primary.adh_zoledronate.hip.fls.female",
    "primary.adh_teriparatide.hip.fls.female",
    "primary.adh_abaloparatide.hip.fls.female",
    "primary.adh_romo.hip.fls.female",
    "primary.adh_alendronate.other.fls.female",
    "primary.adh_risedronate.other.fls.female",
    "primary.adh_strontium.other.fls.female",
    "primary.adh_ibandronate.other.fls.female",
    "primary.adh_raloxifene.other.fls.female",
    "primary.adh_denosumab.other.fls.female",
    "primary.adh_zoledronate.other.fls.female",
    "primary.adh_teriparatide.other.fls.female",
    "primary.adh_abaloparatide.other.fls.female",
    "primary.adh_romo.other.fls.female")

  # set 2 vars
  # value replaced by 1 and source by : "Assuming perfect adherence between months 5 and 12"
  rows_513_550 <- c("monitored.4m_adh_alendronate.male",
                    "monitored.4m_adh_risedronate.male",
                    "monitored.4m_adh_strontium.male",
                    "monitored.4m_adh_ibandronate.male",
                    "monitored.4m_adh_raloxifene.male",
                    "monitored.4m_adh_denosumab.male",
                    "monitored.4m_adh_zoledronate.male",
                    "monitored.4m_adh_teriparatide.male",
                    "monitored.4m_adh_abaloparatide.male",
                    "monitored.4m_adh_romo.male",
                    "monitored.12m_adh_alendronate.male",
                    "monitored.12m_adh_risedronate.male",
                    "monitored.12m_adh_strontium.male",
                    "monitored.12m_adh_ibandronate.male",
                    "monitored.12m_adh_raloxifene.male",
                    "monitored.12m_adh_denosumab.male",
                    "monitored.12m_adh_zoledronate.male",
                    "monitored.12m_adh_teriparatide.male",
                    "monitored.12m_adh_abaloparatide.male",
                    "monitored.4m_adh_alendronate.female",
                    "monitored.4m_adh_risedronate.female",
                    "monitored.4m_adh_strontium.female",
                    "monitored.4m_adh_ibandronate.female",
                    "monitored.4m_adh_raloxifene.female",
                    "monitored.4m_adh_denosumab.female",
                    "monitored.4m_adh_zoledronate.female",
                    "monitored.4m_adh_teriparatide.female",
                    "monitored.4m_adh_abaloparatide.female",
                    "monitored.4m_adh_romo.female",
                    "monitored.12m_adh_alendronate.female",
                    "monitored.12m_adh_risedronate.female",
                    "monitored.12m_adh_strontium.female",
                    "monitored.12m_adh_ibandronate.female",
                    "monitored.12m_adh_raloxifene.female",
                    "monitored.12m_adh_denosumab.female",
                    "monitored.12m_adh_zoledronate.female",
                    "monitored.12m_adh_teriparatide.female",
                    "monitored.12m_adh_abaloparatide.female")
  # set3 vars
  # Value replaced by 0 and source:"Assuming perfect adherence from month 25 to 60"
  rows_567_580 <- c("adh_annual_decline_alendronate.fls.male",
                    "adh_annual_decline_risedronate.fls.male",
                    "adh_annual_decline_strontium.fls.male",
                    "adh_annual_decline_ibandronate.fls.male",
                    "adh_annual_decline_raloxifene.fls.male",
                    "adh_annual_decline_denosumab.fls.male",
                    "adh_annual_decline_zoledronate.fls.male",
                    "adh_annual_decline_alendronate.fls.female",
                    "adh_annual_decline_risedronate.fls.female",
                    "adh_annual_decline_strontium.fls.female",
                    "adh_annual_decline_ibandronate.fls.female",
                    "adh_annual_decline_raloxifene.fls.female",
                    "adh_annual_decline_denosumab.fls.female",
                    "adh_annual_decline_zoledronate.fls.female")

  # mutate and replace

  # mutate and replace
  df_sheet |>
    dplyr::mutate(
      # replace value set 1
      Value = replace(Value, which(name %in% rows_389_448), 1),

      # replace value set 2
      Value = replace(Value, which(name %in% rows_513_550), 1),

      # replace value set 3
      Value = replace(Value, which(name %in% rows_567_580), 0),

      # replace source set 1
      Source = replace(Source, which(name %in% rows_389_448),
                       "Assuming perfect adherence during first 4 months"),
      # replace source set 2
      Source = replace(Source, which(name %in% rows_513_550),
                       "Assuming perfect adherence between months 5 and 24"),
      # replace source set 3
      Source = replace(Source, which(name %in% rows_567_580),
                       "Assuming perfect adherence from month 25 to 60")
    )

}
