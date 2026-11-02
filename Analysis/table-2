# Table 2: Paired t-tests for LDL-C and remnant cholesterol measures
# ===============================================================================
# Compares lipid measures between screening and baseline visits, and between
# calculation methods at baseline using paired t-tests.
#
#  Kristy Robledo
#  Finalised to github 14 OCT 2026
#
# Outputs:
#   - Screening vs baseline paired t-tests for LDL-C and remnant cholesterol
#   - Baseline paired comparisons among LDL-C calculation methods
#   - Baseline paired comparisons among remnant cholesterol calculation methods
#   - Summary statistics returned as a list of model objects and tidy results
#
# Data source:
#   Y:/Statistics/lipid new/LIPID-variability-LDL-RC/Data/df_allchol_notrigs4.RDS
# Note: this analysis dataset excludes patients with triglycerides > 400 mg/dL,
# consistent with the Friedewald equation restriction for LDL-C estimation.
# ===============================================================================

## create function to run paired ttests for screening vs baseline and 
# baseline comparisons of ldl and remnant cholesterol measures

library(tidyverse)
library(gtsummary)
library(flextable)

ttest_screening<-function(df) {


  df |>
    dplyr::select(pid, visit, ldl, ldl_s, ldl_m, remnant_f, remnant_s, remnant_m) |>
    tidyr::pivot_longer(cols = c(ldl, ldl_s, ldl_m, remnant_f, remnant_s, remnant_m),
                        names_to = "var",
                        values_to = "value") |>
    tidyr::pivot_wider(names_from = visit, values_from = value) ->df_long

  ##ttest by var (screening vs baseline)

  df_long |>
    dplyr::nest_by(var,
            .key = "nested_data") |>
    dplyr::mutate(ttest.m = list(t.test(x=nested_data$screening, y=nested_data$baseline,
                                        paired=TRUE))) ->outdf


  outdf |>
    dplyr::reframe(broom::tidy(ttest.m)) ->screen_ttest

  ##baseline tests
  df |>
    dplyr::filter(visit=="baseline") |>
    dplyr::select(pid, ldl, ldl_s, ldl_m, remnant_f, remnant_s, remnant_m) ->df_paired


  list(ttest.mod = t.test(x=df_paired$ldl, y=df_paired$ldl_s,
                                        paired=TRUE),
                               t.test(x=df_paired$ldl, y=df_paired$ldl_m,
                                        paired=TRUE),
                               t.test(x=df_paired$ldl_s, y=df_paired$ldl_m,
                                      paired=TRUE),
                               t.test(x=df_paired$remnant_f, y=df_paired$remnant_s,
                                      paired=TRUE),
                               t.test(x=df_paired$remnant_f, y=df_paired$remnant_m,
                                      paired=TRUE),
                               t.test(x=df_paired$remnant_s, y=df_paired$remnant_m,
                                      paired=TRUE)) ->outdf_paired

  tibble::tibble(
    name = c("ldl vs ldl_s", "ldl vs ldl_m", "ldl_s vs ldl_m",
             "remnant_f vs remnant_s", "remnant_f vs remnant_m", "remnant_s vs remnant_m"),
    mod = c(outdf_paired[1], outdf_paired[2], outdf_paired[3],
            outdf_paired[4], outdf_paired[5], outdf_paired[6] )
  ) ->testdf


  return(list(scr.base.mods=screen_ttest,
              list.comp.mods= outdf_paired,
              base.comp.mods = testdf))
}


# ===============================================================================
# Load analysis dataset  ===============================
# ===============================================================================

## data is stored on secure drive
df_allchol_notrigs4 <- readRDS("Y:/Statistics/lipid new/LIPID-variability-LDL-RC/Data/df_allchol_notrigs4.RDS")

all_ttests <- ttest_screening(df_allchol_notrigs4) # nolint

all_ttests[[2]]
