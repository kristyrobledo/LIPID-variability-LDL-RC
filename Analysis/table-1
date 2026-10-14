# Table 1: Baseline clinical characteristics by randomised treatment (n=8623)
# ===============================================================================
# Produces baseline demographics summary table for the LIPID variability study.
#
#  Kristy Robledo
#  Finalised to github 14 OCT 2026
#
# Outputs:
#   - Baseline characteristics by treatment group
#   - Binary covariate summaries with labelled categories
#   - Age summary and treatment group comparisons
#
# Data source:
#   Y:/Statistics/lipid new/LIPID-variability-LDL-RC/Data/df_outcomes_notrigs4.RDS
# Note: this analysis dataset excludes patients with triglycerides > 400 mg/dL,
# consistent with the Friedewald equation restriction for LDL-C estimation.
# ===============================================================================

# prepare
library(tidyverse)
library(gtsummary)
library(flextable)

# ===============================================================================
# Load analysis dataset  ===============================
# ===============================================================================

## data is stored on secure drive
df_outcomes_notrig4 <- readRDS("Y:/Statistics/lipid new/LIPID-variability-LDL-RC/Data/df_outcomes_notrigs4.RDS")

# lipid_vars
lipid_vars <- c(
  "chol0q",
  "hdl0q",
  "trig0q",
  "apoa1_0",
  "apob_0",
  # "lpa_0",
  # "fglucose_0",
  # "wbc_0",
  # "crp_0",
  "ldl0q",
  "ldl_m_0",
  "ldl_s_0",
  "nonhdl_0",
  "remnant_f_0",
  "remnant_s_0",
  "remnant_m_0"
  # "chol_hdl_0",
  # "apob_apoa_0",
  # "ldl_hdl_f_0",
  # "ldl_hdl_s_0",
  # "ldl_hdl_m_0",
  # "trig_hdl_0",
  # "trig_gluc_0",
  # "rem_hdl_f_0",
  # "rem_hdl_m_0",
  # "rem_hdl_s_0"
)


df_outcomes_notrig4 |>
  dplyr::select(dplyr::all_of(lipid_vars), treat) |>
  gtsummary::tbl_summary(
    by = treat,
    type = list(
      gtsummary::everything() ~ "continuous"
    ),
    statistic = list(
      gtsummary::all_continuous() ~ "{mean} ({sd})",
      dplyr::matches("^remnant_") ~ "{median} ({p25}, {p75})"
    )
  ) |>
  gtsummary::add_overall()
