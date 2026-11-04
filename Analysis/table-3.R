# Table 3 Concordance and correlations of calculated remnant cholesterol (Rc) values (N=8622)

library(psych)
library(tidyverse)
library(gtsummary)
library(flextable)


# ===============================================================================
# Load analysis dataset  ===============================
# ===============================================================================

## data is stored on secure drive
df_outcomes_notrig4 <- readRDS("Y:/Statistics/lipid new/LIPID-variability-LDL-RC/Data/df_outcomes_notrigs4.RDS")


### RC
df_outcomes_notrig4 %>%
  select(contains("remnant")) %>%
  ## categorise each into >1.8 or <1.8
  mutate(
    remnant_f_0_8 = ifelse(remnant_f_0 >= 0.80, "RC (f) 0.80mmol/L+", "<0.80mmol/L"),
    remnant_m_0_8 = ifelse(remnant_m_0 >= 0.80, "RC (m) 0.80mmol/L+", "<0.80mmol/L"),
    remnant_s_0_8 = ifelse(remnant_s_0 >= 0.80, "RC (s) 0.80mmol/L+", "<0.80mmol/L")
  ) %>%
  select(remnant_f_0_8, remnant_m_0_8, remnant_s_0_8) %>%
  tbl_summary(by = remnant_f_0_8, percent = "cell")

df_outcomes_notrig4 %>%
  select(contains("remnant")) %>%
  ## categorise each into >1.8 or <1.8
  mutate(
    remnant_f_0_8 = ifelse(remnant_f_0 >= 0.80, "RC (f) 0.80mmol/L+", "<0.80mmol/L"),
    remnant_m_0_8 = ifelse(remnant_m_0 >= 0.80, "RC (m) 0.80mmol/L+", "<0.80mmol/L"),
    remnant_s_0_8 = ifelse(remnant_s_0 >= 0.80, "RC (s) 0.80mmol/L+", "<0.80mmol/L")
  ) %>%
  select(remnant_s_0_8, remnant_m_0_8) %>%
  tbl_summary(by = remnant_s_0_8, percent = "cell")


df_outcomes_notrig4 %>%
  select(contains("remnant")) %>%
  ## categorise each into >1.8 or <1.8
  mutate(
    remnant_f_0_8 = ifelse(remnant_f_0 >= 0.80, 1, 0),
    remnant_m_0_8 = ifelse(remnant_m_0 >= 0.80, 1, 0),
    remnant_s_0_8 = ifelse(remnant_s_0 >= 0.80, 1, 0)
  ) %>%
  select(remnant_f_0_8, remnant_m_0_8, remnant_s_0_8) -> rc_agree_df


rc_f_m <- cohen.kappa(cbind(rc_agree_df$remnant_f_0_8, rc_agree_df$remnant_m_0_8))
rc_f_s <- cohen.kappa(cbind(rc_agree_df$remnant_f_0_8, rc_agree_df$remnant_s_0_8))
rc_m_s <- cohen.kappa(cbind(rc_agree_df$remnant_m_0_8, rc_agree_df$remnant_s_0_8))


# Kappa statistics of agreement (95% CI) are:

# RC (F) vs RC (M):
round(rc_f_m$confid[1, 2], 3)

# CI
round(rc_f_m$confid[1, 1], 3)
round(rc_f_m$confid[1, 3], 3)

#   RC (F) vs RC (S):
round(rc_f_s$confid[1, 2], 3)

round(rc_f_s$confid[1, 1], 3)
round(rc_f_s$confid[1, 3], 3)


#  RC (M) vs RC (S):

round(rc_m_s$confid[1, 2], 3)

round(rc_m_s$confid[1, 1], 3)
round(rc_m_s$confid[1, 3], 3)

## correlations
cor.test(df_outcomes_notrig4$remnant_f_0,
  df_outcomes_notrig4$remnant_m_0,
  method = "pearson"
)
cor.test(df_outcomes_notrig4$remnant_f_0,
  df_outcomes_notrig4$remnant_s_0,
  method = "pearson"
)
cor.test(df_outcomes_notrig4$remnant_m_0,
  df_outcomes_notrig4$remnant_s_0,
  method = "pearson"
)
