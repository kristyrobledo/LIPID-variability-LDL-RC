#===============================================================================
# Additional graphics for central illustration ==================================================================
#
#  Kristy Robledo
#  22 SEPT 2026
#
# Outputs:
#   - LDL-C measures by triglyceride quartile
#   - Remnant cholesterol measures by triglyceride quartile
#   - non-HDL-C and ApoB by triglyceride quartile
#   - Differences between LDL-C calculation methods
#   - Differences between remnant cholesterol calculation methods
#   - Measurement error (CV%) summary plot
#   - PowerPoint containing all figures
#
# Data source:
#   analysis/_targets_gr4excluded
# Note that this dataset excludes all patients with triglycerides > 400 mg/dL,
# which is the threshold for LDL-C estimation by the Friedewald equation.
#===============================================================================

# prepare
library(tidyverse)
library(officer)
library(rvg)

#===============================================================================
# Load analysis dataset  ===============================
#===============================================================================

df_outcomes_notrig4 <- readRDS("Data/df_outcomes_notrigs4.RDS")

#===============================================================================
# Create plotting dataset ======================================================
#
# Triglycerides are split into quartiles and additional variables describing
# differences between LDL-C and remnant cholesterol estimation methods are
# derived.
#===============================================================================

df_plot <- df_outcomes_notrig4 |>
  mutate(
    trig_quart = ntile(trig0q, 4)
  ) |>
  select(
    trig_quart,
    trig0q,
    ldl0q,
    ldl_s_0,
    ldl_m_0,
    remnant_f_0,
    remnant_s_0,
    remnant_m_0,
    nonhdl_0,
    apob_0
  ) |>
  mutate(
    # LDL-C method differences
    ldl_diff_mf = ldl_m_0 - ldl0q,
    ldl_diff_sf = ldl_s_0 - ldl0q,
    ldl_diff_sm = ldl_s_0 - ldl_m_0,

    # Remnant cholesterol method differences
    rc_diff_mf = remnant_m_0 - remnant_f_0,
    rc_diff_sf = remnant_s_0 - remnant_f_0,
    rc_diff_sm = remnant_s_0 - remnant_m_0
  )

#===============================================================================
# Common theme
#===============================================================================

theme_study <- theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

#===============================================================================
# Figure 1 ==================================================================
# LDL-C estimates by triglyceride quartile
#===============================================================================

p1 <- df_plot |>
  pivot_longer(
    cols = c(ldl0q, ldl_s_0, ldl_m_0),
    names_to = "ldl_method",
    values_to = "ldl"
  ) |>
  ggplot(
    aes(
      x = factor(trig_quart),
      y = ldl,
      fill = factor(trig_quart)
    )
  ) +
  geom_boxplot() +
  facet_wrap(~ldl_method, scales = "free_y") +
  labs(
    x = "Triglyceride quartile",
    y = "LDL-C",
    fill = "TG quartile",
    title = "LDL-C measures across triglyceride quartiles"
  ) +
  theme_study

#===============================================================================
# Figure 2 ==================================================================
# non-HDL-C and ApoB by triglyceride quartile
#===============================================================================

p2 <- df_plot |>
  pivot_longer(
    cols = c(nonhdl_0, apob_0),
    names_to = "parameter",
    values_to = "value"
  ) |>
  ggplot(
    aes(
      x = factor(trig_quart),
      y = value,
      fill = factor(trig_quart)
    )
  ) +
  geom_boxplot() +
  facet_wrap(~parameter, scales = "free_y") +
  labs(
    x = "Triglyceride quartile",
    y = NULL
  ) +
  theme_study

#===============================================================================
# Figure 3 ==================================================================
# Remnant cholesterol estimates by triglyceride quartile
#===============================================================================

p3 <- df_plot |>
  pivot_longer(
    cols = c(remnant_f_0, remnant_s_0, remnant_m_0),
    names_to = "method",
    values_to = "rc"
  ) |>
  ggplot(
    aes(
      x = factor(trig_quart),
      y = rc,
      fill = factor(trig_quart)
    )
  ) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 2)) +
  facet_wrap(~method) +
  labs(
    x = "Triglyceride quartile",
    y = "Remnant cholesterol"
  ) +
  theme_study

#===============================================================================
# Figure 4 ==================================================================
# LDL-C method differences
#===============================================================================

p4 <- df_plot |>
  pivot_longer(
    cols = c(ldl_diff_mf, ldl_diff_sf, ldl_diff_sm),
    names_to = "comparison",
    values_to = "difference"
  ) |>
  ggplot(
    aes(
      x = factor(trig_quart),
      y = difference,
      fill = factor(trig_quart)
    )
  ) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  facet_wrap(~comparison) +
  labs(
    x = "Triglyceride quartile",
    y = "Difference"
  ) +
  theme_study

#===============================================================================
# Figure 5 ==================================================================
# Remnant cholesterol method differences
#===============================================================================

p5 <- df_plot |>
  pivot_longer(
    cols = c(rc_diff_mf, rc_diff_sf, rc_diff_sm),
    names_to = "comparison",
    values_to = "difference"
  ) |>
  ggplot(
    aes(
      x = factor(trig_quart),
      y = difference,
      fill = factor(trig_quart)
    )
  ) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  facet_wrap(~comparison) +
  labs(
    x = "Triglyceride quartile",
    y = "Difference"
  ) +
  theme_study

#===============================================================================
# Figure 6 ==================================================================
# Coefficient of variation (CV) from Supplementary Table 4
#
# Lower CV indicates lower measurement error.
#===============================================================================

cv_dat <- tribble(
  ~Parameter, ~CV,
  "HDLc", 0.71,
  "apoA1 g/L", 0.77,
  "apoB:apoA1", 0.81,
  "apoB g/L", 0.92,
  "Rc-MH", 1.73,
  "TC", 2.59,
  "LDLc-MH", 2.75,
  "non-HDLc", 2.84,
  "LDLc-S", 2.96,
  "Rc-MH:HDLc", 3.08,
  "LDLc-F", 3.12,
  "Rc-S", 3.30,
  "Rc-F", 3.49,
  "LDLc-MH:HDLc", 4.76,
  "LDLc-S:HDLc", 4.77,
  "TC:HDLc", 4.86,
  "LDL-F:HDLc", 4.92,
  "Rc-MH:HDLc (alt)", 5.35,
  "Rc-F:HDLc", 5.74,
  "TG", 7.68,
  "TG:HDLc", 12.64
)

p6 <- cv_dat |>
  mutate(Parameter = forcats::fct_reorder(Parameter, CV)) |>
  ggplot(
    aes(
      x = CV,
      y = Parameter
    )
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = CV,
      y = Parameter,
      yend = Parameter
    ),
    colour = "grey80"
  ) +
  geom_point(
    size = 3,
    colour = "steelblue"
  ) +
  labs(
    x = "Coefficient of variation (%)",
    y = NULL,
    title = "Measurement error by lipid parameter"
  ) +
  theme_minimal()

#===============================================================================
# prep plots for powerpoint (fully editable) ==========================================================
#===============================================================================

plot1 <- rvg::dml(code = print(p1, newpage = FALSE))
plot2 <- rvg::dml(code = print(p2, newpage = FALSE))
plot3 <- rvg::dml(code = print(p3, newpage = FALSE))
plot4 <- rvg::dml(code = print(p4, newpage = FALSE))
plot5 <- rvg::dml(code = print(p5, newpage = FALSE))
plot6 <- rvg::dml(code = print(p6, newpage = FALSE))


### create document + load in plots into slides
doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot1, location = ph_location(width = 8, height=6) )
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot2, location = ph_location(width = 8, height=6) )
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot3, location = ph_location(width = 8, height=6) )
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot4, location = ph_location(width = 8, height=6) )
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot5, location = ph_location(width = 8, height=6) )
doc <- add_slide(doc, layout = "Title and Content", master = "Office Theme")
doc <- ph_with(doc, plot6, location = ph_location(width = 8, height=6) )


print(doc, target = "Output/Graphics-powerpoint.pptx")
