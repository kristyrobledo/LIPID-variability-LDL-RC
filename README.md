# LIPID trial: Variability of LDL and RC

Code accompanying the manuscript:

> **Variability of Calculated Low Density Lipoprotein Cholesterol and Remnant Cholesterol Concentrations in the LIPID Study**

## Overview

This repository contains the analysis code used to investigate variability in calculated low-density lipoprotein cholesterol (LDL-C) and remnant cholesterol (RC) concentrations within the LIPID study.

The project evaluates:

- Variability of LDL-C estimates derived using multiple calculation methods
- Variability of remnant cholesterol estimates derived using multiple calculation methods
- Differences between methods across triglyceride levels
- Measurement error and coefficient of variation (CV) for lipid biomarkers and derived lipid measures
- Summary tables and graphical outputs used in the manuscript and supplementary materials

## Repository Structure

```text
.
├── Analysis/          # Analysis scripts 
├── Data/              # Input data (not publicly distributed)
├── Output/            # Manuscript tables and figures
└── README.md          # readme
```

## Requirements

Analyses were conducted in R using the following key packages:

```r
tidyverse
ggplot2
officer
rvg
gtsummary
gamlss
```

Install required packages using:

```r
install.packages(
  c(
    "tidyverse",
    "officer",
    "rvg",
    "forcats"
  )
)
```

## Reproducibility

Each script in the `analysis/` directory is designed to be run sequentially. 
The scripts are well-documented with comments explaining each step of the analysis.

## Data Availability

The original LIPID study data are not publicly available and are therefore not included in this repository.

All code required to reproduce the analyses and figures is provided. 
Researchers interested in accessing LIPID study data should follow the appropriate data governance and approval processes detailed in the manuscript.

## Outputs

The repository generates:

- Summary tables for lipid measures
- Boxplots of LDL-C and remnant cholesterol measures across triglyceride quartiles
- Comparisons between calculation methods
- Measurement error visualisations based on coefficients of variation
- PowerPoint files containing publication-quality figures

## Citation

If you use this code, please cite:

> Variability of Calculated Low Density Lipoprotein Cholesterol and Remnant Cholesterol Concentrations in the LIPID Study.

## Author of code

Developed by Kristy Robledo 
