# ==============================================================================
# USJ COMMUTER STRESS STUDY — CONFIGURATION & ENVIRONMENT
# Script: scripts/00_config.R
# Group Standardized Configuration
# ==============================================================================

# 1. Load Required Libraries
suppressPackageSuppressMessages <- function() {
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(psych)
  library(MASS)
  library(writexl)
}
suppressPackageSuppressMessages()

# 2. Reproducibility Seed & STS Score Cutoffs (Ettema et al., 2011)
SEED_VALUE  <- 42
set.seed(SEED_VALUE)

CUTOFF_LOW  <- -1.0  # stress_score < -1.0 -> Low
CUTOFF_HIGH <-  1.0  # stress_score >  1.0 -> High

# 3. Create Required Relative Directories
dir.create("data/clean",      recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

# 4. Global Graphic Theme (Times New Roman 12pt, White Background)
theme_report <- function() {
  theme_classic(base_size = 12, base_family = "Times New Roman") +
    theme(
      text              = element_text(family = "Times New Roman", size = 12, color = "black"),
      axis.text         = element_text(family = "Times New Roman", size = 12, color = "black"),
      axis.title        = element_text(family = "Times New Roman", size = 12, face = "bold", color = "black"),
      legend.text       = element_text(family = "Times New Roman", size = 12, color = "black"),
      legend.title      = element_text(family = "Times New Roman", size = 12, face = "bold", color = "black"),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      plot.title        = element_blank(),
      plot.subtitle     = element_blank()
    )
}

# 5. Color Palettes (Aligned with group's stress_level factor)
blue_grey_palette <- c("Low" = "#90A4AE", "Moderate" = "#455A64", "High" = "#1A365D")
single_bar_color  <- "#2B4C7E"

# 6. Data Loader Function (Loads "Data (labels)" sheet)
load_raw_commuter_data <- function() {
  file_path <- "data/raw/commuter-survey-complete-2026-11-22.xlsx"
  if (!file.exists(file_path)) {
    stop("Data file not found at: ", file_path)
  }
  read_excel(file_path, sheet = "Data (labels)")
}
