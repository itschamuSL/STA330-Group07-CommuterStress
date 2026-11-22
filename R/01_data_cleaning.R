# ==============================================================================
# USJ COMMUTER STRESS STUDY — STEP 1: DATA CLEANING & STRESS RECODING
# Script: 01_data_cleaning.R
# Group: 7 (STA 330)
# Data Source: Excel Workbook -> Sheet: 'Data (codes)'
# ==============================================================================

# Load required packages
library(readxl)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. LOAD DATASET DIRECTLY FROM EXCEL SHEET 'Data (codes)'
# ------------------------------------------------------------------------------
excel_path <- "commuter-survey-complete-2026-11-22.xlsx" # Update path if needed

raw_df <- read_excel(excel_path, sheet = "Data (codes)")
cat("Loaded 'Data (codes)' sheet. Initial N =", nrow(raw_df), "\n")

# ------------------------------------------------------------------------------
# 2. APPLY SCREENING FILTERS (Gates 1, 2, 3)
# ------------------------------------------------------------------------------
step1_df <- raw_df %>%
  filter(
    A1 == "yes",               # Gate 1: Consent confirmed
    A2 != "1-2",              # Gate 2: Commute frequency >= 3 days/week ('3-4' or '5+')
    `Duration (s)` >= 45      # Gate 3: Speeding filter (>= 45 seconds)
  )

cat("After Consent, Frequency & Speeding filters: N =", nrow(step1_df), "\n")

# ------------------------------------------------------------------------------
# 3. RECODE STRESS SCORE (STS C1-C3) & CREATE DEPENDENT VARIABLE (Y)
# ------------------------------------------------------------------------------
# Recode raw (-3 to +3) items to 1 to 7 scale:
# Formula: Recoded = 4 - Raw
#   +3 (Very Relaxed)  -> 4 - ( 3) = 1 (Low Stress)
#    0 (Neither)       -> 4 - ( 0) = 4 (Moderate Stress)
#   -3 (Very Stressed) -> 4 - (-3) = 7 (High Stress)

cleaned_df <- step1_df %>%
  mutate(
    C1_Stressed = 4 - as.numeric(C1),
    C2_Worried  = 4 - as.numeric(C2),
    C3_Hurried  = 4 - as.numeric(C3)
  ) %>%
  # Gate 4: Straight-lining filter (drop unmotivated responses with SD = 0)
  rowwise() %>%
  filter(sd(c(C1_Stressed, C2_Worried, C3_Hurried), na.rm = TRUE) > 0) %>%
  ungroup() %>%
  mutate(
    # Composite Sum Score (Range: 3 to 21)
    STS_Composite = C1_Stressed + C2_Worried + C3_Hurried,
    
    # Categorize Dependent Variable Y into Ordered Factors
    Stress_Level = case_when(
      STS_Composite <= 8  ~ "Low",
      STS_Composite <= 14 ~ "Moderate",
      TRUE                ~ "High"
    ),
    Stress_Level = factor(Stress_Level, levels = c("Low", "Moderate", "High"), ordered = TRUE)
  )

cat("After Straight-lining filter (Final Sample): N =", nrow(cleaned_df), "\n")

# ------------------------------------------------------------------------------
# 4. CONVERT PREDICTORS & CONTROLS USING NATIVE EXCEL CODES
# ------------------------------------------------------------------------------
cleaned_df <- cleaned_df %>%
  mutate(
    # Demographics (Native Codes)
    A3_Faculty   = factor(A3),
    A4_Year      = factor(A4, ordered = TRUE),
    A5_Gender    = factor(A5),
    A6_Resid     = factor(A6),
    
    # Transport Friction Predictors (Native Codes matching Codebook)
    B1_Distance  = factor(B1, ordered = TRUE), # Travel Distance
    B2_Mode      = factor(B2),                 # Main Transport Mode
    B3_Transfers = factor(B3, ordered = TRUE), # Number of Transfers
    B6_Seating   = factor(B6),                 # Seating Status
    B7_Delays    = factor(B7, ordered = TRUE), # Delay Frequency
    
    # Baseline Controls & Contextual Variables (Numeric Covariates)
    D1_Sleep     = as.numeric(D1),             # Sleep Quality Last Night
    D2_Workload  = as.numeric(D2),             # Academic Workload Today
    C4_GenStress = as.numeric(C4),             # Baseline Chronic Stress ("these days")
    D3_Crowding  = as.numeric(D3),             # Crowding Level
    D4_Weather   = as.numeric(D4)              # Weather Impact
  )

# ------------------------------------------------------------------------------
# 5. SAVE CLEANED DATASET
# ------------------------------------------------------------------------------
saveRDS(cleaned_df, "cleaned_commuter_data.rds")
write.csv(cleaned_df, "cleaned_commuter_data.csv", row.names = FALSE)

cat("\n==============================================================================\n")
cat("SUCCESS: Data cleaning and stress score recoding complete!\n")
cat("Summary of Recoded Stress Outcome (Stress_Level):\n")
print(table(cleaned_df$Stress_Level))
cat("==============================================================================\n")

## Critical Adjustment Before Regression: Collapsing Sparse Categories

library(dplyr)

# Load cleaned dataset
cleaned_df <- readRDS("cleaned_commuter_data.rds")

# Collapse sparse categories for statistical model stability
cleaned_df <- cleaned_df %>%
  mutate(
    # Collapse Distance 4 (30-49km) & 5 (50+km) into level 4 ("30+ km")
    B1_Distance_Collapsed = factor(ifelse(as.numeric(as.character(B1_Distance)) >= 4, 4, as.numeric(as.character(B1_Distance))), ordered = TRUE),
    
    # Collapse Transfers 2 & 3+ into level 2 ("2+ changes")
    B3_Transfers_Collapsed = factor(ifelse(as.numeric(as.character(B3_Transfers)) >= 2, 2, as.numeric(as.character(B3_Transfers))), ordered = TRUE),
    
    # Collapse Delays 2 & 3 into level 2 ("2+ days/wk")
    B7_Delays_Collapsed = factor(ifelse(as.numeric(as.character(B7_Delays)) >= 2, 2, as.numeric(as.character(B7_Delays))), ordered = TRUE)
  )
getwd()

# Re-save cleaned dataset
saveRDS(cleaned_df, "cleaned_commuter_data.rds")
write.csv(cleaned_df, "cleaned_commuter_data.csv", row.names = FALSE)
cat("Sparse categories successfully collapsed and saved to cleaned_commuter_data.rds!\n")
