# ==============================================================================
# USJ COMMUTER STRESS STUDY — EXPLORATORY DATA ANALYSIS (EDA)
# Script: 02b_eda_summary.R
# Group: 7 (STA 330)
# ==============================================================================

library(dplyr)

# ------------------------------------------------------------------------------
# 1. LOAD CLEANED DATASET
# ------------------------------------------------------------------------------
if (!file.exists("cleaned_commuter_data.rds")) {
  stop("ERROR: 'cleaned_commuter_data.rds' not found. Run 01_data_cleaning.R first.")
}

cleaned_df <- readRDS("cleaned_commuter_data.rds")
cat("Cleaned Dataset Loaded successfully. N =", nrow(cleaned_df), "\n\n")

# ------------------------------------------------------------------------------
# 2. UNIVARIATE SUMMARY: OUTCOME VARIABLE (Y_Stress)
# ------------------------------------------------------------------------------
cat("==============================================================================\n")
cat("1. TARGET OUTCOME DISTRIBUTION (Y_Stress)\n")
cat("==============================================================================\n")

y_counts <- table(cleaned_df$Y_Stress)
y_props  <- round(prop.table(y_counts) * 100, 2)
y_summary <- data.frame(
  Stress_Category = names(y_counts),
  Count = as.numeric(y_counts),
  Percentage = paste0(y_props, "%")
)
print(y_summary, row.names = FALSE)
cat("\n")

# ------------------------------------------------------------------------------
# 3. UNIVARIATE SUMMARY: FRICTION PREDICTORS (H1 - H5)
# ------------------------------------------------------------------------------
cat("==============================================================================\n")
cat("2. TRANSPORT FRICTION PREDICTORS PROFILE\n")
cat("==============================================================================\n")

predictors <- c("B2_Mode", "B1_Distance", "B3_Transfers", "B7_Delays", "B6_Seating")

for (var in predictors) {
  cat("\n--- Distribution of:", var, "---\n")
  counts <- table(cleaned_df[[var]])
  props  <- round(prop.table(counts) * 100, 1)
  df_summary <- data.frame(Category = names(counts), Count = as.numeric(counts), Percent = paste0(props, "%"))
  print(df_summary, row.names = FALSE)
}

# ------------------------------------------------------------------------------
# 4. CROSS-TABULATIONS (SPARSITY CHECK & BIVARIATE PATTERNS)
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("3. CROSS-TABULATIONS (Predictors vs Outcome Y_Stress)\n")
cat("   * Look for 0 or very small counts (< 5) that could affect polr()\n")
cat("==============================================================================\n")

cat("\n[H1] Transport Mode vs Stress Level:\n")
print(table(Transport_Mode = cleaned_df$B2_Mode, Stress = cleaned_df$Y_Stress))

cat("\n[H2] Travel Distance vs Stress Level:\n")
print(table(Distance = cleaned_df$B1_Distance, Stress = cleaned_df$Y_Stress))

cat("\n[H3] Vehicle Transfers vs Stress Level:\n")
print(table(Transfers = cleaned_df$B3_Transfers, Stress = cleaned_df$Y_Stress))

cat("\n[H4] Delay Frequency vs Stress Level:\n")
print(table(Delays = cleaned_df$B7_Delays, Stress = cleaned_df$Y_Stress))

cat("\n[H5] Seating Status vs Stress Level:\n")
print(table(Seating = cleaned_df$B6_Seating, Stress = cleaned_df$Y_Stress))

# ------------------------------------------------------------------------------
# 5. CONTINUOUS CONFOUNDERS SUMMARY (Z1, Z2, Z3)
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("4. BASELINE CONFOUNDERS SUMMARY STATISTICS\n")
cat("==============================================================================\n")

confounder_summary <- cleaned_df %>%
  summarise(
    Z1_Sleep_Mean     = round(mean(D1_Sleep, na.rm = TRUE), 2),
    Z1_Sleep_SD       = round(sd(D1_Sleep, na.rm = TRUE), 2),
    Z2_Workload_Mean  = round(mean(D2_Workload, na.rm = TRUE), 2),
    Z2_Workload_SD    = round(sd(D2_Workload, na.rm = TRUE), 2),
    Z3_GenStress_Mean = round(mean(C4_GenStress, na.rm = TRUE), 2),
    Z3_GenStress_SD   = round(sd(C4_GenStress, na.rm = TRUE), 2)
  )

print(confounder_summary)

cat("\n==============================================================================\n")
cat("EDA Complete! Review cross-tabulations above before fitting polr().\n")
cat("==============================================================================\n")

