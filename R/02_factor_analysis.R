# ==============================================================================
# USJ COMMUTER STRESS STUDY — STEP 2: RELIABILITY & FACTOR ANALYSIS
# Script: 02_factor_analysis.R
# Group: 7 (STA 330)
# ==============================================================================

# Load required packages
library(dplyr)
library(psych)

# ------------------------------------------------------------------------------
# 1. LOAD CLEANED DATASET
# ------------------------------------------------------------------------------
if (!file.exists("cleaned_commuter_data.rds")) {
  stop("ERROR: 'cleaned_commuter_data.rds' not found. Please run 01_data_cleaning.R first.")
}

cleaned_df <- readRDS("cleaned_commuter_data.rds")
cat("Loaded cleaned dataset. Sample size N =", nrow(cleaned_df), "\n\n")

# Extract the 3 recoded STS items (1 to 7 positive stress scale)
sts_items <- cleaned_df %>%
  select(C1_Stressed, C2_Worried, C3_Hurried)

# ------------------------------------------------------------------------------
# 2. SCALE RELIABILITY ANALYSIS (Cronbach's Alpha)
# ------------------------------------------------------------------------------
cat("--- 1. SCALE RELIABILITY (Cronbach's Alpha) ---\n")
alpha_result <- psych::alpha(sts_items)

cat("Cronbach's Raw Alpha (α) =", round(alpha_result$total$raw_alpha, 3), "\n")
if (alpha_result$total$raw_alpha >= 0.70) {
  cat("-> Pass: High scale internal consistency (α >= 0.70)\n\n")
} else {
  cat("-> Warning: Internal consistency below target threshold (α < 0.70)\n\n")
}

# ------------------------------------------------------------------------------
# 3. SAMPLING ADEQUACY TESTS (KMO Index & Bartlett's Test)
# ------------------------------------------------------------------------------
cat("--- 2. SAMPLING ADEQUACY FOR FACTOR ANALYSIS ---\n")

# KMO Test
kmo_result <- psych::KMO(sts_items)
cat("Kaiser-Meyer-Olkin (KMO) Index =", round(kmo_result$MSA, 3), "\n")
if (kmo_result$MSA >= 0.60) {
  cat("-> KMO Pass: Data is suitable for factor analysis (KMO >= 0.60)\n")
} else {
  cat("-> KMO Fail: Inadequate common variance (KMO < 0.60)\n")
}

# Bartlett's Test of Sphericity
bartlett_result <- psych::cortest.bartlett(sts_items, n = nrow(cleaned_df))
cat("Bartlett's Test p-value =", format.pval(bartlett_result$p.value, digits = 4), "\n")
if (bartlett_result$p.value < 0.05) {
  cat("-> Bartlett Pass: Inter-item correlations are statistically significant (p < 0.05)\n\n")
} else {
  cat("-> Bartlett Fail: Items are completely uncorrelated (p >= 0.05)\n\n")
}

# ------------------------------------------------------------------------------
# 4. EXPLORATORY FACTOR ANALYSIS (Single-Factor Solution)
# ------------------------------------------------------------------------------
cat("--- 3. EXPLORATORY FACTOR ANALYSIS (1-Factor EFA) ---\n")
efa_result <- psych::fa(sts_items, nfactors = 1, rotate = "none", fm = "pa")

# Factor Loadings
cat("Item Factor Loadings on 'Commuter Stress':\n")
print(round(efa_result$loadings[], 3))

# Variance Explained
prop_var <- round(efa_result$Vaccounted["Proportion Var", 1] * 100, 2)
cat("\nProportion of Variance Explained by Primary Factor =", prop_var, "%\n")

cat("\n==============================================================================\n")
cat("SUCCESS: Factor analysis complete.\n")
cat("==============================================================================\n")

