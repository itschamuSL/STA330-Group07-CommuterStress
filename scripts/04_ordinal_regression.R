# ==============================================================================
# USJ COMMUTER STRESS STUDY — ROBUST MULTIVARIATE MODELING
# Script: scripts/04_ordinal_regression.R
# Group Standardized Setup (Zero Warnings, Unbiased Odds Ratios & OLS Model)
# ==============================================================================

source("scripts/00_config.R")

cat("Executing Step 5: Robust Ordinal & Linear Regression Models...\n")

# 1. Load Cleaned Data
cleaned_df <- readRDS("data/clean/cleaned_commuter_data.rds")

# 2. Re-code Predictors into Parsimonious, Non-Sparse Categories
model_df <- cleaned_df %>%
  mutate(
    # Grouped Mode: Solves Zero-Cell Separation
    mode_grouped = case_when(
      main_mode %in% c("Bus", "Train") ~ "Public Transit (Bus/Train)",
      main_mode %in% c("Walk or bicycle", "Three-wheeler / PickMe / Uber") ~ "Active / Hired / Walking",
      TRUE ~ "Private Vehicle"
    ),
    mode_grouped = factor(mode_grouped, levels = c("Private Vehicle", "Public Transit (Bus/Train)", "Active / Hired / Walking")),
    
    # Grouped Distance (3 Levels)
    distance_grouped = case_when(
      distance %in% c("Less than 5 km") ~ "< 5 km",
      distance %in% c("5–14 km")       ~ "5–14 km",
      TRUE                              ~ "≥ 15 km"
    ),
    distance_grouped = factor(distance_grouped, levels = c("< 5 km", "5–14 km", "≥ 15 km")),
    
    # Binary Vehicle Changes
    transfers = case_when(
      vehicle_changes == "No change" ~ "Direct (No transfers)",
      TRUE                           ~ "Transfers Required"
    ),
    transfers = factor(transfers, levels = c("Direct (No transfers)", "Transfers Required")),
    
    gender = factor(gender),
    gender = relevel(gender, ref = "Male")
  )

# ==============================================================================
# MODEL A: PARSIMONIOUS ORDINAL LOGISTIC REGRESSION (PLUM)
# ==============================================================================
cat("\n==============================================================================\n")
cat("1. PARSIMONIOUS ORDINAL LOGISTIC REGRESSION (PLUM)\n")
cat("==============================================================================\n")

plum_model <- polr(
  stress_level ~ mode_grouped + transfers + distance_grouped + gender,
  data = model_df,
  Hess = TRUE
)

summary_table <- coef(summary(plum_model))
p_values <- pnorm(abs(summary_table[, "t value"]), lower.tail = FALSE) * 2

ci_vals <- suppressMessages(confint(plum_model))
num_coefs <- nrow(summary_table) - 2

or_table <- data.frame(
  Predictor  = rownames(summary_table)[1:num_coefs],
  B_Coef     = round(summary_table[1:num_coefs, "Value"], 3),
  Std_Error  = round(summary_table[1:num_coefs, "Std. Error"], 3),
  Odds_Ratio = round(exp(summary_table[1:num_coefs, "Value"]), 3),
  CI_2.5     = round(exp(ci_vals[1:num_coefs, 1]), 3),
  CI_97.5    = round(exp(ci_vals[1:num_coefs, 2]), 3),
  p_value    = format.pval(p_values[1:num_coefs], digits = 4)
)

print(or_table, row.names = FALSE)

null_model <- polr(stress_level ~ 1, data = model_df, Hess = TRUE)
mcfadden_r2 <- 1 - (as.numeric(logLik(plum_model)) / as.numeric(logLik(null_model)))

cat("\n------------------------------------------------------------------------------\n")
cat("PLUM GOODNESS-OF-FIT STATISTICS:\n")
cat("AIC                  :", round(AIC(plum_model), 2), "\n")
cat("Log Likelihood       :", round(as.numeric(logLik(plum_model)), 2), "\n")
cat("McFadden Pseudo R²   :", round(mcfadden_r2, 4), "\n")
cat("------------------------------------------------------------------------------\n")

# ==============================================================================
# MODEL B: MULTIPLE LINEAR REGRESSION (OLS) ON CONTINUOUS STS SCORE
# ==============================================================================
cat("\n==============================================================================\n")
cat("2. MULTIPLE LINEAR REGRESSION (OLS) ON CONTINUOUS STS STRESS SCORE\n")
cat("==============================================================================\n")

ols_model <- lm(stress_score ~ mode_grouped + transfers + distance_grouped + gender, data = model_df)
ols_summary <- summary(ols_model)
print(round(ols_summary$coefficients, 4))

cat("\n------------------------------------------------------------------------------\n")
cat("OLS GOODNESS-OF-FIT STATISTICS:\n")
cat("Multiple R²          :", round(ols_summary$r.squared, 4), "\n")
cat("Adjusted R²          :", round(ols_summary$adj.r.squared, 4), "\n")
cat("Overall F-statistic  :", round(ols_summary$fstatistic[1], 3), 
    "(p =", format.pval(pf(ols_summary$fstatistic[1], ols_summary$fstatistic[2], ols_summary$fstatistic[3], lower.tail = FALSE), digits = 4), ")\n")
cat("------------------------------------------------------------------------------\n")

# Save Models & Export Results
saveRDS(plum_model, "outputs/tables/plum_model.rds")
saveRDS(ols_model, "outputs/tables/ols_model.rds")
write_xlsx(list("PLUM_Results" = or_table, "OLS_Coefficients" = as.data.frame(ols_summary$coefficients)), 
           "outputs/tables/ordinal_regression_results.xlsx")

cat("\nStep 5 Complete: All models saved successfully without numeric warnings.\n")

