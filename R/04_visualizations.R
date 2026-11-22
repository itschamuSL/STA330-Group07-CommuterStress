# ==============================================================================
# USJ COMMUTER STRESS STUDY — STEP 4: VISUALIZATIONS & CHARTS
# Script: 04_visualizations.R
# Group: 7 (STA 330)
# ==============================================================================

# Load required packages
library(dplyr)
library(ggplot2)
library(MASS)

# ------------------------------------------------------------------------------
# 1. LOAD CLEANED DATASET
# ------------------------------------------------------------------------------
if (!file.exists("cleaned_commuter_data.rds")) {
  stop("ERROR: 'cleaned_commuter_data.rds' not found. Run 01_data_cleaning.R first.")
}

cleaned_df <- readRDS("cleaned_commuter_data.rds")
cat("Cleaned dataset loaded. N =", nrow(cleaned_df), "\n\n")

# Custom Color Palette for Stress Levels
stress_colors <- c("Low" = "#2B6CB0", "Moderate" = "#DD6B20", "High" = "#C53030")

# ------------------------------------------------------------------------------
# CHART 1: OUTCOME VARIABLE DISTRIBUTION (Y_Stress)
# ------------------------------------------------------------------------------
cat("Generating Chart 1: Outcome Stress Distribution...\n")

p1 <- ggplot(cleaned_df, aes(x = Y_Stress, fill = Y_Stress)) +
  geom_bar(width = 0.55, color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = stress_colors) +
  labs(
    title = "Distribution of Commuter Stress Levels",
    subtitle = "Target Outcome Variable (Y_Stress)",
    x = "Stress Category",
    y = "Number of Students (Count)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", color = "#1A365D")
  )

print(p1)
ggsave("chart1_stress_distribution.png", plot = p1, width = 6, height = 4.5, dpi = 300)

# ------------------------------------------------------------------------------
# CHART 2: STRESS BY TRANSPORT MODE (H1)
# ------------------------------------------------------------------------------
cat("Generating Chart 2: Stress by Transport Mode...\n")

p2 <- ggplot(cleaned_df, aes(x = B2_Mode, fill = Y_Stress)) +
  geom_bar(position = "fill", width = 0.6, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = stress_colors) +
  labs(
    title = "Commuter Stress Distribution by Transport Mode (H1)",
    subtitle = "Proportional breakdown per main transport mode",
    x = "Main Transport Mode",
    y = "Percentage of Students",
    fill = "Stress Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title = element_text(face = "bold", color = "#1A365D")
  )

print(p2)
ggsave("chart2_stress_by_mode.png", plot = p2, width = 7.5, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# CHART 3: STRESS BY DELAY FREQUENCY (H4)
# ------------------------------------------------------------------------------
cat("Generating Chart 3: Stress by Delay Frequency...\n")

p3 <- ggplot(cleaned_df, aes(x = B7_Delays, fill = Y_Stress)) +
  geom_bar(position = "fill", width = 0.6, color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = stress_colors) +
  labs(
    title = "Commuter Stress by Delay Frequency (H4)",
    subtitle = "Proportional breakdown by weekly 15+ minute arrival delays",
    x = "Delay Frequency (Days per Week)",
    y = "Percentage of Students",
    fill = "Stress Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#1A365D")
  )

print(p3)
ggsave("chart3_stress_by_delays.png", plot = p3, width = 7.5, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# CHART 4: REGRESSION ODDS RATIOS (FOREST PLOT)
# ------------------------------------------------------------------------------
cat("Generating Chart 4: Regression Odds Ratio Forest Plot...\n")

# Fit the Ordinal Regression Model
olr_model <- MASS::polr(
  Y_Stress ~ B2_Mode + B1_Distance + B3_Transfers + B7_Delays + B6_Seating + 
    D1_Sleep + D2_Workload + C4_GenStress,
  data = cleaned_df,
  Hess = TRUE
)

# Extract Odds Ratios & CIs for predictors
predictor_names <- names(coef(olr_model))
coef_matrix     <- coef(summary(olr_model))[predictor_names, ]
ci_bounds       <- exp(suppressMessages(confint.default(olr_model)))

forest_df <- data.frame(
  Variable   = factor(predictor_names, levels = rev(predictor_names)),
  Odds_Ratio = exp(coef_matrix[, "Value"]),
  Lower_CI   = ci_bounds[, 1],
  Upper_CI   = ci_bounds[, 2]
)

p4 <- ggplot(forest_df, aes(x = Odds_Ratio, y = Variable)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "gray40", size = 0.8) +
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.25, color = "#2B6CB0", size = 0.8) +
  geom_point(size = 3.2, color = "#1A365D") +
  scale_x_log10() +
  labs(
    title = "Impact of Transit Friction Predictors on Commuter Stress",
    subtitle = "Adjusted Proportional Odds Ratios (OR) with 95% Confidence Intervals",
    x = "Odds Ratio (Log Scale; Reference line = 1.0)",
    y = "Predictor / Baseline Control Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#1A365D")
  )

print(p4)
ggsave("chart4_odds_ratio_forest_plot.png", plot = p4, width = 8.5, height = 6, dpi = 300)

cat("\n==============================================================================\n")
cat("SUCCESS: All 4 charts displayed and saved to your project directory:\n")
cat("  - chart1_stress_distribution.png\n")
cat("  - chart2_stress_by_mode.png\n")
cat("  - chart3_stress_by_delays.png\n")
cat("  - chart4_odds_ratio_forest_plot.png\n")
cat("==============================================================================\n")