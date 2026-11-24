# ==============================================================================
# USJ COMMUTER STRESS STUDY — PUBLICATION-QUALITY FIGURES
# Script: scripts/05_visualizations.R
# Group Standardized Visualizations
# ==============================================================================

source("scripts/00_config.R")

cat("Executing Step 6: Generating High-Resolution Publication Figures...\n")

# 1. Load Data & Model Results
cleaned_df <- readRDS("data/clean/cleaned_commuter_data.rds")
or_table   <- readxl::read_excel("outputs/tables/ordinal_regression_results.xlsx")

# ------------------------------------------------------------------------------
# FIGURE 1: Stress Level Distribution by Transport Mode
# ------------------------------------------------------------------------------
fig1 <- cleaned_df %>%
  ggplot(aes(x = main_mode, fill = stress_level)) +
  geom_bar(position = "fill", width = 0.65, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Low" = "#2b83ba", "Moderate" = "#fdae61", "High" = "#d7191c")) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1, face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Commute Stress Level Breakdown by Main Transport Mode",
    subtitle = "University of Sri Jayewardenepura Commuter Survey (N = 132)",
    x = "Primary Mode of Travel",
    y = "Proportion of Respondents",
    fill = "Stress Level:"
  )

ggsave("outputs/figures/fig1_stress_by_mode.png", fig1, width = 9, height = 6, dpi = 300)
cat("Saved: outputs/figures/fig1_stress_by_mode.png\n")

# ------------------------------------------------------------------------------
# FIGURE 2: Continuous STS Stress Score by Seating Condition (Boxplot)
# ------------------------------------------------------------------------------
fig2 <- cleaned_df %>%
  ggplot(aes(x = seat, y = stress_score, fill = seat)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 2) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    title = "Subjective Travel Stress (STS Score) by Seating Availability",
    subtitle = "Higher values indicate elevated acute commute stress (Ettema et al., 2011)",
    x = "Seating Condition",
    y = "Calculated STS Stress Score (-3 to +3)"
  )

ggsave("outputs/figures/fig2_stress_by_seat.png", fig2, width = 8, height = 5.5, dpi = 300)
cat("Saved: outputs/figures/fig2_stress_by_seat.png\n")

# ------------------------------------------------------------------------------
# FIGURE 3: Ordinal Logistic Regression Forest Plot (Odds Ratios)
# ------------------------------------------------------------------------------
forest_df <- or_table %>%
  mutate(
    Predictor = factor(Predictor, levels = rev(Predictor)),
    Sig = ifelse(as.numeric(p_value) < 0.05, "Statistically Significant (p < 0.05)", "Not Significant")
  )

fig3 <- forest_df %>%
  ggplot(aes(x = Odds_Ratio, y = Predictor, color = Sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = CI_2.5, xmax = CI_97.5), height = 0.3, linewidth = 0.8) +
  geom_point(size = 3.5) +
  scale_x_log10(breaks = c(0.2, 0.5, 1, 2, 5, 10, 20, 50, 100)) +
  scale_color_manual(values = c("Statistically Significant (p < 0.05)" = "#d7191c", "Not Significant" = "#4575b4")) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 13),
    axis.text.y = element_text(face = "bold")
  ) +
  labs(
    title = "Adjusted Odds Ratios for High Commute Stress (PLUM Model)",
    subtitle = "Reference: Private Vehicle | No Change | Distance < 5km | Male",
    x = "Odds Ratio (Log Scale, 95% Confidence Interval)",
    y = "Model Predictors",
    color = "Significance:"
  )

ggsave("outputs/figures/fig3_forest_plot.png", fig3, width = 9, height = 6, dpi = 300)
cat("Saved: outputs/figures/fig3_forest_plot.png\n\n")

cat("Step 6 Complete: All figures successfully saved in 'outputs/figures/'.\n")