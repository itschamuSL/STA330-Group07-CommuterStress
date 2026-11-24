# ==============================================================================
# USJ COMMUTER STRESS STUDY — DESCRIPTIVE STATISTICS & BIVARIATE TESTS
# Script: scripts/03_descriptive_bivariate.R
# Group Standardized Variable Naming
# ==============================================================================

source("scripts/00_config.R")

cat("Executing Step 4: Descriptive Statistics & Bivariate Analyses...\n")

# 1. Load Cleaned Data
cleaned_df <- readRDS("data/clean/cleaned_commuter_data.rds")

# 2. Overall Target Distribution (Stress Levels & Mean Score)
cat("\n==============================================================================\n")
cat("1. OVERALL COMMUTE STRESS DISTRIBUTION (N = ", nrow(cleaned_df), ")\n", sep="")
cat("==============================================================================\n")

stress_summary <- cleaned_df %>%
  group_by(stress_level) %>%
  summarise(
    Count = n(),
    Percentage = round((n() / nrow(cleaned_df)) * 100, 2),
    Mean_Score = round(mean(stress_score, na.rm = TRUE), 3),
    SD_Score = round(sd(stress_score, na.rm = TRUE), 3)
  )

print(as.data.frame(stress_summary))

cat("\nOverall Mean STS Stress Score:", round(mean(cleaned_df$stress_score), 3),
    "(SD =", round(sd(cleaned_df$stress_score), 3), ")\n")

# 3. Bivariate Association Tests with Stress Level
cat("\n==============================================================================\n")
cat("2. BIVARIATE STATISTICAL TESTS WITH COMMUTE STRESS LEVEL\n")
cat("==============================================================================\n")

bivariate_vars <- c("main_mode", "seat", "vehicle_changes", "late_arrivals", 
                    "distance", "sleep_quality", "workload_pressure", "gender")

for (v in bivariate_vars) {
  if (v %in% names(cleaned_df)) {
    cat(sprintf("\n--------------------------------------------------\n"))
    cat(sprintf(" Factor: %s\n", toupper(v)))
    cat(sprintf("--------------------------------------------------\n"))
    
    # Contingency Table
    tbl <- table(cleaned_df[[v]], cleaned_df$stress_level)
    print(tbl)
    
    # Chi-square test (with simulated p-value for small cell counts)
    chisq_res <- suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE, B = 2000))
    
    # Kruskal-Wallis Test on continuous STS Score
    kw_res <- kruskal.test(cleaned_df$stress_score ~ cleaned_df[[v]])
    
    cat(sprintf("\nChi-Sq p-value (simulated) : %s\n", format.pval(chisq_res$p.value, digits = 4)))
    cat(sprintf("Kruskal-Wallis p-value     : %s\n", format.pval(kw_res$p.value, digits = 4)))
  }
}

