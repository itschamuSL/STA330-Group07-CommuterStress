library(dplyr)

# ------------------------------------------------------------------------------
# 1. LOAD DATASET

data_path <- "https://raw.githubusercontent.com/itschamuSL/STA330-Group07-CommuterStress/refs/heads/Induwara-analysis/data/synth/synthetic_commuter_data_250.csv"

raw_df <- read.csv(data_path, check.names = FALSE)

# ------------------------------------------------------------------------------
# 2. SCREENING FILTERS (Consent, Frequency, Speeders)
# ------------------------------------------------------------------------------
step1_df <- raw_df %>%
  filter(
    Status == "complete",
    A1 == "yes",               # Informed consent
    A2 != "1-2",              # Regular commuters (>= 3 days/week)
    `Duration (s)` >= 45      # Speeding filter (>= 45 seconds)
  )

# ------------------------------------------------------------------------------
# 3. RECODE STRESS OUTCOME (Y) & STRAIGHT-LINING FILTER
# ------------------------------------------------------------------------------
cleaned_df <- step1_df %>%
  mutate(
    # Recode raw (-3 to +3) items to positive 1 to 7 scale (1 = Low Stress, 7 = High Stress)
    C1_Stressed = 4 - as.numeric(C1),
    C2_Worried  = 4 - as.numeric(C2),
    C3_Hurried  = 4 - as.numeric(C3),
    
    # Composite Sum Score (3 to 21)
    STS_Sum = C1_Stressed + C2_Worried + C3_Hurried
  ) %>%
  # Filter out straight-liners (identical answers across all 3 stress questions)
  rowwise() %>%
  filter(sd(c(C1_Stressed, C2_Worried, C3_Hurried), na.rm = TRUE) > 0) %>%
  ungroup() %>%
  # Categorize target outcome Y (Low, Moderate, High Stress)
  mutate(
    Y_Stress = case_when(
      STS_Sum <= 8  ~ "Low",
      STS_Sum <= 14 ~ "Moderate",
      TRUE          ~ "High"
    ),
    Y_Stress = factor(Y_Stress, levels = c("Low", "Moderate", "High"), ordered = TRUE)
  )

# ------------------------------------------------------------------------------
# 4. FACTORIZE PREDICTORS & CONTROLS
# ------------------------------------------------------------------------------
cleaned_df <- cleaned_df %>%
  mutate(
    # Demographics
    A3_Faculty   = factor(A3),
    A4_Year      = factor(A4, ordered = TRUE),
    A5_Gender    = factor(A5),
    A6_Resid     = factor(A6),
    
    # Transport Friction Factors (Predictors)
    B1_Distance  = factor(B1, ordered = TRUE),
    B2_Mode      = factor(B2),
    B3_Transfers = factor(B3, ordered = TRUE),
    B6_Seating   = factor(B6),
    B7_Delays    = factor(B7, ordered = TRUE),
    
    # Control Covariates
    C4_GenStress = as.numeric(C4),
    D1_Sleep     = as.numeric(D1),
    D2_Workload  = as.numeric(D2)
  )

# ------------------------------------------------------------------------------
# 5. SAVE CLEANED DATASET
# ------------------------------------------------------------------------------
saveRDS(cleaned_df, "cleaned_commuter_data.rds")
write.csv(cleaned_df, "cleaned_commuter_data.csv", row.names = FALSE)

# Display final cleaned sample size & outcome distribution
cat("Cleaned sample size N =", nrow(cleaned_df), "\n")
print(table(cleaned_df$Y_Stress))



