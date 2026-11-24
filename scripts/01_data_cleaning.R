# ==============================================================================
# USJ COMMUTER STRESS STUDY — DATA CLEANING & RECODING
# Script: scripts/01_data_cleaning.R
# Group Standardized Variable Naming
# ==============================================================================

source("scripts/00_config.R")

cat("Executing Step 2: Data Cleaning with Team-Standardized Variable Names...\n")

raw_df <- load_raw_commuter_data()

# Helper function to extract leading integer from Likert labels (e.g. "-2 Quite stressed" -> -2)
score_of <- function(x) as.numeric(sub(" .*", "", x))

# 1. Variable Mapping Dictionary (Group Standardized)
new_names <- c(
  A2  = "days_per_week",
  A3  = "faculty",
  A4  = "study_year",
  A5  = "gender",
  A6  = "residence",
  B1  = "distance",
  B2  = "main_mode",
  B3  = "vehicle_changes",
  B4  = "access_mode",
  B5  = "egress_mode",
  B6  = "seat",
  B7  = "late_arrivals",
  B8  = "trip_time",
  B9  = "wait_time",
  B10 = "departure_time",
  B11 = "travel_budget",
  B12 = "feel_unsafe",
  C1  = "stressed_relaxed",
  C2  = "worried_confident",
  C3  = "hurried_calm",
  C4  = "stress_these_days",
  D1  = "sleep_quality",
  D2  = "workload_pressure",
  D3  = "crowding",
  D4  = "weather_impact"
)

# 2. Extract leading question codes & select ONLY the 25 target columns safely
raw_headings <- names(raw_df)
codes        <- sub(" .*", "", raw_headings)

# Match target question codes while ignoring optional text fields like "A6 other"
target_indices <- which(codes %in% names(new_names) & !grepl("other", raw_headings, ignore.case = TRUE))

# Extract clean subset and apply snake_case column names
cleaned_df <- raw_df[, target_indices]
names(cleaned_df) <- unname(new_names[codes[target_indices]])

# 3. Merging Top 5 Faculties
top5_faculties <- cleaned_df %>%
  count(faculty, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(faculty)

cleaned_df <- cleaned_df %>%
  mutate(
    faculty = ifelse(faculty %in% top5_faculties, faculty, "Other faculties"),
    
    # Recode vehicle_changes strings
    vehicle_changes = recode(
      as.character(vehicle_changes),
      "0"         = "No change",
      "1"         = "1 change",
      "2"         = "2 changes",
      "3 or more" = "3 or more changes"
    )
  )

# 4. STS Score & Ordinal Stress Level Calculation (Ettema et al., 2011)
c1_vals <- score_of(cleaned_df$stressed_relaxed)
c2_vals <- score_of(cleaned_df$worried_confident)
c3_vals <- score_of(cleaned_df$hurried_calm)

raw_score <- -1 * ((c1_vals + c2_vals + c3_vals) / 3)

cleaned_df <- cleaned_df %>%
  mutate(
    c1_num = c1_vals,
    c2_num = c2_vals,
    c3_num = c3_vals,
    stress_score = round(raw_score, 2),
    stress_level = factor(
      case_when(
        raw_score > CUTOFF_HIGH ~ "High",
        raw_score < CUTOFF_LOW  ~ "Low",
        TRUE                    ~ "Moderate"
      ),
      levels = c("Low", "Moderate", "High"),
      ordered = TRUE
    )
  )

# 5. Save cleaned dataset for R pipeline (.rds) and Excel format (.xlsx)
saveRDS(cleaned_df, "data/clean/cleaned_commuter_data.rds")
write_xlsx(list("Cleaned data" = cleaned_df), "data/clean/commuter-survey-cleaned.xlsx")
cat("Cleaned dataset successfully saved to 'data/clean/cleaned_commuter_data.rds'. Total N =", nrow(cleaned_df), "\n")

