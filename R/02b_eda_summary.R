# ==============================================================================
# STEP 2B: EDA & CROSS-TABULATIONS (N = 92)
# ==============================================================================
cleaned_df <- readRDS("cleaned_commuter_data.rds")

cat("\n--- [H1] Mode vs Stress ---\n")
print(table(Mode = cleaned_df$B2_Mode, Stress = cleaned_df$Stress_Level))

cat("\n--- [H2] Distance vs Stress ---\n")
print(table(Distance = cleaned_df$B1_Distance, Stress = cleaned_df$Stress_Level))

cat("\n--- [H3] Transfers vs Stress ---\n")
print(table(Transfers = cleaned_df$B3_Transfers, Stress = cleaned_df$Stress_Level))

cat("\n--- [H4] Delays vs Stress ---\n")
print(table(Delays = cleaned_df$B7_Delays, Stress = cleaned_df$Stress_Level))

cat("\n--- [H5] Seating vs Stress ---\n")
print(table(Seating = cleaned_df$B6_Seating, Stress = cleaned_df$Stress_Level))

