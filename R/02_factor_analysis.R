# ==============================================================================
# STEP 2: RELIABILITY & EFA ON REAL DATA (N = 92)
# ==============================================================================
library(dplyr)
library(psych)

cleaned_df <- readRDS("cleaned_commuter_data.rds")
sts_items  <- cleaned_df %>% select(C1_Stressed, C2_Worried, C3_Hurried)

cat("--- 1. CRONBACH'S ALPHA ---\n")
print(psych::alpha(sts_items)$total$raw_alpha)

cat("\n--- 2. KMO INDEX ---\n")
print(psych::KMO(sts_items)$MSA)

cat("\n--- 3. BARTLETT'S TEST ---\n")
print(psych::cortest.bartlett(sts_items, n = nrow(cleaned_df))$p.value)

cat("\n--- 4. EFA (1-FACTOR SOLUTION) ---\n")
efa_res <- psych::fa(sts_items, nfactors = 1, rotate = "none", fm = "pa")
print(efa_res$loadings)

