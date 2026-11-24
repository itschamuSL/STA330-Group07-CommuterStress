# ==============================================================================
# USJ COMMUTER STRESS STUDY — PSYCHOMETRICS, EFA & VALIDITY
# Script: scripts/02_factor_analysis.R
# Group Standardized Setup
# ==============================================================================

source("scripts/00_config.R")

cat("Executing Step 3: Psychometric Validation, EFA & Convergent Validity...\n")

# 1. Load Cleaned Dataset
cleaned_df <- readRDS("data/clean/cleaned_commuter_data.rds")

# Robust vectorized helper function for text & numeric Likert labels
score_of <- function(x) {
  if (is.numeric(x)) return(x)
  
  x_str <- trimws(as.character(x))
  leading_num <- suppressWarnings(as.numeric(sub(" .*", "", x_str)))
  
  xl <- tolower(x_str)
  mapped_num <- case_when(
    grepl("not at all", xl)  ~ 1,
    grepl("a little", xl)    ~ 2,
    grepl("some extent", xl) ~ 3,
    grepl("rather much", xl) ~ 4,
    grepl("very much", xl)   ~ 5,
    TRUE                     ~ suppressWarnings(as.numeric(x_str))
  )
  
  ifelse(!is.na(leading_num), leading_num, mapped_num)
}

# 2. Select the 3 STS Numeric Items
sts_items <- cleaned_df %>% dplyr::select(c1_num, c2_num, c3_num)

cat("\n==============================================================================\n")
cat("1. PSYCHOMETRIC VALIDATION OF SATISFACTION WITH TRAVEL SCALE (STS)\n")
cat("==============================================================================\n")

# 3. Cronbach's Alpha (Reliability)
alpha_res <- psych::alpha(sts_items)$total$raw_alpha
cat("1. Cronbach's Alpha (Raw α) :", round(alpha_res, 4), "\n")

# 4. Kaiser-Meyer-Olkin (KMO) Sampling Adequacy
kmo_res <- psych::KMO(sts_items)$MSA
cat("2. KMO Index (MSA)          :", round(kmo_res, 4), "\n")

# 5. Bartlett's Test of Sphericity
bart_res <- psych::cortest.bartlett(sts_items, n = nrow(cleaned_df))$p.value
cat("3. Bartlett's Test P-value  :", format.pval(bart_res, digits = 4), "\n\n")

# 6. Exploratory Factor Analysis (EFA - 1 Factor)
cat("--- 4. EFA (1-FACTOR SOLUTION) ---\n")
efa_res <- psych::fa(sts_items, nfactors = 1, rotate = "none", fm = "pa")
print(efa_res$loadings)

cat("\nSS Loadings    :", round(efa_res$Vaccounted["SS loadings", 1], 3), "\n")
cat("Proportion Var :", round(efa_res$Vaccounted["Proportion Var", 1], 3), "\n\n")

# 7. Convergent Validity Test (Spearman correlation with C4 - Elo et al., 2003)
cat("==============================================================================\n")
cat("2. CONVERGENT VALIDITY TEST (Correlation with C4 Single-Item Stress)\n")
cat("==============================================================================\n")

c4_num <- score_of(cleaned_df$stress_these_days)

# Calculate Spearman correlation
spearman_test <- cor.test(cleaned_df$stress_score, c4_num, method = "spearman", exact = FALSE)

cat("Spearman rho (ρ)   :", round(spearman_test$estimate, 4), "\n")
cat("p-value            :", format.pval(spearman_test$p.value, digits = 4), "\n")

