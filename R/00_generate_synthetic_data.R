# ==============================================================================
# STEP 0: GENERATE REALISTIC CORRELATED SYNTHETIC DATASET (N = 250)
# ==============================================================================

set.seed(330) # For reproducible results
n_sim <- 250

# Generate an underlying latent stress trait for each simulated respondent
latent_stress <- rnorm(n_sim, mean = 0, sd = 1)

# Generate STS items strongly tied to the latent trait + random error
# Note: Raw scale is -3 (Stressed) to +3 (Relaxed), so higher latent stress = lower raw rating
c1_raw <- pmax(-3, pmin(3, round(-1.2 * latent_stress + rnorm(n_sim, 0, 1.0))))
c2_raw <- pmax(-3, pmin(3, round(-1.1 * latent_stress + rnorm(n_sim, 0, 1.0))))
c3_raw <- pmax(-3, pmin(3, round(-1.0 * latent_stress + rnorm(n_sim, 0, 1.1))))

synthetic_data <- data.frame(
  ID = paste0("SIM_", sprintf("%03d", 1:n_sim)),
  Submitted = Sys.time() - runif(n_sim, 0, 86400 * 7),
  Status = "complete",
  Language = sample(c("English", "Sinhala", "Tamil"), n_sim, replace = TRUE, prob = c(0.6, 0.3, 0.1)),
  `Duration (s)` = round(runif(n_sim, 120, 600)),
  
  # Section A
  A1 = "yes",
  A2 = sample(c("3-4", "5+"), n_sim, replace = TRUE, prob = c(0.4, 0.6)),
  index = paste0("AS2023", sprintf("%03d", 1:n_sim)),
  A3 = sample(c("applied_sciences", "engineering", "management_commerce", "technology", 
                "humanities_social", "computing", "medical", "dental", "allied_health", "urban_aquatic"),
              n_sim, replace = TRUE, prob = c(0.25, 0.15, 0.20, 0.10, 0.10, 0.08, 0.05, 0.02, 0.03, 0.02)),
  A4 = sample(1:4, n_sim, replace = TRUE, prob = c(0.3, 0.3, 0.25, 0.15)),
  A5 = sample(c("female", "male"), n_sim, replace = TRUE, prob = c(0.55, 0.45)),
  A6 = sample(c("home", "boarding", "hostel"), n_sim, replace = TRUE, prob = c(0.6, 0.25, 0.15)),
  
  # Section B (Predictors linked slightly to latent stress for realistic modeling)
  B1 = sample(1:5, n_sim, replace = TRUE, prob = c(0.15, 0.35, 0.30, 0.12, 0.08)),
  B2 = sample(c("bus", "train", "three_wheeler", "private", "walk_bicycle"), n_sim, replace = TRUE, prob = c(0.55, 0.15, 0.10, 0.12, 0.08)),
  B3 = sample(0:3, n_sim, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  B4 = sample(c("walk", "bus", "three_wheeler", "dropped", "na"), n_sim, replace = TRUE),
  B5 = sample(c("walk", "bus", "three_wheeler", "dropped", "na"), n_sim, replace = TRUE),
  B6 = sample(c("seat", "stand", "na"), n_sim, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
  B7 = sample(0:3, n_sim, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.15)),
  B8 = sample(1:5, n_sim, replace = TRUE),
  B9 = sample(0:4, n_sim, replace = TRUE),
  B10 = sample(1:4, n_sim, replace = TRUE),
  B11 = sample(0:4, n_sim, replace = TRUE),
  B12 = sample(1:4, n_sim, replace = TRUE),
  
  # Section C (Correlated STS scale)
  C1 = c1_raw,
  C2 = c2_raw,
  C3 = c3_raw,
  C4 = pmax(1, pmin(5, round(3 + 0.8 * latent_stress + rnorm(n_sim, 0, 0.8)))),
  
  # Section D Controls
  D1 = pmax(1, pmin(5, round(3 - 0.7 * latent_stress + rnorm(n_sim, 0, 0.8)))), # Poor sleep linked to stress
  D2 = pmax(1, pmin(5, round(3 + 0.7 * latent_stress + rnorm(n_sim, 0, 0.8)))), # High workload linked to stress
  D3 = sample(1:5, n_sim, replace = TRUE),
  D4 = sample(1:5, n_sim, replace = TRUE),
  check.names = FALSE
)

write.csv(synthetic_data, "synthetic_commuter_data_250.csv", row.names = FALSE)
cat("Successfully updated 'synthetic_commuter_data_250.csv' with realistic correlated STS items.\n")