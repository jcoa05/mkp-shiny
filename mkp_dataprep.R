# ============================================================================
# MoustiKAP-P Shiny App: Data Preparation Script
# ============================================================================
# This script processes the simulated KAP data and creates aggregated datasets
# optimized for the Shiny app visualization
#
# Run this script AFTER running the simulation (mkp-sim.Rmd)
# Output: data/app_data.rds (used by the Shiny app)
# ============================================================================

library(tidyverse)
library(sf)

# ----------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------

# Path to simulated data (adjust if needed)
DATA_PATH <- "mkp_kap_simulated_data.csv"

# Reference year for exposure calculation
REFERENCE_YEAR <- 2024L

# Output path
OUTPUT_PATH <- "data/app_data.rds"

# ----------------------------------------------------------------------------
# Load and Process Data
# ----------------------------------------------------------------------------

cat("Loading simulated data...\n")
df_raw <- read_csv(DATA_PATH, show_col_types = FALSE)

cat("Processing individual-level data...\n")

# Ensure all required variables exist
required_vars <- c(

  "id", "A5_departement", "A3_region", "geo_zone",
  "protection_count", "protection_intensity", "protection_intensity_norm",
  "knowledge_total", "risk_perception_score", "efficacy_score",
  "exposure_years", "exposure_category", "colonization_year",
  "climate_suitability_index", "climate_suitability_cat",
  "concordance_gap", "concordance_category",
  "aedes_present_dept", "dept_latitude"
)

missing_vars <- setdiff(required_vars, names(df_raw))
if (length(missing_vars) > 0) {
  stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
}

# ----------------------------------------------------------------------------
# Aggregate to Département Level
# ----------------------------------------------------------------------------

cat("Aggregating to département level...\n")

df_dept <- df_raw %>%
  group_by(A5_departement) %>%
  summarise(
    # Sample characteristics
    n_respondents = n(),
    
    # Geographic identifiers
    region = first(A3_region),
    geo_zone = first(geo_zone),
    dept_latitude = first(dept_latitude),
    
    # Aedes colonization
    aedes_present = first(aedes_present_dept),
    colonization_year = first(colonization_year),
    exposure_years = first(exposure_years),
    exposure_category = first(exposure_category),
    
    # Climate
    climate_suitability_index = first(climate_suitability_index),
    climate_suitability_cat = first(climate_suitability_cat),
    
    # Protection outcomes (means)
    mean_protection_count = mean(protection_count, na.rm = TRUE),
    sd_protection_count = sd(protection_count, na.rm = TRUE),
    mean_protection_intensity = mean(protection_intensity, na.rm = TRUE),
    sd_protection_intensity = sd(protection_intensity, na.rm = TRUE),
    mean_protection_intensity_norm = mean(protection_intensity_norm, na.rm = TRUE),
    
    # Knowledge and perceptions
    mean_knowledge = mean(knowledge_total, na.rm = TRUE),
    sd_knowledge = sd(knowledge_total, na.rm = TRUE),
    mean_risk_perception = mean(risk_perception_score, na.rm = TRUE),
    sd_risk_perception = sd(risk_perception_score, na.rm = TRUE),
    mean_efficacy = mean(efficacy_score, na.rm = TRUE),
    sd_efficacy = sd(efficacy_score, na.rm = TRUE),
    
    # Concordance
    mean_concordance_gap = mean(concordance_gap, na.rm = TRUE),
    
    # Proportions
    prop_under_protected = mean(concordance_category == "Under-protected", na.rm = TRUE),
    prop_over_protected = mean(concordance_category == "Over-protected", na.rm = TRUE),
    prop_concordant = mean(concordance_category == "Concordant", na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  rename(departement = A5_departement) %>%
  mutate(
    # Compute ranks for concordance analysis
    climate_risk_rank = percent_rank(climate_suitability_index),
    protection_rank = percent_rank(mean_protection_intensity),
    dept_concordance_gap = climate_risk_rank - protection_rank,
    
    # Concordance category at département level
    dept_concordance_cat = case_when(
      dept_concordance_gap > 0.15 ~ "Under-protected",
      dept_concordance_gap < -0.15 ~ "Over-protected",
      TRUE ~ "Concordant"
    )
  )

# ----------------------------------------------------------------------------
# Fit Adaptation Models
# ----------------------------------------------------------------------------

cat("Fitting adaptation curve models...\n")

# Filter to colonized départements only for model fitting
df_colonized <- df_dept %>%
  filter(aedes_present == 1, exposure_years > 0)

# Fit multiple functional forms
models <- list()

# Model 1: Linear
models$linear <- lm(mean_protection_intensity ~ exposure_years, data = df_colonized)

# Model 2: Logarithmic
models$logarithmic <- lm(mean_protection_intensity ~ log(exposure_years + 1), data = df_colonized)

# Model 3: Quadratic
models$quadratic <- lm(mean_protection_intensity ~ exposure_years + I(exposure_years^2), data = df_colonized)

# Model 4: Square root
models$sqrt <- lm(mean_protection_intensity ~ sqrt(exposure_years), data = df_colonized)

# Extract model statistics
model_stats <- tibble(
 model_name = names(models),
 model_label = c("Linear", "Logarithmic", "Quadratic", "Square Root"),
 formula_display = c(
   "Y = β₀ + β₁·years",
   "Y = β₀ + β₁·log(years+1)",
   "Y = β₀ + β₁·years + β₂·years²",
   "Y = β₀ + β₁·√years"
 ),
 aic = map_dbl(models, AIC),
 bic = map_dbl(models, BIC),
 r_squared = map_dbl(models, ~ summary(.x)$r.squared),
 adj_r_squared = map_dbl(models, ~ summary(.x)$adj.r.squared),
 rmse = map_dbl(models, ~ sqrt(mean(residuals(.x)^2)))
) %>%
 mutate(
   delta_aic = aic - min(aic),
   delta_bic = bic - min(bic),
   is_best_aic = delta_aic == 0,
   is_best_bic = delta_bic == 0
 ) %>%
 arrange(aic)

# Generate predictions for plotting
prediction_grid <- tibble(
  exposure_years = seq(0, max(df_colonized$exposure_years) + 2, by = 0.5)
)

model_predictions <- map_df(names(models), function(m) {
  pred <- predict(models[[m]], newdata = prediction_grid, se.fit = TRUE)
  tibble(
    model_name = m,
    exposure_years = prediction_grid$exposure_years,
    fitted = pred$fit,
    se = pred$se.fit,
    lower = fitted - 1.96 * se,
    upper = fitted + 1.96 * se
  )
})

# Best model based on AIC
best_model_name <- model_stats$model_name[1]
best_model <- models[[best_model_name]]

# Calculate adaptation metrics from best model
adaptation_metrics <- list(
  best_model = best_model_name,
  r_squared = summary(best_model)$r.squared,
  
  # Predicted protection at key time points
  protection_at_0 = predict(best_model, newdata = data.frame(exposure_years = 0.1)),
  protection_at_5 = predict(best_model, newdata = data.frame(exposure_years = 5)),
  protection_at_10 = predict(best_model, newdata = data.frame(exposure_years = 10)),
  protection_at_20 = predict(best_model, newdata = data.frame(exposure_years = 20)),
  
  # Time to reach certain thresholds (if model allows)
  max_observed_protection = max(df_colonized$mean_protection_intensity),
  min_observed_protection = min(df_colonized$mean_protection_intensity)
)

# ----------------------------------------------------------------------------
# Compute Residuals for Priority Identification
# ----------------------------------------------------------------------------

cat("Computing residuals for intervention targeting...\n")

df_dept <- df_dept %>%
  mutate(
    # Predicted protection based on best model
    predicted_protection = case_when(
      aedes_present == 0 ~ NA_real_,
      exposure_years == 0 ~ NA_real_,
      TRUE ~ predict(best_model, newdata = data.frame(exposure_years = exposure_years))
    ),
    
    # Residual: observed - expected
    protection_residual = mean_protection_intensity - predicted_protection,
    
    # Priority classification
    priority_level = case_when(
      is.na(protection_residual) ~ "Not applicable",
      protection_residual < -0.3 ~ "High priority",
      protection_residual < -0.1 ~ "Medium priority",
      protection_residual > 0.3 ~ "Exemplary",
      protection_residual > 0.1 ~ "Above average",
      TRUE ~ "On track"
    )
  )

# ----------------------------------------------------------------------------
# Create France Département Coordinates (Approximate Centroids)
# ----------------------------------------------------------------------------

cat("Creating geographic reference data...\n")

# Approximate département centroids for mapping
# These are simplified coordinates for visualization
dept_coords <- tribble(
  ~departement, ~lon, ~lat, ~dept_name,
  # Île-de-France
  "75", 2.35, 48.86, "Paris",
  "77", 2.99, 48.62, "Seine-et-Marne",
  "78", 1.85, 48.80, "Yvelines",
  "91", 2.23, 48.52, "Essonne",
  "92", 2.22, 48.84, "Hauts-de-Seine",
  "93", 2.44, 48.91, "Seine-Saint-Denis",
  "94", 2.47, 48.78, "Val-de-Marne",
  "95", 2.17, 49.08, "Val-d'Oise",
  # Auvergne-Rhône-Alpes
  "01", 5.35, 46.10, "Ain",
  "03", 3.20, 46.40, "Allier",
  "07", 4.42, 44.75, "Ardèche",
  "15", 2.67, 45.03, "Cantal",
  "26", 5.17, 44.68, "Drôme",
  "38", 5.58, 45.25, "Isère",
  "42", 4.17, 45.72, "Loire",
  "43", 3.88, 45.08, "Haute-Loire",
  "63", 3.15, 45.72, "Puy-de-Dôme",
  "69", 4.64, 45.87, "Rhône",
  "73", 6.42, 45.50, "Savoie",
  "74", 6.35, 46.00, "Haute-Savoie",
  # Bourgogne-Franche-Comté
  "21", 4.85, 47.42, "Côte-d'Or",
  "25", 6.35, 47.17, "Doubs",
  "39", 5.72, 46.73, "Jura",
  "58", 3.50, 47.12, "Nièvre",
  "70", 6.08, 47.62, "Haute-Saône",
  "71", 4.58, 46.65, "Saône-et-Loire",
  "89", 3.58, 47.83, "Yonne",
  "90", 6.87, 47.63, "Territoire de Belfort",
  # Bretagne
  "22", -2.83, 48.45, "Côtes-d'Armor",
  "29", -4.25, 48.40, "Finistère",
  "35", -1.67, 48.12, "Ille-et-Vilaine",
  "56", -2.75, 47.75, "Morbihan",
  # Centre-Val de Loire
  "18", 2.42, 47.07, "Cher",
  "28", 1.33, 48.33, "Eure-et-Loir",
  "36", 1.58, 46.80, "Indre",
  "37", 0.75, 47.25, "Indre-et-Loire",
  "41", 1.33, 47.58, "Loir-et-Cher",
  "45", 2.25, 47.92, "Loiret",
  # Corse
  "2A", 8.97, 41.87, "Corse-du-Sud",
  "2B", 9.25, 42.40, "Haute-Corse",
  # Grand Est
  "08", 4.62, 49.62, "Ardennes",
  "10", 4.08, 48.33, "Aube",
  "51", 4.17, 48.97, "Marne",
  "52", 5.17, 48.08, "Haute-Marne",
  "54", 6.17, 48.75, "Meurthe-et-Moselle",
  "55", 5.33, 49.00, "Meuse",
  "57", 6.67, 49.08, "Moselle",
  "67", 7.58, 48.67, "Bas-Rhin",
  "68", 7.25, 47.92, "Haut-Rhin",
  "88", 6.42, 48.17, "Vosges",
  # Hauts-de-France
  "02", 3.58, 49.50, "Aisne",
  "59", 3.17, 50.42, "Nord",
  "60", 2.50, 49.42, "Oise",
  "62", 2.33, 50.50, "Pas-de-Calais",
  "80", 2.33, 49.92, "Somme",
  # Normandie
  "14", -0.42, 49.17, "Calvados",
  "27", 1.00, 49.08, "Eure",
  "50", -1.33, 49.00, "Manche",
  "61", 0.00, 48.58, "Orne",
  "76", 1.00, 49.67, "Seine-Maritime",
  # Nouvelle-Aquitaine
  "16", 0.17, 45.72, "Charente",
  "17", -0.75, 45.83, "Charente-Maritime",
  "19", 1.92, 45.35, "Corrèze",
  "23", 2.08, 46.17, "Creuse",
  "24", 0.75, 45.17, "Dordogne",
  "33", -0.58, 44.75, "Gironde",
  "40", -0.83, 43.92, "Landes",
  "47", 0.50, 44.35, "Lot-et-Garonne",
  "64", -0.75, 43.25, "Pyrénées-Atlantiques",
  "79", -0.33, 46.50, "Deux-Sèvres",
  "86", 0.50, 46.58, "Vienne",
  "87", 1.25, 45.92, "Haute-Vienne",
  # Occitanie
  "09", 1.50, 42.92, "Ariège",
  "11", 2.42, 43.17, "Aude",
  "12", 2.67, 44.25, "Aveyron",
  "30", 4.17, 44.00, "Gard",
  "31", 1.33, 43.42, "Haute-Garonne",
  "32", 0.42, 43.67, "Gers",
  "34", 3.42, 43.58, "Hérault",
  "46", 1.58, 44.58, "Lot",
  "48", 3.50, 44.50, "Lozère",
  "65", 0.17, 43.08, "Hautes-Pyrénées",
  "66", 2.67, 42.58, "Pyrénées-Orientales",
  "81", 2.17, 43.83, "Tarn",
  "82", 1.25, 44.08, "Tarn-et-Garonne",
  # Pays de la Loire
  "44", -1.75, 47.33, "Loire-Atlantique",
  "49", -0.50, 47.42, "Maine-et-Loire",
  "53", -0.75, 48.08, "Mayenne",
  "72", 0.17, 47.92, "Sarthe",
  "85", -1.33, 46.67, "Vendée",
  # Provence-Alpes-Côte d'Azur
  "04", 6.25, 44.08, "Alpes-de-Haute-Provence",
  "05", 6.25, 44.67, "Hautes-Alpes",
  "06", 7.17, 43.92, "Alpes-Maritimes",
  "13", 5.17, 43.50, "Bouches-du-Rhône",
  "83", 6.25, 43.42, "Var",
  "84", 5.17, 44.00, "Vaucluse"
)

# Merge coordinates with département data
df_dept <- df_dept %>%
  left_join(dept_coords, by = "departement")

# ----------------------------------------------------------------------------
# Aggregate to Région Level
# ----------------------------------------------------------------------------

cat("Aggregating to région level...\n")

df_region <- df_raw %>%
  group_by(A3_region) %>%
  summarise(
    n_respondents = n(),
    geo_zone = first(geo_zone),
    mean_protection_intensity = mean(protection_intensity, na.rm = TRUE),
    mean_knowledge = mean(knowledge_total, na.rm = TRUE),
    mean_risk_perception = mean(risk_perception_score, na.rm = TRUE),
    mean_efficacy = mean(efficacy_score, na.rm = TRUE),
    mean_exposure_years = mean(exposure_years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(region = A3_region)

# ----------------------------------------------------------------------------
# Summary Statistics for App Display
# ----------------------------------------------------------------------------

cat("Computing summary statistics...\n")

summary_stats <- list(
  # Sample size
  n_total = nrow(df_raw),
  n_dept = nrow(df_dept),
  n_colonized_dept = sum(df_dept$aedes_present == 1),
  n_not_colonized_dept = sum(df_dept$aedes_present == 0),
  
 # Protection statistics
  mean_protection_intensity = mean(df_raw$protection_intensity, na.rm = TRUE),
  sd_protection_intensity = sd(df_raw$protection_intensity, na.rm = TRUE),
  mean_protection_count = mean(df_raw$protection_count, na.rm = TRUE),
  
  # Exposure statistics
  mean_exposure_years = mean(df_raw$exposure_years[df_raw$aedes_present_dept == 1], na.rm = TRUE),
  max_exposure_years = max(df_raw$exposure_years, na.rm = TRUE),
  min_colonization_year = min(df_raw$colonization_year, na.rm = TRUE),
  
  # Concordance
  pct_under_protected = mean(df_raw$concordance_category == "Under-protected", na.rm = TRUE) * 100,
  pct_concordant = mean(df_raw$concordance_category == "Concordant", na.rm = TRUE) * 100,
  pct_over_protected = mean(df_raw$concordance_category == "Over-protected", na.rm = TRUE) * 100
)

# ----------------------------------------------------------------------------
# Package All Data for App
# ----------------------------------------------------------------------------

cat("Packaging data for Shiny app...\n")

app_data <- list(
  # Main datasets
  df_individual = df_raw,
  df_dept = df_dept,
  df_region = df_region,
  
 # Model results
  models = models,
  model_stats = model_stats,
  model_predictions = model_predictions,
  adaptation_metrics = adaptation_metrics,
  best_model_name = best_model_name,
  
  # Reference data
  dept_coords = dept_coords,
  
  # Summary statistics
  summary_stats = summary_stats,
  
  # Metadata
  metadata = list(
    created = Sys.time(),
    reference_year = REFERENCE_YEAR,
    n_observations = nrow(df_raw),
    n_departements = nrow(df_dept)
  )
)

# ----------------------------------------------------------------------------
# Save Output
# ----------------------------------------------------------------------------

cat("Saving app data to:", OUTPUT_PATH, "\n")
saveRDS(app_data, OUTPUT_PATH)

cat("\n=== Data Preparation Complete ===\n")
cat("Total observations:", summary_stats$n_total, "\n")
cat("Départements:", summary_stats$n_dept, "\n")
cat("  - Colonized:", summary_stats$n_colonized_dept, "\n
")
cat("  - Not colonized:", summary_stats$n_not_colonized_dept, "\n")
cat("Best adaptation model:", best_model_name, "(R² =", 
    round(adaptation_metrics$r_squared, 3), ")\n")
