# ============================================================
# 03_summary_tables.R
# TDC 2026 - 90 Day Survival Data
# Summary tables and statistics
# ============================================================

# Load packages and project setup
source("scripts/01_setup.R")

# Load cleaned data
survival_clean <- read_csv("data/processed/survival_clean.csv")

# ============================================================
# Overall dataset summary
# ============================================================

overall_summary <- survival_clean %>%
  summarise(
    Total_Records = n(),
    Sites = n_distinct(Site),
    Genotypes = n_distinct(`Genotype ID`),
    Species = n_distinct(paste(Genus, Species)),
    Mean_Survival = mean(
      `Cluster Survival at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    Median_Survival = median(
      `Cluster Survival at 90 Day Monitoring`,
      na.rm = TRUE
    )
  )

overall_summary
