# ============================================================
# TDC 2026 - 90D Survival Data: Figures & Tables
# Data cleaning and preparation
# will fill in missing information that tag lab does not add and clean up any spelling errors or missing genotypes
# ============================================================

# Load packages ------------------------------------------------

library(tidyverse)
library(readr)
library(here)


# Load raw data -------------------------------------------------
survival_raw <- read_csv("data/raw/dataForExcel.csv")

survival_clean <- survival_raw

glimpse(survival_raw)

# =========================================================
# DATA QUALITY CHECKS
# =========================================================

cat("\n==============================\n")
cat("DATA QUALITY CHECK\n")
cat("==============================\n")

# Basic dataset size
cat("\nRows:", nrow(survival_raw), "\n")
cat("Columns:", ncol(survival_raw), "\n")


# ---------------------------------------------------------
# Missing values
# ---------------------------------------------------------

cat("\n--- Missing Values ---\n")

missing_summary <- survival_raw %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Missing"
  ) %>%
  mutate(
    Percent_Missing = round(Missing / nrow(survival_raw) * 100, 1)
  ) %>%
  arrange(desc(Missing))

print(missing_summary, n = Inf)

# ---------------------------------------------------------
# Monitoring Date QC
# ---------------------------------------------------------

cat("\n--- Monitoring Date QC ---\n")

missing_monitoring_dates <- survival_raw %>%
  filter(is.na(`Monitoring Date`))

cat(
  "Missing Monitoring Dates:",
  nrow(missing_monitoring_dates),
  "of",
  nrow(survival_raw),
  "\n"
)

if (nrow(missing_monitoring_dates) == 0) {
  cat("PASS: All records have a Monitoring Date.\n")
} else {
  cat("CHECK: Some records are missing a Monitoring Date.\n")
  cat("These should be populated when additional monitoring data are added.\n")
  
  cat("\nMissing Monitoring Dates by Site:\n")
  
  missing_monitoring_dates %>%
    count(Site, sort = TRUE) %>%
    print(n = Inf)
}

# ---------------------------------------------------------
# Outplant Date QC
# ---------------------------------------------------------

cat("\n--- Outplant Date QC ---\n")

missing_outplant_dates <- survival_raw %>%
  filter(is.na(`Outplant Date`))

cat(
  "Missing Outplant Dates:",
  nrow(missing_outplant_dates),
  "of",
  nrow(survival_raw),
  "\n"
)

if (nrow(missing_outplant_dates) == 0) {
  cat("PASS: All records have an Outplant Date.\n")
} else {
  cat("CHECK: Some records are missing an Outplant Date.\n")
  
  missing_outplant_dates %>%
    select(
      Site,
      Genus,
      Species,
      `Genotype ID`,
      `Monitoring Date`
    ) %>%
    print(n = Inf)
}
#check where the missing NAs are coming from
survival_raw %>%
  filter(
    Site == "MS_P001",
    is.na(`Monitoring Date`) | is.na(`Outplant Date`)
  ) %>%
  select(
    Site,
    Genus,
    Species,
    `Genotype ID`,
    `Outplant Date`,
    `Monitoring Date`
  )

#there is a problem with MS_P001
#force all of the metadata to reflect the correct dates

# ---------------------------------------------------------
# Site-level metadata corrections
# ---------------------------------------------------------

survival_raw <- survival_raw %>%
  mutate(
    `Outplant Date` = case_when(
      Site == "MS_P001" ~ as.Date("2026-05-31"),
      TRUE ~ `Outplant Date`
    ),
    
    `Monitoring Date` = case_when(
      Site == "MS_P001" ~ as.Date("2026-08-31"),
      TRUE ~ `Monitoring Date`
    )
  )
survival_raw %>%
  filter(Site == "MS_P001") %>%
  count(`Outplant Date`, `Monitoring Date`)

survival_raw %>%
  summarise(
    Missing_Outplant_Date = sum(is.na(`Outplant Date`)),
    Missing_Monitoring_Date = sum(is.na(`Monitoring Date`))
  )


# ---------------------------------------------------------
# Standardize Genotype IDs
#this will turn any IDs with lower case letters into upper case letters
# ---------------------------------------------------------

survival_raw <- survival_raw %>%
  mutate(
    `Genotype ID` = toupper(`Genotype ID`)
  )

# ---------------------------------------------------------
# Check for completely duplicated rows
# ---------------------------------------------------------

duplicate_rows <- survival_raw %>%
  filter(duplicated(.))

cat(
  "Completely duplicated rows:",
  nrow(duplicate_rows),
  "\n"
)

if (nrow(duplicate_rows) > 0) {
  print(duplicate_rows, n = Inf)
} else {
  cat("PASS: No completely duplicated rows found.\n")
}


# ---------------------------------------------------------
# 90-Day Data QC
#checking for any 90d measurements where the initial monitoring is missing
# ---------------------------------------------------------

cat("\n--- 90-Day Data QC ---\n")

missing_t0_with_90d <- survival_raw %>%
  filter(
    !is.na(`Area at 90 Day Monitoring`) |
      !is.na(`Number of Clusters at 90 Day Monitoring`) |
      !is.na(`Cluster Survival at 90 Day Monitoring`)
  ) %>%
  filter(
    is.na(`Area at Time of Outplanting`) |
      is.na(`Number of Clusters at Time of Outplanting`)
  )

cat(
  "90-day records missing required T0 measurements:",
  nrow(missing_t0_with_90d),
  "\n"
)

if (nrow(missing_t0_with_90d) == 0) {
  cat("PASS: All 90-day records have the required T0 measurements.\n")
} else {
  cat("CHECK: Some 90-day records are missing T0 measurements.\n")
  print(
    missing_t0_with_90d %>%
      select(
        Site,
        `Genotype ID`,
        `Area at Time of Outplanting`,
        `Area at 90 Day Monitoring`,
        `Number of Clusters at Time of Outplanting`,
        `Number of Clusters at 90 Day Monitoring`,
        `Cluster Survival at 90 Day Monitoring`
      ),
    n = Inf
  )
}


# ---------------------------------------------------------
# Biological Range QC
# ---------------------------------------------------------

cat("\n--- Biological Range QC ---\n")

# Survival should be between 0 and 100%
invalid_survival <- survival_raw %>%
  filter(
    !is.na(`Cluster Survival at 90 Day Monitoring`) &
      (
        `Cluster Survival at 90 Day Monitoring` < 0 |
          `Cluster Survival at 90 Day Monitoring` > 100
      )
  )

cat(
  "Invalid survival values:",
  nrow(invalid_survival),
  "\n"
)

# Area should not be negative
invalid_area <- survival_raw %>%
  filter(
    (!is.na(`Area at Time of Outplanting`) &
       `Area at Time of Outplanting` < 0) |
      (!is.na(`Area at 90 Day Monitoring`) &
         `Area at 90 Day Monitoring` < 0)
  )

cat(
  "Negative area values:",
  nrow(invalid_area),
  "\n"
)

# Cluster counts should not be negative
invalid_clusters <- survival_raw %>%
  filter(
    (!is.na(`Number of Clusters at Time of Outplanting`) &
       `Number of Clusters at Time of Outplanting` < 0) |
      (!is.na(`Number of Clusters at 90 Day Monitoring`) &
         `Number of Clusters at 90 Day Monitoring` < 0)
  )

cat(
  "Negative cluster counts:",
  nrow(invalid_clusters),
  "\n"
)

if (
  nrow(invalid_survival) == 0 &
  nrow(invalid_area) == 0 &
  nrow(invalid_clusters) == 0
) {
  cat("PASS: No biologically impossible values detected.\n")
}

# ---------------------------------------------------------
# Check for Inf / -Inf / NaN
# ---------------------------------------------------------

cat("\n--- Infinite / NaN Value QC ---\n")

infinite_values <- survival_raw %>%
  summarise(
    across(
      where(is.numeric),
      ~ sum(is.infinite(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Infinite_Values"
  ) %>%
  filter(Infinite_Values > 0)

nan_values <- survival_raw %>%
  summarise(
    across(
      where(is.numeric),
      ~ sum(is.nan(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "NaN_Values"
  ) %>%
  filter(NaN_Values > 0)

cat(
  "Variables containing Inf or -Inf:",
  nrow(infinite_values),
  "\n"
)

cat(
  "Variables containing NaN:",
  nrow(nan_values),
  "\n"
)

if (nrow(infinite_values) > 0) {
  print(infinite_values, n = Inf)
}

if (nrow(nan_values) > 0) {
  print(nan_values, n = Inf)
}

if (nrow(infinite_values) == 0 & nrow(nan_values) == 0) {
  cat("PASS: No Inf, -Inf, or NaN values detected.\n")
}

# ---------------------------------------------------------
#DATA CLEANING
# ---------------------------------------------------------

# ---------------------------------------------------------
# Clean invalid survival values
# ---------------------------------------------------------

invalid_survival_count <- sum(
  !is.na(survival_raw$`Cluster Survival at 90 Day Monitoring`) &
    (
      survival_raw$`Cluster Survival at 90 Day Monitoring` < 0 |
        survival_raw$`Cluster Survival at 90 Day Monitoring` > 100 |
        is.infinite(survival_raw$`Cluster Survival at 90 Day Monitoring`)
    )
)

cat(
  "Invalid survival values identified:",
  invalid_survival_count,
  "\n"
)

survival_raw <- survival_raw %>%
  mutate(
    `Cluster Survival at 90 Day Monitoring` = if_else(
      `Cluster Survival at 90 Day Monitoring` < 0 |
        `Cluster Survival at 90 Day Monitoring` > 100 |
        is.infinite(`Cluster Survival at 90 Day Monitoring`),
      NA_real_,
      `Cluster Survival at 90 Day Monitoring`
    )
  )

cat(
  "Invalid survival values converted to NA:",
  invalid_survival_count,
  "\n"
)

# ---------------------------------------------------------
# Identify Inf / -Inf values
# ---------------------------------------------------------

inf_locations <- survival_original %>%
  mutate(Row = row_number()) %>%
  summarise(
    across(
      where(is.numeric),
      ~ sum(is.infinite(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Inf_Count"
  ) %>%
  filter(Inf_Count > 0)

inf_locations

# Show the rows containing Inf values

survival_original %>%
  filter(
    if_any(
      where(is.numeric),
      is.infinite
    )
  ) %>%
  print(width = Inf)

# ---------------------------------------------------------
# Clean Inf / -Inf / NaN values
# ---------------------------------------------------------

survival_raw <- survival_raw %>%
  mutate(
    across(
      where(is.numeric),
      ~ if_else(
        is.infinite(.) | is.nan(.),
        NA_real_,
        .
      )
    )
  )
#verify that that worked
sum(
  sapply(
    survival_raw,
    function(x) any(is.infinite(x), na.rm = TRUE)
  )
)

# ---------------------------------------------------------
# Check species names
# ---------------------------------------------------------

survival_raw %>%
  count(Genus, Species, sort = TRUE)

# ---------------------------------------------------------
# Check sites and observations
# ---------------------------------------------------------

survival_raw %>%
  count(Site, sort = TRUE)

survival_raw %>%
  count(Site, Genus, Species, sort = TRUE) %>%
  print(n = Inf)

# ---------------------------------------------------------
# Check site names
# ---------------------------------------------------------

survival_raw %>%
  count(Site, sort = TRUE)
sort(unique(survival_raw$Site))


# ---------------------------------------------------------
# Save cleaned dataset
# ---------------------------------------------------------

write_csv(
  survival_clean,
  "data/processed/survival_clean.csv"
)

cat("\nCleaned dataset saved to data/processed/survival_clean.csv\n")

if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

write_csv(
  survival_raw,
  "data/processed/survival_clean.csv"
)

cat("\nCleaned dataset saved to data/processed/survival_clean.csv\n")
