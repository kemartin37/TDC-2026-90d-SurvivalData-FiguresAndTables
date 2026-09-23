# ============================================================
# TDC 2026 - 90D Survival Data: Figures & Tables
# Setup script
# ============================================================

# Install/load packages ---------------------------------------

packages <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "here"
)

installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(tidyverse)
library(readxl)
library(janitor)
library(here)


# Project folders ---------------------------------------------

dir.create(here("data", "raw"), 
           recursive = TRUE, 
           showWarnings = FALSE)

dir.create(here("data", "processed"), 
           recursive = TRUE, 
           showWarnings = FALSE)

dir.create(here("outputs"), 
           recursive = TRUE, 
           showWarnings = FALSE)


# Check project location --------------------------------------

here()

# Load raw data ------------------------------------------------

raw_data <- read_csv(
  here("data", "raw", "dataForExcel.csv")
)

# Check the data
glimpse(raw_data)