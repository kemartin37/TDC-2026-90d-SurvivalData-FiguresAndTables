# ============================================================
# 03_summary_tables.R
# TDC 2026 - 90 Day Survival Data
# Summary tables and statistics
# ============================================================

# Load packages and project setup
source("scripts/01_setup.R")
library(gt)

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
# ============================================================
# 90-day survival summary by reef
# ============================================================

survival_by_reef <- survival_clean %>%
  mutate(
    Reef = str_remove(Site, "_P\\d+$"),
    Reef = case_when(
      Reef == "MS" ~ "Maryland Shoal",
      Reef == "PS" ~ "Pelican Shoal",
      Reef == "WS" ~ "Western Sambo",
      TRUE ~ Reef
    )
  ) %>%
  group_by(Reef) %>%
  summarise(
    `Total Fragments Outplanted` = first(
      `Total # of Corals Outplanted`
    ),
    `Coral Clusters Alive at Outplant` = sum(
      `Number of Clusters at Time of Outplanting`,
      na.rm = TRUE
    ),
    `Coral Clusters Alive at 90D` = sum(
      `Number of Clusters at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    `Percentage Survival (%)` = mean(
      `Cluster Survival at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(Reef)

survival_by_reef



reef_table <- survival_by_reef %>%
  gt() %>%
  tab_header(
    title = "90-Day Coral Survival by Reef",
    subtitle = "TDC 2026"
  ) %>%
  fmt_number(
    columns = c(
      `Coral Clusters Alive at Outplant`,
      `Coral Clusters Alive at 90D`
    ),
    decimals = 0
  ) %>%
  fmt_number(
    columns = `Percentage Survival (%)`,
    decimals = 1,
    suffix = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_options(
    table.font.size = px(16),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(15),
    column_labels.font.size = px(15),
    data_row.padding = px(8)
  )

reef_table


gtsave(reef_table, "outputs/reef_survival_summary.png")
gtsave(reef_table, "outputs/reef_survival_summary.pdf")

# ============================================================
# 90-day survival summary by species
# All plots combined
# ============================================================

survival_by_species <- survival_clean %>%
  group_by(Genus, Species) %>%
  summarise(
    `Coral Clusters At Outplant` = sum(
      `Number of Clusters at Time of Outplanting`,
      na.rm = TRUE
    ),
    `Coral Clusters at 90d` = sum(
      `Number of Clusters at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    `Percentage Survival (%)` = mean(
      `Cluster Survival at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Species = paste(Genus, Species)
  ) %>%
  select(
    Species,
    `Coral Clusters At Outplant`,
    `Coral Clusters at 90d`,
    `Percentage Survival (%)`
  ) %>%
  arrange(Species)

survival_by_species


# ============================================================
# Format species summary table
# ============================================================

species_table <- survival_by_species %>%
  gt() %>%
  tab_header(
    title = "90-Day Coral Survival by Species",
    subtitle = "TDC 2026 — All Plots Combined"
  ) %>%
  fmt_number(
    columns = c(
      `Coral Clusters At Outplant`,
      `Coral Clusters at 90d`
    ),
    decimals = 0
  ) %>%
  fmt_number(
    columns = `Percentage Survival (%)`,
    decimals = 1,
    suffix = "%"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_options(
    table.font.size = px(16),
    heading.title.font.size = px(22),
    heading.subtitle.font.size = px(15),
    column_labels.font.size = px(15),
    data_row.padding = px(8)
  )

species_table
