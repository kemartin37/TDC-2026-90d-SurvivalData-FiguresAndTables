# ============================================================
# 04_figures.R
# TDC 2026 - 90 Day Survival Figures
# ============================================================

# Load packages and project setup
source("scripts/01_setup.R")
library(ggplot2)

# Load cleaned data
survival_clean <- read_csv("data/processed/survival_clean.csv")

# Create output folder if needed
if (!dir.exists("outputs/figures")) {
  dir.create("outputs/figures")
}


# ============================================================
# Figure 1: 90-day survival by reef
# ============================================================

reef_survival_plot <- survival_by_reef %>%
  ggplot(
    aes(
      x = Reef,
      y = `Percentage Survival (%)`
    )
  ) +
  geom_col(
    aes(fill = Reef)
  )+
  scale_fill_manual(
    values = c(
      "Maryland Shoal" = "#006D77",
      "Pelican Shoal" = "#0077B6",
      "Western Sambo" = "#3A86B8",
      "Reef 4" = "#457B9D",
      "Reef 5" = "#264653"
  ) 
  )+
  geom_text(
    aes(
      label = paste0(round(`Percentage Survival (%)`, 1), "%")
    ),
    vjust = -0.4,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "90-Day Coral Survival by Reef",
    subtitle = "TDC 2026",
    x = "Reef",
    y = "Mean Survival (%)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      size = 14
    )
  )

reef_survival_plot

ggsave("outputs/figures/reef_survival_summary.png", reef_survival_plot, width = 10, height = 7, dpi = 300)

# ============================================================
# Figure 2: 90-day survival by species
# ============================================================

species_survival_plot <- survival_by_species %>%
  ggplot(
    aes(
      x = reorder(Species, `Percentage Survival (%)`),
      y = `Percentage Survival (%)`
    )
  ) +
  geom_col(
    fill = "#2A9D8F"
  ) +
  geom_text(
    aes(
      label = paste0(round(`Percentage Survival (%)`, 1), "%")
    ),
    hjust = -0.15,
    size = 4
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "90-Day Coral Survival by Species",
    subtitle = "TDC 2026 — All Plots Combined",
    x = NULL,
    y = "Mean Survival (%)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text = element_text(
      size = 13
    )
  )

species_survival_plot

ggsave("outputs/figures/species_survival_summary.png", species_survival_plot, width = 11, height = 9, dpi = 300)


# ============================================================
# 90-day survival by species and reef
# ============================================================

survival_species_reef <- survival_clean %>%
  mutate(
    Reef = str_remove(Site, "_P\\d+$"),
    Reef = case_when(
      Reef == "MS" ~ "Maryland Shoal",
      Reef == "PS" ~ "Pelican Shoal",
      Reef == "WS" ~ "Western Sambo",
      TRUE ~ Reef
    )
  ) %>%
  group_by(Reef, Genus, Species) %>%
  summarise(
    `Percentage Survival (%)` = mean(
      `Cluster Survival at 90 Day Monitoring`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Species = paste0(
      str_sub(Genus, 1, 1),
      ". ",
      Species
    )
  )

species_reef_heatmap <- survival_species_reef %>%
  ggplot(
    aes(
      x = Reef,
      y = Species,
      fill = `Percentage Survival (%)`
    )
  ) +
  geom_tile(
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_gradient(
    low = "#D9EEF2",
    high = "#006D77",
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    na.value = "white"
  ) +
  labs(
    title = "90-Day Coral Survival by Species and Reef",
    subtitle = "TDC 2026",
    x = "Reef",
    y = NULL,
    fill = "Mean Survival (%)"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 13
    ),
    axis.text.y = element_text(
      size = 12,
      face = "italic"
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

species_reef_heatmap
