# load necessary packages and libraries

install.packages("readxl")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot_2")

library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(readr)
library(ggplot2)

# read raw data

billfish_raw <- read_csv("data/raw/BillfishData.csv")

# wrangle data; rename columns

spec(billfish_raw)

billfish_clean <- billfish_raw |>
  rename(
    year = Year,
    month = Month,
    species = Species,
    lat = LatC1,
    lon = LonC1,
    size_cm = SizeCm,
    num_fish = NumFish)

# find the weighted average of each species by year

billfish_summary <- billfish_clean |>
  filter(species != "BIL") |>              # remove BIL
  group_by(year, species) |>               # group by year and species
  summarise(
    avg_size_cm = weighted.mean(size_cm, num_fish, na.rm = TRUE),
    total_fish = sum(num_fish, na.rm = TRUE),
    .groups = "drop"
  )

# plot data

ggplot(billfish_summary, aes(x = year, y = avg_size_cm, color = species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Billfish Size Over Time (Weighted by Number of Fish)",
    x = "Year",
    y = "Average Size (cm)",
    color = "Species"
  ) +
  theme_minimal()


# excluded BIL because that could obfuscate the data
# used weighted_mean to find averages for each year

# save as .rds file

saveRDS(billfish_summary, "data/processed/billfish_summary.rds")

