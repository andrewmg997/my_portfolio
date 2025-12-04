# load necessary packages and libraries

install.packages("readxl")
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot_2")
install.packages("patchwork")
install.packages("mapview")

library(readxl)
library(dplyr)
library(janitor)
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapview)

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

p1 <- ggplot(billfish_summary, aes(x = year, y = avg_size_cm, color = species)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Billfish Size Over Time (Weighted by Number of Fish)",
    x = "Year",
    y = "Average Size (cm)",
    color = "Species"
  ) +
  theme_dark()


# excluded BIL because that could obfuscate the data
# used weighted_mean to find averages for each year

# save as .rds file

saveRDS(billfish_summary, "data/processed/billfish_summary.rds")


p2 <- ggplot(billfish_summary, aes(x = species, y = avg_size_cm, fill = species)) +
  geom_col() +
  labs(
    title = "Average Billfish Size by Species",
    x = "Species",
    y = "Average Size (cm)",
    fill = "Species"
  ) +
  theme_dark()

# create paneled figure using patchwork package

p1 / p2

# convert to sf object

billfish_sf <- st_as_sf(billfish_clean,
                        coords = c("lon", "lat"),
                        crs = 4326)

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white") +
  geom_sf(data = billfish_sf, aes(color = species), size = 2, alpha = 0.7) +
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "bl") +
  labs(
    title = "Billfish Sampling Locations",
    subtitle = "Showing locations of observed billfish species",
    x = "Longitude",
    y = "Latitude",
    color = "Species",
    caption = "Data source: Billfish assignment dataset"
  ) +
  theme_minimal()

mapview(billfish_sf, zcol = "species")
