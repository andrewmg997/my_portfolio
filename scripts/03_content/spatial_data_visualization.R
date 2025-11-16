# load necessary packages and libraries

install.packages("mapview")
install.packages("viridis")

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
library(viridis)

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

# create spatial sf polygon object

world <- ne_countries(scale = "medium", returnclass = "sf")

# plot bill fish locations onto map

billfish_locations <- ggplot() +
  geom_sf(data = world, fill = "grey20", color = "white") +
  geom_sf(data = billfish_sf, aes(color = species), size = .75) +
   coord_sf(xlim = c(-170, -50), ylim = c(-30, 40),
    expand = FALSE) +     # cropped map to appropriate size
  annotation_north_arrow(location = "tr", which_north = "true") +
  annotation_scale(location = "bl") +
  labs(
    title = "Billfish Sampling Locations",
    subtitle = "Locations of observed billfish species",
    x = "Longitude",
    y = "Latitude",
    color = "Species") +
  theme_minimal()

mapview(billfish_sf, zcol = "species") # make an interactive map
                                       # for easier navigation of data

species <- c("BIL","BLM","BUM","MLS","SFA","SSP","SWO")

species_colors <- viridis(7)    # give each species distinct color
names(species_colors) <- c("BIL","BLM","BUM","MLS","SFA","SSP","SWO")

layer_list <- lapply(species, function(sp) {  # allow for toggling between points
  mapview(
    billfish_sf |> filter(species == sp),
    layer.name = sp,
    col.regions = species_colors[sp],
    zcol = "species")})

Reduce(`+`, layer_list)

# now there is a static plot and a map that can be viewed and
# interacted with for more convenient viewing of spatial data points

ggsave(filename = "results/img/billfish_map.png",
  plot = billfish_locations) # save ggplot image as png file
