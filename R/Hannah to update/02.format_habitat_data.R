###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise habitat data
# Author:  Claude Spencer
# Date:    May 2024
##

# Clear objects from your environment
rm(list = ls())


# Set up----

# Load libraries and install the CheckEM package (only need to install once)
# library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # Use this to install the CheckEM package if you have not already done so
library(CheckEM)
library(tidyverse)
library(ggbeeswarm)
library(scatterpie)



# Set your working directory to the project's directory
setwd(here::here())

name <- "SCI3304-2025"

# Read in data----

# Metadata read
metadata <- read.csv("data/raw/SCI3304-2025-BRUVs_Metadata.csv") %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  dplyr::mutate(opcode = as.character(opcode)) %>%
  glimpse()


# Load and format habitat data
habitat <- read.delim("data/raw/Annotation_1_Dot Point Measurements.txt",
                      skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, broad) %>%
  dplyr::filter(!broad %in% c("Unknown", NA, "")) %>%
  dplyr::mutate(number = 1) %>%
  group_by(campaignid, opcode, broad) %>%
  summarise(number = sum(number)) %>%
  pivot_wider(names_from = "broad", values_from = "number", values_fill = 0) %>%
  clean_names() %>%
  ungroup() %>%
  dplyr::mutate(total_points_annotated = rowSums(.[, 3:ncol(.)])) %>%
  glimpse()

#check habitat data & metadata to make sure we have the expected number of rows
nrow(metadata)

nrow(habitat)

#check if any mismatches between metadata and habitat
check_habitat <- anti_join(metadata, habitat) %>%
  glimpse()

#and the other way with habitat and metadata flipped
check_metadata <- anti_join(habitat, metadata) %>%
  glimpse()


# Load and format the relief data----
relief <- read.delim("data/raw/habitat/Relief_Dot Point Measurements.txt",
                     skip = 4, stringsAsFactors = F, colClasses = "character") %>%
  clean_names() %>%
  dplyr::select(campaignid, opcode, level_5) %>%
  dplyr::filter(!level_5 %in% c(NA, "")) %>%
  dplyr::mutate(level_5 = as.numeric(level_5)) %>%
  group_by(campaignid, opcode) %>%
  summarise(mean_relief = mean(level_5),
            sd_relief = sd(level_5)) %>%
  ungroup() %>%
  glimpse()

# Some basic checks
relief_missing_metadata <- relief %>%
  anti_join(metadata) %>%
  glimpse()

metadata_missing_relief <- metadata %>%
  anti_join(relief) %>%
  glimpse()

# Join the two datasets----
tidy_habitat <- habitat %>%
  left_join(relief) %>%
  glimpse()

# Save the final tidy dataset----
saveRDS(tidy_habitat, file = paste0("data/tidy/", name, "_tidy-habitat.rds"))




# Some extra plots to check Metadata to habitat points and habitat point to Metadata----
# Same as CheckEM?
#    - claude to add one or two bascis plots
#   bar and plot by deooth and scatter ies? and bubble?

# Transform the data back into long format - ggplot needs data this way
plot_data <- habitat %>%
  pivot_longer(cols = c(macroalgae, unconsolidated, seagrasses, sponges), names_to = "habitat",
               values_to = "count") %>%
  glimpse()

# Nicely jittered plot that only moves points along the y axis (eg it doesn't move the actual 'count' values)
# Keep in mind that this data doesn't factor the total number of points annotated - eg the open water points removed
ggplot() +
  geom_quasirandom(data = plot_data, aes(x = count, y = habitat), groupOnX = F, method = "quasirandom", alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()

# Spatial bubble plot
# Join the data with the metadata to plot spatially
habitat_metadata <- habitat %>%
  left_join(metadata) %>%
  dplyr::filter(!is.na(longitude_dd)) %>% # 113 is missing metadata
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = F) %>%
  glimpse()

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

# Another basic plot to make nice
# See package 'ggspatial' for scalebars and north arrows
ggplot() +
  geom_sf(data = aus) +
  geom_sf(data = habitat_metadata, aes(size = macroalgae/total_points_annotated),
          alpha = 0.5, colour = "darkblue") +
  coord_sf(xlim = c(117.839435, 117.949606),
           ylim = c(-35.007973, -35.091606),
           crs = 4326) +
  theme_minimal()

# To save plots, see
?ggplot2::ggsave

# Spatial pie charts - scatterpies

ggplot() +
  geom_sf(data = aus) +
  geom_scatterpie(data = as.data.frame(habitat_metadata), # Doesn't work with sf dataframes
                  aes(x = longitude_dd, y = latitude_dd),
                  cols = c("macroalgae", "unconsolidated", "seagrasses", "sponges"),
                  pie_scale = 1.5) +
  coord_sf(xlim = c(117.839435, 117.949606),
           ylim = c(-35.007973, -35.091606),
           crs = 4326) +
  theme_minimal()

# Add some nice colours, see
?ggplot2::scale_fill_manual
