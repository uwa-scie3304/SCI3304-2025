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
#setwd(here::here())

name <- "SCI3304-2025"

# Read in data----

# Metadata read
metadata <- readRDS(here::here(paste0("data/tidy/",
                                      name, "_Metadata.rds")))

#Read in habitat data

benthos <- read.csv((paste0("data/raw/", name, "_benthos.csv")))%>%
  dplyr::rename(sample = opcode)%>%
  dplyr::select(sample, number, longitude_dd, latitude_dd, date_time, location,
                site, depth_m, successful_count, successful_length,
                level_2, level_3, scientific)%>%
  dplyr::mutate(habitat = case_when(
      level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ "sand",
      level_2 %in% "Macroalgae" ~ "turf",
      level_2 %in% "Seagrasses" & scientific %in% "Posidonia spp" ~ "posidonia")) %>%
  dplyr::select(sample, habitat, number) %>%
  group_by(sample, habitat) %>%
  dplyr::tally(number, name = "number") %>%
  dplyr::mutate(total_points_annotated = sum(number)) %>%
  ungroup()%>%
  pivot_wider(names_from = "habitat", values_from = "number", values_fill = 0) %>%
  pivot_longer(cols = c("turf",
                        "posidonia",
                        "sand"),
               names_to = "habitat", values_to = "number")%>%
  glimpse()


# Read in Relief

relief <- read.csv((paste0("data/raw/", name, "_relief.csv")))%>%
  dplyr::rename(sample = opcode)%>%
  uncount(number) %>%
  group_by(sample) %>%
  dplyr::summarise(mean.relief = mean(as.numeric(level_5)),
                   sd.relief = sd(as.numeric(level_5), na.rm = T)) %>%
  ungroup() %>%
  glimpse()

# Format habitat and relief for plotting and modelling

tidy.habitat <- metadata  %>%
  left_join(benthos)%>%
  left_join(relief) %>%
  dplyr::mutate(longitude_dd = as.numeric(longitude_dd),
                latitude_dd = as.numeric(latitude_dd)) %>%
  clean_names() %>%
  glimpse()


#save as RDS file
saveRDS(tidy.habitat, file = here::here(paste0("./data/tidy/", name, "_tidy.habitat.rds")))

#plot occurrence of habitat - each dot represents a sample
plot.habitat <- tidy.habitat %>%
  group_by(sample, habitat) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  glimpse()


ggplot() +
  geom_quasirandom(data = plot.habitat,
                   aes(x = number, y = habitat), #changed x from total_annotated_points
                   groupOnX = F, method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.2) +
  labs(x = "Number of points", y = "") +
  theme_classic()

# plotting relief

plot.relief <- read.csv((paste0("data/raw/", name, "_relief.csv")))%>%
  dplyr::rename(sample = opcode)%>%
  group_by(sample, level_5) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  dplyr::mutate(class.relief = as.factor(level_5)) %>%
  glimpse()


ggplot() +
  geom_quasirandom(data = plot.relief,
                   aes(x = number, y = class.relief),
                   groupOnX = F,
                   method = "quasirandom",
                   alpha = 0.25, size = 1.8, width = 0.05) +
  labs(x = "Number of points", y = "Relief (0-5)") +
  theme_classic()


# changing occurrance to percentage cover of habitat
habitat <- readRDS("./data/tidy/SCI3304-2025_tidy.habitat.rds")%>%
  dplyr::group_by(sample, habitat, total_points_annotated) %>%
  dplyr::mutate(percentage = (number / total_points_annotated) * 100) %>%
  ungroup()%>%
  dplyr::select(sample, habitat, percentage)%>%
  pivot_wider(names_from = "habitat", values_from = "percentage", values_fill = 0)%>%
  ungroup()%>%
  left_join(metadata)%>%
  left_join(relief)%>%
  glimpse()

saveRDS(habitat, file = here::here(paste0("./data/tidy/", name, "_habitat.rds")))
