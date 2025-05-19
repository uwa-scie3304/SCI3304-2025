###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Format and visualise fish data
# Author:  Claude Spencer & edited by Hannah Williams
# Date:    May 2024, edited May 2025
##

# Clear objects from your environment
rm(list = ls())

# Load libraries and install the CheckEM package (only need to install once)----
# library(devtools)
# devtools::install_github("GlobalArchiveManual/CheckEM") # Use this to install the CheckEM package if you have not already done so
library(CheckEM)
library(tidyverse)
library(ggplot2)
library(sf)
library(here)
library(leaflet)

# Set your working directory - shouldn't have to do this if you're in your SCI3304-2025 project
# setwd(here::here())

# Set the study name (e.g. campaignID) -
# this way your code is reuseable for a different stereo-video campaign
name <- "SCI3304-2025"


# Load and format metadata aka the labsheet----
metadata <- read.csv(here("data/raw/SCI3304-2025_Metadata.csv")) %>%
  clean_names() %>%
  dplyr::select(sample,longitude_dd, latitude_dd, date_time, location,
                site, depth_m, successful_count, successful_length,
                successful_habitat_forward)%>%
  dplyr::filter(successful_count %in% "Yes") %>%
  glimpse()

#save metadata as an RDS as its a lighter file than .csv
saveRDS(metadata, file = here::here(paste0("data/tidy/",
                                           name, "_Metadata.rds")))

# Load the count data
complete.count <- read.csv(paste0("data/raw/", name, "_count.csv")) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  mutate(species = if_else(species %in% "novaezelandiae", "spp", species))%>%
  mutate(genus = if_else(genus %in% "Unknown" & family %in% "Monacanthidae", "Acanthaluteres", genus))%>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  group_by(campaignid, opcode, family, genus, species, scientific)%>%
  summarise(count = max(count))%>%
  ungroup()%>%
  rename(sample = opcode)%>%
  glimpse()

unique(complete.count$scientific)

length(unique(complete.count$scientific)) #checking for no. of unique species seen
is.na(unique(complete.count$scientific)) #checking for NAs in species
which(is.na(complete.count$count)) #checking for NAs in count (MaxN)

saveRDS(complete.count, file = here::here(paste0("data/tidy/",
                                           name, "_complete.count.rds")))

#load lengths

complete.length <- read.csv(paste0("data/raw/", name, "_length.csv"))%>%
  dplyr::filter(successful_length %in% "Yes") %>%
  mutate(species = if_else(species %in% "novaezelandiae", "spp", species))%>%
  mutate(genus = if_else(genus %in% "Unknown" & family %in% "Monacanthidae", "Acanthaluteres", genus))%>%
  mutate(scientific = paste(family, genus, species, sep = " ")) %>%
  rename(sample = opcode)%>%
  glimpse()


saveRDS(complete.length, file = here::here(paste0("data/tidy/",
                                                 name, "_complete.length.rds")))
#adding total abundance & species richness

ta.sr <-  complete.count %>%
  select(-c(family, genus, species))%>%
  pivot_wider(names_from = "scientific", values_from = count, values_fill = 0) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(total_abundance = rowSums(.[, 6:(ncol(.))], na.rm = TRUE),
                species_richness = rowSums(.[, 6:(ncol(.))] > 0)) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"),
               names_to = "response", values_to = "number") %>%
  glimpse()

saveRDS(ta.sr, file = here::here(paste0("data/tidy/",
                                                  name, "_ta.sr.rds")))
