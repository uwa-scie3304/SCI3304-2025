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

# Set your working directory to the project's directory
setwd(here::here())

# Set the study name (e.g. campaignID) - this way your code is reuseable for a different stereo-video campaign
name <- "SCI3304-2025"


# Load and format metadata aka the labsheet----
metadata <- read.csv(here("data/raw/SCI3304-2025-BRUVs.csv")) %>%
  clean_names() %>%
  dplyr::filter(campaignid %in% "2020-10_south-west_stereo-BRUVs", ##HAN CHECK
                successful_count %in% "Yes") %>%
  glimpse()


# Load the count data
count <- read.csv(paste0("data/raw/", name, "_count.csv")) %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(campaignid, sample, family, genus, species, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  glimpse()



# Make cumulative MaxN - to visualise the most abundant species
top_species <- count %>%
  group_by(scientific) %>%
  summarise(sum_maxn = sum(count)) %>%
  arrange(sum_maxn) %>%
  glimpse()


# Check the data----

# Plot the most abundant species using ggplot
ggplot(data = top_species, aes(x = reorder(scientific, sum_maxn),
                               y = sum_maxn)) +
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey",
           position = position_dodge()) +
  coord_flip() +
  labs(x = expression(Overall~abundance~(Sigma~MaxN)), y = "Species") +
  theme_classic()



# Format for plotting and modeling----
# Format the data to a format suitable to use in modelling scripts
# This needs to be long format data, with one line per deployment/opcode
# In this example we are just interested in the abundance of King George Whiting, so the data is filtered to this species, and a character column added (this will make sense when you run the modelling)

kgw <- count %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus") %>%
  dplyr::mutate(response = "Abundance of KGW") %>%
  glimpse()

# Save the count data
saveRDS(kgw, file = paste0("data/staging/", name, "_tidy-count.rds"))


# Bring in size of maturity----
# Load size of maturity cut off's data - this is loaded by the CheckEM package
maturity_mean <- maturity %>%
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(Lm = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()


# Load length annotation data and join with size of maturity dataset
length <- read.csv(paste0("data/raw/fish/", name, "_length.csv")) %>%
  dplyr::filter(successful_length %in% "Yes") %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  left_join(maturity_mean) %>%
  glimpse()

# Create a 'metadata' file for lengths - this is a list of all the unique deployments/opcodes, so that the 0s can be added back in
metadata_length <- length %>%
  distinct(campaignid, opcode, latitude_dd, longitude_dd, depth_m, status)


# Create abundance by size class data----

# Create a dataframe for the abundance of greater than size of maturity King George Whiting
mature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Mature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than size of maturity King George Whiting
immature_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < Lm) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Immature KGW") %>%
  glimpse()

# Create a dataframe for the abundance of greater than legal size King George Whiting
legal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm > 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Legal KGW") %>%
  glimpse()

# Create a dataframe for the abundance of smaller than legal size King George Whiting
sublegal_kgw <- length %>%
  dplyr::filter(scientific %in% "Sillaginodes punctatus", length_mm < 280) %>%
  group_by(campaignid, opcode) %>%
  summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "Sublegal KGW") %>%
  glimpse()

# Join the datasets
tidy_length <- bind_rows(mature_kgw, immature_kgw, legal_kgw, sublegal_kgw) %>%
  glimpse()


# Save the length data----
saveRDS(tidy_length, file = paste0("data/staging/", name, "_tidy-length.rds"))


# Extra ways to check the data----
# Visualise the length distributions
# Manually added on minimum legal size and length of maturity (Lm)
library(png)

kgw <- as.raster(readPNG("data/images/Sillaginodes_punctatus_nb_TAYLOR.png"))

ggplot(filter(length, scientific %in% "Sillaginodes punctatus"), aes(length_mm)) +
  geom_histogram()+
  # geom_density(fill = "grey40", alpha = 0.3) +
  # Or think about another way to show this data? histogram?
  labs(title = "King George Whiting length distribution") +
  geom_vline(xintercept = 280, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = 410, colour = "blue", linetype = "dashed") +
  annotation_raster(kgw, 100, 200, 0.0075, 0.001) +
  annotate(geom = "text", x = c(280, 410), y = 0.01, label = c("Minimum legal size", "Lm")) +
  theme_classic()

