###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Plot importance scores
# Author:  Claude Spencer
# Date:    May 2024
##

# Clear objects from your environment
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyverse)
library(patchwork)

# Set your working directory to the project's directory
setwd(here::here())

# Set the study name
name <- 'SCI3304-2025'

# Read in the data
dat <- read.csv(paste("model out", paste(name, "all.var.imp.csv", sep = "_"), sep = "/")) %>%
  dplyr::rename(resp.var = X) %>%
  pivot_longer(cols = -c(resp.var), names_to = "predictor", values_to = "importance") %>%
  glimpse()

dat.taxa <- dat %>%
  dplyr::mutate(label = case_when(predictor %in% "Immature KGW" & resp.var %in% "Unconsolidated" ~ "X",
                                  .default = NA)) %>%
  glimpse()

ggplot(dat.taxa, aes(x = predictor, y = resp.var, fill = importance)) +
  geom_tile(show.legend = T) +
  scale_fill_gradient(low = "white", high = "red2", limits = c(0, 1), name = "Importance") +
  labs(x = NULL, y = NULL) +
  theme_classic()

#save output - changed dimensions for larger text in report
ggsave(paste0("plots/", name, "_importance-scores.png"), height = 4, width = 6.275, dpi = 300)
