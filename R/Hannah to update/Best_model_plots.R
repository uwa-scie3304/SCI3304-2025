# How to make prettplots
# More examples at https://globalarchivemanual.github.io/CheckEM/articles/r-workflows/fish-predictions.html


library(CheckEM)
library(tidyverse)
library(mgcv)
library(devtools)
library(FSSgam)
library(here)
library(ggplot2)
library(ggnewscale)
library(viridis)
library(terra)
library(sf)
library(patchwork)

name <- 'SCI3304-2025'

dat <- readRDS(here::here(paste0('data/tidy/',
                                 name,'_tidy-length.rds'))) %>%
  dplyr::filter(!is.na(macroalgae),
                !is.na(mean_relief)) %>%
  glimpse()

dat_immature <- dat %>%
  dplyr::filter(response %in% "Immature KGW") %>%
  glimpse()

mod <- gam(number ~ s(unconsolidated, k = 3, bs = 'cr'), family = tw(), data = dat_immature)
summary(mod)

testdata <- expand.grid(unconsolidated = seq(min(dat$unconsolidated), max(dat$unconsolidated),length.out = 20)) %>%
  distinct() %>%
  glimpse()

fits <- predict.gam(mod, newdata = testdata, type = 'response', se.fit = T)

predicts_immature_unconsolidated <- testdata %>%
  data.frame(fits) %>%
  group_by(unconsolidated) %>% # Only change here
  summarise(number = mean(fit), se.fit = mean(se.fit)) %>%
  ungroup()

ggplot() +
  geom_point(data = dat_immature, aes(x = unconsolidated, y = number),  alpha = 0.5, size = 1, show.legend = F) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = unconsolidated, y = number), alpha = 0.5) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = unconsolidated, y = number - se.fit), linetype = "dashed", alpha = 0.5) +
  geom_line(data = predicts_immature_unconsolidated, aes(x = unconsolidated, y = number + se.fit), linetype = "dashed", alpha = 0.5) +
  theme_classic() +
  labs(x = "Sand", y = "", title = "Immature KGW") +
  theme(plot.title = element_text(hjust = 0))
