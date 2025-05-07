###
# Project: SCIE3304 - BRUV Analysis
# Data:    Example analysis
# Task:    Format and visualise fish data
# Author:  Claude Spencer
# Date:    March 2025
##

# install.packages('remotes')
library('remotes')
options(timeout = 9999999)

# remotes::install_github("GlobalArchiveManual/CheckEM")
library(CheckEM)
library(tidyverse)
library(leaflet)
library(pscl)
library(here)


##Load metadata

metadata <- read.csv(here("data/tidy/south-west-corner-marine-park-western-arm_metadata.csv")) %>%
  clean_names() %>%
  dplyr::filter(campaignid %in% "2020-10_south-west_stereo-BRUVs",
                successful_count %in% "Yes") %>%
  glimpse()

#load count data
count <- read.csv(here("data/tidy/south-west-corner-marine-park-western-arm_count.csv")) %>%
  clean_names() %>%
  dplyr::filter(campaignid %in% "2020-10_south-west_stereo-BRUVs") %>%
  group_by(campaignid, sample, family, genus, species, scientific) %>%
  dplyr::summarise(count = sum(count)) %>%
  ungroup() %>%
  glimpse()

#load habitat data
habitat <- read.csv(here("data/tidy/south-west-corner-marine-park-western-arm_habitat.csv")) %>%
  clean_names() %>%
  glimpse()

#check habitat and metadata - make sure rows even and expected
nrow(metadata)

nrow(habitat)

#checking there arent any mismatched
check_habitat <- anti_join(metadata, habitat) %>%
  glimpse()

#then flipping to check the other way
check_metadata <- anti_join(habitat, metadata) %>%
  glimpse()

##choose a species of interest
maoriwrasse <- count %>%
  dplyr::filter(scientific %in% "Labridae Ophthalmolepis lineolatus") %>%
  glimpse()

## adding in zeros where no maoriwrasse were seen
maoriwrasse_w_zeros <- maoriwrasse %>%
  right_join(metadata) %>%
  left_join(habitat) %>%
  dplyr::mutate(count = if_else(is.na(count), 0, count)) %>%
  glimpse()

#check the data again
summary(maoriwrasse_w_zeros$count)

summary(maoriwrasse_w_zeros$reef)

summary(maoriwrasse_w_zeros$mean_relief)

summary(maoriwrasse_w_zeros$depth_m)


## VISUALISING THE DATA
#create interactive plot for count
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maoriwrasse_w_zeros$count)

leaflet(maoriwrasse_w_zeros) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~count,
                   color = ~pal(count),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.8) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ count,
            title = "Count",
            opacity = 0.8)

#create interactive plot for depth
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maoriwrasse_w_zeros$depth_m)

leaflet(maoriwrasse_w_zeros) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~5,
                   color = ~pal(depth_m),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ depth_m,
            title = "Count",
            opacity = 0.8)
#create interactive plot for percentage cover of reef
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maoriwrasse_w_zeros$reef)

leaflet(maoriwrasse_w_zeros) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~5,
                   color = ~pal(reef),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ reef,
            title = "Count",
            opacity = 0.8)

#create interactive plot with mean relief
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = maoriwrasse_w_zeros$mean_relief)

leaflet(maoriwrasse_w_zeros) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~5,
                   color = ~pal(mean_relief),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ mean_relief,
            title = "Count",
            opacity = 0.8)

## Checking Predictor Variables and running a GLM
#plot correlation between variables
ggplot() +
  geom_point(data = maoriwrasse_w_zeros, aes(x = mean_relief, y = reef))

#check the correlation values between variables
cor(maoriwrasse_w_zeros$mean_relief, maoriwrasse_w_zeros$reef)

cor(maoriwrasse_w_zeros$reef, maoriwrasse_w_zeros$depth_m)

#visualise data distribution to determine distribution family in model
hist(maoriwrasse_w_zeros$count) #lots of zeros so we use poisson

## RUNNING THE MODEL
mod <- zeroinfl(count ~ depth_m + reef, data = maoriwrasse_w_zeros,
                dist = "poisson")
AIC(mod)

summary(mod)
