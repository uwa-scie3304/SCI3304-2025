###
# Project: SCIE3304 - BRUV Analysis
# Data:    Example analysis
# Task:    Format and visualise fish data
# Author:  Claude Spencer
# Date:    March 2025
##


# Clear objects from your environment
rm(list = ls())

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

metadata <- readRDS("./data/tidy/SCI3304-2025_Metadata.rds") %>%
  dplyr::filter(successful_count %in% "Yes") %>%
  glimpse()

#load count data
count <- readRDS("./data/tidy/SCI3304-2025_complete.count.rds") %>%
  glimpse()

#load habitat data
habitat <- readRDS("./data/tidy/SCI3304-2025_habitat.rds") %>%
  glimpse()


ta.sr <- readRDS("./data/tidy/SCI3304-2025_ta.sr.rds")%>%
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
total.abund <- ta.sr %>%
  dplyr::filter(response %in% "total_abundance") %>%
  glimpse()

## join to habitat
total.abund.w.habitat <- total.abund %>%
  left_join(habitat) %>%
  filter(!sample == c("S7"))%>% #no habitat data
  glimpse()

#check the data again
summary(total.abund.w.habitat$number)

summary(total.abund.w.habitat$sand)

summary(total.abund.w.habitat$posidonia)

summary(total.abund.w.habitat$turf)

summary(total.abund.w.habitat$mean.relief)

summary(total.abund.w.habitat$depth_m)


## VISUALISING THE DATA
#create interactive plot for count
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = total.abund.w.habitat$number)

leaflet(total.abund.w.habitat) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~number/5, #divided to make circles less huge
                   color = ~pal(number),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.8) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ number,
            title = "Total Abundance",
            opacity = 0.8)

#create interactive plot for depth
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = total.abund.w.habitat$depth_m)

leaflet(total.abund.w.habitat) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~5,
                   color = ~pal(depth_m),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ depth_m,
            title = "Depth",
            opacity = 0.8)
#create interactive plot for percentage cover of sand
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = total.abund.w.habitat$sand)

leaflet(total.abund.w.habitat) %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~longitude_dd, ~latitude_dd, radius = ~5,
                   color = ~pal(sand),
                   stroke = FALSE, label = ~as.character(sample),
                   fillOpacity = 0.6) %>%
  addLegend("bottomright",
            pal = pal,
            values = ~ sand,
            title = "%sand",
            opacity = 0.8)

#try creating interactive plot with mean relief, %posidonia, and %turf

## Checking Predictor Variables and running a GLM
#plot correlation between variables
ggplot() +
  geom_point(data = total.abund.w.habitat, aes(x = posidonia, y = turf))

#check the correlation values between variables, try the other combinations yourselves
cor(total.abund.w.habitat$posidonia, total.abund.w.habitat$turf)

#visualise data distribution to determine distribution family in model
hist(total.abund.w.habitat$number) #lots of zeros so we use poisson

## RUNNING THE MODEL
mod1 <- zeroinfl(number ~ depth_m, data = total.abund.w.habitat,
                dist = "poisson")
AIC(mod1)

summary(mod1)

mod2 <- zeroinfl(number ~ depth_m + sand, data = total.abund.w.habitat,
                 dist = "poisson")
AIC(mod1,mod2)

summary(mod2)

# with site as a factor
total.abund.w.habitat <- total.abund.w.habitat %>%
  dplyr::mutate(site = as.factor(site))%>%
  glimpse()

mixmod <- glm(number ~ depth_m + site, data=total.abund.w.habitat,
              family = poisson)

summary(mixmod)

# random effect model (where you want to include a random effect to control for it
# but you don't necessarily want to actually test it as a covariate)

#install.packages(lme4)

library(lme4)
remod <- glmer(number ~ depth_m + (1|site), #(1|site) is specifying random effect
                data=total.abund.w.habitat,
                family = poisson)
summary(remod)

#you might also want to include date as a random effect to control
#for differences in weather

total.abund.w.habitat <- total.abund.w.habitat  %>%
  dplyr::mutate(date = substr(date_time, 1, 10))%>%
  dplyr::mutate(time = substr(date_time, 12, 19))%>%
  dplyr::mutate(date = as.factor(date))%>%
  glimpse()

remod2 <- glmer(number ~ depth_m + (1|date),
               data=total.abund.w.habitat,
               family = poisson)

summary(remod2)
