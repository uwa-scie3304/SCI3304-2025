###
# Project: SCIE3304 - BRUV Analysis
# Data:    Albany BRUV data
# Task:    Run model selection with FSSgam
# Author:  Claude Spencer
# Date:    May 2024
##

# Clear objects from your environment
rm(list = ls())

# Load libraries
# devtools::install_github("GlobalArchiveManual/CheckEM") # Use this to install the CheckEM package if you have not already done so
library(CheckEM)
library(tidyverse)
library(mgcv)
# library(devtools)
# devtools::install_github("beckyfisher/FSSgam_package") # USe this to install the FSSgam package if you have not already done so
library(FSSgam)
library(here)
library(patchwork)

# Set your working directory to the project's directory
setwd(here::here())

# Set the study name
name <- 'SCI3304-2025'

# Load data
dat <- readRDS(here::here(paste0('data/tidy/',
                                 name,'_tidy-length.rds'))) %>%
  dplyr::filter(!is.na(macroalgae),
                !is.na(mean_relief)) %>%
  dplyr::mutate(unconsolidated = unconsolidated/total_points_annotated,         # Transform habitats into proportions
                seagrasses = seagrasses/total_points_annotated,
                macroalgae = macroalgae/total_points_annotated) %>%
  glimpse()


# Consider sub-setign the data to the area of the harbour that is most comparable - maybe the port area? Can we do this by depth?


# Set the predictor variables to use - these should be variables that you expect to influence your response variable (e.g. ecologically meaningful)
pred.vars <- c("distance_from_access", "unconsolidated", "macroalgae", "seagrasses",
               "depth_m", "mean_relief", "sd_relief")

# Create a correlation table in order to remove highly correlated variables, which can influence model selection (>0.95)
round(cor(dat[ , pred.vars]), 2)

# Check to see if any transformations are necessary
# Hey Tim this works fine on my computer - maybe will work if you restart R?
CheckEM::plot_transformations(pred.vars = pred.vars, dat = dat)

# Re-set the predictor variables with highly correlated variables removed and any transformations carried out
pred.vars <- c("distance_from_access", "unconsolidated", "macroalgae", "seagrasses",
               "depth_m", "mean_relief", "sd_relief")

# Check to see that your response variables have more than 95% zeroes. Model selection will produce unreliable results if data is too zero-inflated
unique.vars <- unique(as.character(dat$response))
resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- dat[which(dat$response == unique.vars[i]), ]
  if(length(which(temp.dat$number == 0)) / nrow(temp.dat) < 0.95){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars


# Is the number of egal KGW meaningful? Be interested in a plot?
# Is there another way of looking/presenting the length data?


# Set up the R environment for model selection
outdir  <- ("model out/")
out.all <- list()
var.imp <- list()

summary(dat)

# Run the full subset model selection process
# More information is available at:
citation("FSSgam") # Run this with other packages when you need a citation for your reports :)

# Fisher, Rebecca, Shaun K. Wilson, Tsai M. Sin, Ai C. Lee, and Tim J. Langlois. 2018. “A Simple Function for Full-Subsets Multiple Regression in Ecology with R.” Ecology and Evolution 8 (12): 6104–13.

citation("mgcv")

# resp.vars
#
#
# Model1  <- gam(number ~ s(unconsolidated, k = 3, bs = 'cr'),
#                family = tw(),  data = dat%>%filter(response%in%"Sublegal KGW"))
#
#
# plot(Model1)
# summary(Model1)

unique(use.dat$response)
names(dat)
pred.vars

for(i in 1:length(resp.vars)){
  use.dat = as.data.frame(dat[which(dat$response == resp.vars[i]),])
  print(resp.vars[i])



  Model1  <- gam(number ~ s(depth_m, k = 3, bs = 'cr'),
                 family = tw(),  data = use.dat)

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  # factor.smooth.interactions = NA,

                                  k = 3)
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T)
  names(out.list)

  out.list$failed.models
  mod.table = out.list$mod.data.out
  mod.table = mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi = cumsum(mod.table$wi.AICc)
  out.i = mod.table[which(mod.table$delta.AICc <= 10),]
  out.all = c(out.all,list(out.i))
  var.imp = c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw))

  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = here::here(paste(outdir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/")))
    if(best.model.name != "null"){
      par(mfrow = c(3,1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T,pages = 1,residuals = T,pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}
    dev.off()
  }
}






# Save the output files
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- list_rbind(out.all, names_to = "response")
all.var.imp  <- as.data.frame(do.call("rbind", var.imp))
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_all.var.imp.csv"))
