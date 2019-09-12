library(tidyverse)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(rosettafish)
library(herringutils)

## ------------------------------------------------------------------------------------------------
## Directories and names of stocks
major_stock_dir <- list("HG", "PRD", "CC", "SOG", "WCVI")
minor_stock_dir <- list("A27", "A2W")
major_model_dirs <- lapply(1:length(major_stock_dir),
                           function(x){
                             file.path(models_dir, major_stock_dir[[x]], "AM2")})
lapply(major_model_dirs, run_retro, yrs = retrospective_yrs, overwrite = run_retrospectives)

minor_model_dirs <- lapply(1:length(minor_stock_dir),
                           function(x){
                             file.path(models_dir, minor_stock_dir[[x]], "AM2")})

# Cutoffs by region, corresponding to above region order in major_stock_dir
fixed_cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)
# Level of confidence interval
ci_level <- 0.9
confidence_vals <- c(0.05, 0.95)

build_herring_rdata_files(major_model_dirs,
                          ovwrt.rdata = rebuild_rdata_model_files,
                          mcmc.subdir = "mcmc",
                          load.proj = TRUE,
                          lower = confidence_vals[1],
                          upper = confidence_vals[2],
                          burnin = 1000,
                          thin = 1,
                          fixed.cutoffs = fixed_cutoffs,
                          which.model = 2)

build_herring_rdata_files(minor_model_dirs,
                          ovwrt.rdata = rebuild_rdata_model_files,
                          mcmc.subdir = "mcmc",
                          load.proj = TRUE,
                          lower = confidence_vals[1],
                          upper = confidence_vals[2],
                          burnin = 1000,
                          thin = 1,
                          fixed.cutoffs = fixed_cutoffs,
                          which.model = 2)
