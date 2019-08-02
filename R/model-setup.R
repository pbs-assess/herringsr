rebuild_rdata_model_files <- FALSE

library(tidyverse)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(rosettafish)
library(coda)

models_dir <- here::here("models")
confidence_vals <- c(0.05, 0.95)
## Fixed cutoffs for decision tables, corresponsing to the stock order
## HG, PRD, CC, SOG, WCVI
fixed_cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)
assess_yr <- as.numeric(substr(Sys.Date(), 1, 4))
last_assess_yr <- assess_yr - 1
forecast_yr <- assess_yr + 1
start_yr <- 1951
start_yr_age_comps <- 1951
end_yr <- 2016
last_data_yr <- 2016
this_season <- paste(assess_yr - 1, assess_yr, sep = "/")

## Directories and names of stocks
stock_dir <- list("HG", "PRD", "CC", "SOG", "WCVI", "A27", "A2W")
base_model_dirs <- lapply(1:length(stock_dir),
                          function(x){
                            file.path(models_dir, stock_dir[[x]], "AM2")})

build_herring_rdata_files(base_model_dirs,
                          ovwrt.rdata = rebuild_rdata_model_files,
                          mcmc.subdir = "mcmc",
                          load.proj = TRUE,
                          lower = confidence_vals[1],
                          upper = confidence_vals[2],
                          burnin = 1000,
                          thin = 1,
                          fixed.cutoffs = fixed_cutoffs)
