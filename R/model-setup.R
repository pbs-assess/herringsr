library(tidyverse)
library(here)
library(csasdown)
library(gfutilities)
library(gfiscamutils)
library(rosettafish)
library(herringutils)
library(kableExtra)

# Warning if using 32-bit R
if (.Machine$sizeof.pointer == 4) {
  warning("May run out of memory in 32-bit R")
}

# Directories and names of stocks
major_stock_dir <- list("HG", "PRD", "CC", "SoG", "WCVI")
minor_stock_dir <- list("A27", "A2W")
special_stock_dir <- list("A10")
major_model_dirs <- lapply(
  1:length(major_stock_dir),
  function(x) {
    file.path(models_dir, major_stock_dir[[x]], "AM2")
  }
)
lapply(
  major_model_dirs, run_retro,
  yrs = retrospective_yrs,
  overwrite = run_retrospectives
)

minor_model_dirs <- lapply(
  1:length(minor_stock_dir),
  function(x) {
    file.path(models_dir, minor_stock_dir[[x]], "AM2")
  }
)

special_model_dirs <- lapply(
  1:length(special_stock_dir),
  function(x) {
    file.path(models_dir, special_stock_dir[[x]], "AM2")
  }
)

# Cutoffs by region, corresponding to above region order in major_stock_dir
fixed_cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)

# Level of confidence interval
confidence_vals <- c(0.05, 0.95)
ci_level <- confidence_vals[2] - confidence_vals[1]

# Build major model files
build_herring_rdata_files(
  major_model_dirs,
  ovwrt.rdata = rebuild_rdata_model_files,
  mcmc.subdir = "mcmc",
  load.proj = TRUE,
  lower = confidence_vals[1],
  upper = confidence_vals[2],
  burnin = 1000,
  thin = 1,
  fixed.cutoffs = fixed_cutoffs,
  which.model = 2
)

# Build minor model files
build_herring_rdata_files(
  minor_model_dirs,
  ovwrt.rdata = rebuild_rdata_model_files,
  mcmc.subdir = "mcmc",
  load.proj = TRUE,
  lower = confidence_vals[1],
  upper = confidence_vals[2],
  burnin = 1000,
  thin = 1,
  fixed.cutoffs = fixed_cutoffs,
  which.model = 2
)

# Build special model files
build_herring_rdata_files(
  special_model_dirs,
  ovwrt.rdata = rebuild_rdata_model_files,
  mcmc.subdir = "mcmc",
  load.proj = TRUE,
  lower = confidence_vals[1],
  upper = confidence_vals[2],
  burnin = 1000,
  thin = 1,
  fixed.cutoffs = fixed_cutoffs,
  which.model = 2
)
