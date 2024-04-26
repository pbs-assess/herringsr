# Years and proportion for productive period
prod_period <- list(
  HG = list(
    region = "HG",
    region_name = "Haida Gwaii",
    yrs = 1975:1985,
    prop = 0.75
  ),
  PRD = list(
    region = "PRD",
    region_name = "Prince Rupert District",
    yrs = 1983:1992,
    prop = 1.0
  ),
  CC = list(
    region = "CC",
    region_name = "Central Coast",
    yrs = 1990:1999,
    prop = 1.0
  ),
  WCVI = list(
    region = "WCVI",
    region_name = "West Coast of Vancouver Island",
    yrs = 1990:1999,
    prop = 1.0
  )
)

# Directories and names of stocks
major_model_dirs <- lapply(
  1:length(major_stock_dir),
  function(x) {
    file.path(models_dir, major_stock_dir[[x]])
  }
)
lapply(
  major_model_dirs, run_retro,
  yrs = retro_yrs,
  overwrite = run_retros
)

minor_model_dirs <- lapply(
  1:length(minor_stock_dir),
  function(x) {
    file.path(models_dir, minor_stock_dir[[x]])
  }
)

special_model_dirs <- lapply(
  1:length(special_stock_dir),
  function(x) {
    file.path(models_dir, special_stock_dir[[x]])
  }
)

# Cutoffs by region, corresponding to above region order in major_stock_dir
fixed_cutoffs <- c(10.7, 12.1, 17.6, 21.2, 18.8)

# Level of confidence interval
confidence_vals <- c(0.05, 0.95)
ci_level <- confidence_vals[2] - confidence_vals[1]
mcmc_ci <- paste(ci_level * 100, "\\%", sep = "")

# Build major model files
build_herring_rdata_files(
  major_model_dirs,
  ovwrt.rdata = rebuild_rdata_model_files,
  mcmc.subdir = "mcmc",
  load.proj = TRUE,
  lower = confidence_vals[1],
  upper = confidence_vals[2],
  burnin = mcmc_burnin,
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
  burnin = mcmc_burnin,
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
  burnin = mcmc_burnin,
  thin = 1,
  fixed.cutoffs = fixed_cutoffs,
  which.model = 2
)
