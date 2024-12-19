# Years
last_assess_yr <- assess_yr - 1
this_season <- paste(assess_yr - 1, assess_yr, sep = "/")

# MCMC
mcmc_num_samples <- (mcmc_length / mcmc_samp_freq) - mcmc_burnin

# Check MCMC parameters
check_mcmc <- function(
    mcmc_in = list(length = mcmc_length, samp_freq = mcmc_samp_freq),
    model) {
  # Get model folder
  model_dir = file.path(models_dir, model)
  # Get chain length from iscam output
  out_length <- scan(file = file.path(model_dir, "sims"), quiet = TRUE)[2]
  # Error if chains not the same length
  if(mcmc_in$length != out_length)
    stop("Wrong MCMC chain length in ", model, ".")
  # Read a CSV file
  blob <- readLines(con = file.path(model_dir, "mcmc", "iscam_m_mcmc.csv"))
  # Determine sample frequency from iscam output
  out_samp_freq <- out_length / (length(blob) - 1)
  # Error if sample frequencies not the same
  if(mcmc_in$samp_freq != out_samp_freq)
    stop("Wrong MCMC sample frequency in ", model, ".")
} # End check_mcmc function

# Check MCMC pars
for (i in seq(major_stock_dir)){
  check_mcmc(model = major_stock_dir[i])
}

# Age class to highlight in weight-at-age plots
age_highlight <- 3

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
new_surv_yr <- 1988

# Post-reduction fishery catches
recent_catch_yr <- 1972

# Number of years to show in tables (+1)
num_yrs_tab <- 9

# Limits for the weight-at-age plot
wa_ylim <- c(0.05, 0.20)

# Number of years to calculate running mean weight-at-age (window)
nRollWA <- 5

# Number of years to calculate running mean recruitment (window)
nRollRec <- 3

# Target harvest rate
target_hr <- 0.2

# Proportion of F_MSY
prop_f_msy <- 0.5

regions <- tribble(
  ~SAR, ~Region,                      ~RegionName,     ~Type,
  1,       "HG",                    "Haida Gwaii",   "Major",
  2,      "PRD",         "Prince Rupert District",   "Major",
  3,       "CC",                  "Central Coast",   "Major",
  4,      "SoG",              "Strait of Georgia",   "Major",
  5,     "WCVI", "West Coast of Vancouver Island",   "Major",
  6,      "A27",                        "Area 27",   "Minor",
  7,      "A2W",                    "Area 2 West",   "Minor",
  8,      "A10",                        "Area 10",   "Special"
)
regions$Region <- en2fr(regions$Region, french)
regions$RegionName <- en2fr(regions$RegionName, french)

gear <- tribble(
  ~gear, ~gearname,
  1, "Other",
  2, "RoeSN",
  3, "RoeGN"
)
gear$gearname <- en2fr(gear$gearname, french)
roe_gear <- c(2, 3)

# Survey gear follows the fishery gears above, but q parameters exist only for
# survey gears so they are included here as `qind`
surv_type <- tribble(
  ~gear, ~gearname, ~qind,
  4, "Surface", 1,
  5, "Dive", 2
)
surv_type$gearname <- en2fr(surv_type$gearname, french)

major_models <- load.models(unlist(major_model_dirs), inc_retro = TRUE)
minor_models <- load.models(unlist(minor_model_dirs), inc_retro = FALSE)
special_models <- load.models(unlist(special_model_dirs), inc_retro = FALSE)
# Assumes all major region models have the same year range
major_start_yr <- major_models[[1]]$dat$start.yr
major_end_yr <- major_models[[1]]$dat$end.yr
major_yr_range <- major_start_yr:major_end_yr
# Assumes all minor region models have the same year range
minor_start_yr <- minor_models[[1]]$dat$start.yr
minor_end_yr <- minor_models[[1]]$dat$end.yr
minor_yr_range <- minor_start_yr:minor_end_yr
minor_start_yr_plot <- 1978
# Assumes all special area models have the same year range
special_start_yr <- special_models[[1]]$dat$start.yr
special_end_yr <- special_models[[1]]$dat$end.yr
special_yr_range <- special_start_yr:special_end_yr
special_start_yr_plot <- 1978

all_regions_short <- regions$Region
major_regions_short <- regions$Region[regions$Type == "Major"]
minor_regions_short <- regions$Region[regions$Type == "Minor"]
special_regions_short <- regions$Region[regions$Type == "Special"]

all_regions_full <- regions$RegionName
major_regions_full <- regions$RegionName[regions$Type == "Major"]
minor_regions_full <- regions$RegionName[regions$Type == "Minor"]
special_regions_full <- regions$RegionName[regions$Type == "Special"]

all_regions_full_parens <- paste0(
  all_regions_full, " (", all_regions_short, ")"
)
major_regions_full_parens <- paste0(
  major_regions_full, " (", major_regions_short, ")"
)
minor_regions_full_parens <- paste0(
  minor_regions_full, " (", minor_regions_short, ")"
)
special_regions_full_parens <- paste0(
  special_regions_full, " (", special_regions_short, ")"
)

# Catch
major_catch <- get_catch(major_models, major_regions_full, gear)
major_catch_short <- get_catch(major_models, major_regions_short, gear)
minor_catch <- get_catch(minor_models, minor_regions_full, gear)
minor_catch_short <- get_catch(minor_models, minor_regions_short, gear)
special_catch <- get_catch(special_models, special_regions_full, gear)
special_catch_short <- get_catch(special_models, special_regions_short, gear)

# Minimum age
age_first <- major_models[[1]]$dat$start.age

# Age class of plus group for proportion-at-age
age_plus <- major_models[[1]]$dat$end.age

suppressWarnings(
  # supress warnings is because the two tables have different factors for the
  # `region` column and are therefore coerced to character types before binding
  total_final_yr_catch <- bind_rows(major_catch_short, minor_catch_short) %>%
    filter(year == assess_yr)
)

total_final_yr_roe_catch <- total_final_yr_catch %>%
  filter(gear %in% !!gear$gearname[c(2, 3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

total_final_yr_other_catch <- total_final_yr_catch %>%
  filter(!gear %in% !!gear$gearname[c(2, 3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

# Data file
data_path <- here("data")

# Spawn-on-kelp
sok_file_pattern <- "harvest-sok-*"
sok_filenames <- dir(data_path, pattern = sok_file_pattern)
sok <- sok_filenames %>%
  map(~ read_csv(file.path(data_path, .), col_types = cols())) %>%
  reduce(rbind)
sok$Region <- en2fr(sok$Region, french)

# Proportion-of-spawn
ps_file_pattern <- "prop-spawn-*"
ps_filenames <- dir(data_path, pattern = ps_file_pattern)
ps_shortnames <- sub("prop-spawn-", "", ps_filenames)
ps_shortnames <- toupper(sub(".csv", "", ps_shortnames))
ps_shortnames[ps_shortnames == "SOG"] <- "SoG"
ps_filenames <- file.path(data_path, ps_filenames)
ps <- lapply(ps_filenames, function(x) read_csv(x, col_types = cols()))
names(ps) <- en2fr(ps_shortnames, french)

# Weight-at-age
major_wa <- get_wa(major_models, major_regions_full, gear)
minor_wa <- get_wa(minor_models, minor_regions_full, gear)
special_wa <- get_wa(special_models, special_regions_full, gear)

# Proportion-at-age
major_pa <- get_pa(major_models, major_regions_full, gear)
minor_pa <- get_pa(minor_models, minor_regions_full, gear)
special_pa <- get_pa(special_models, special_regions_full, gear)

# Survey indices
major_surv <- get_surv_ind(major_models, major_regions_full, surv_type)
surv_yr_rng <- range(major_surv$year)
minor_surv <- get_surv_ind(minor_models, minor_regions_full, surv_type)
minor_surv_short <- get_surv_ind(minor_models, minor_regions_short, surv_type)
special_surv <- get_surv_ind(special_models, special_regions_full, surv_type)
special_surv_short <- get_surv_ind(
  special_models, special_regions_short, surv_type
)

# Maximum natural mortality
M_max <- lapply(
  X = major_models, FUN = function(x) max(x$mcmccalcs$nat.mort.quants)
) %>%
  unlist() %>%
  max(na.rm = TRUE)

# Input data for model input
inp_data <- read_csv(here("data", "input-data.csv"), col_types = cols())

# Input data for table of ecosystem trends description
eco_trend_desc_data <- read_delim(
  here("data", ifelse(french, "eco-trend-desc-fr.csv", "eco-trend-desc.csv")),
  delim = ";", col_types = cols(), locale = locale(encoding = "Windows-1252")
)

# SARs - dependent values
hg_vars <- get_vars("HG", french = french)
prd_vars <- get_vars("PRD", french = french)
cc_vars <- get_vars("CC", french = french)
if(!keep_sog) sog_vars <- get_vars("SoG", french = french)
wcvi_vars <- get_vars("WCVI", french = french)

incl_sog <- function(tabfig = "table", trans = french) {
  if(french){
    res <- paste(
      "Notez que le stock SoG est évalué dans MPO (En press)$^{\\ref{fn:sogfsar}}$",
      "mais les données SoG sont incluses dans ce",
      en2fr(tabfig, translate = trans),
      "par souci d'exhaustivité."
    )
  } else {
    res <- paste(
      "Note that the SoG stock is assessed in DFO (In Prep.)$^{\\ref{fn:sogfsar}}$",
      "but SoG data is included in this", tabfig, "for completeness."
    )
  }
  res
}
