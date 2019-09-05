assess_yr <- 2019 #as.numeric(substr(Sys.Date(), 1, 4))
last_assess_yr <- assess_yr - 1
this_season <- paste(assess_yr - 1, assess_yr, sep = "/")

# Age class of plus group for proportion-at-age
age_plus <- 10

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
new_surv_yr <- 1988

# Limits for the weight-at-age plot
wa_ylim <- c(0.05, 0.15)

regions <- tribble(
  ~SAR, ~Region,                      ~RegionName, ~Major,
  1,       "HG",                    "Haida Gwaii",   TRUE,
  2,      "PRD",         "Prince Rupert District",   TRUE,
  3,       "CC",                  "Central Coast",   TRUE,
  4,      "SoG",              "Strait of Georgia",   TRUE,
  5,     "WCVI", "West Coast of Vancouver Island",   TRUE,
  6,      "A27",                        "Area 27",  FALSE,
  7,      "A2W",                    "Area 2 West",  FALSE)
regions$Region <- en2fr(regions$Region, french)
regions$RegionName <- en2fr(regions$RegionName, french)

gear <- tribble(
  ~gear,   ~gearname,
      1,     "Other",
      2,     "RoeSN",
      3,     "RoeGN")
gear$gearname <- en2fr(gear$gearname, french)
roe_gear <- c(2, 3)

# Survey gear follows the fishery gears above, but q parameters exist only for survey
# gears so they are included here as `qind`
surv_type <- tribble(
  ~gear,   ~gearname, ~qind,
      4,   "Surface",     1,
      5,      "Dive",     2)
surv_type$gearname <- en2fr(surv_type$gearname, french)

major_models <- load.models(unlist(major_model_dirs))
minor_models <- load.models(unlist(minor_model_dirs))
# Assumes all major region models have the same year range
major_start_yr <- major_models[[1]]$dat$start.yr
major_end_yr <- major_models[[1]]$dat$end.yr
major_yr_range <- major_start_yr:major_end_yr
# Assumes all minor region models have the same year range
minor_start_yr <- minor_models[[1]]$dat$start.yr
minor_end_yr <- minor_models[[1]]$dat$end.yr
minor_yr_range <- minor_start_yr:minor_end_yr

all_regions_short <- regions$Region
major_regions_short <- regions$Region[regions$Major]
minor_regions_short <- regions$Region[!regions$Major]

all_regions_full <- regions$RegionName
major_regions_full <- regions$RegionName[regions$Major]
minor_regions_full <- regions$RegionName[!regions$Major]

all_regions_full_parens <- paste0(all_regions_full,  " (", all_regions_short, ")")
major_regions_full_parens <- paste0(major_regions_full,  " (", major_regions_short, ")")
minor_regions_full_parens <- paste0(minor_regions_full,  " (", minor_regions_short, ")")

#Catch
major_catch <- get_catch(major_models,
                         major_regions_full,
                         gear)
major_catch_short <- get_catch(major_models,
                               major_regions_short,
                               gear)

minor_catch <- get_catch(minor_models,
                         minor_regions_short,
                         gear)

minor_final_yr_catch <- minor_catch %>%
  filter(year %in% max(year))

minor_final_yr_roe_catch <- minor_final_yr_catch %>%
  filter(gear %in% !!gear$gearname[c(2,3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

minor_final_yr_other_catch <- minor_final_yr_catch %>%
  filter(!gear %in% !!gear$gearname[c(2,3)]) %>%
  summarize(catch = sum(value) * 1000) %>%
  pull() %>%
  f()

#Spawn-on-kelp
sok_file_pattern <- "harvest-sok-*"
data_path <- here::here("data")
sok_filenames <- dir(data_path, pattern = sok_file_pattern)
sok <- sok_filenames %>%
  map(~read_csv(file.path(data_path, .))) %>%
  reduce(rbind)
sok$Region <- en2fr(sok$Region, french)

#Proportion-of-spawn
ps_file_pattern <- "prop-spawn-*"
data_path <- here::here("data")
ps_filenames <- dir(data_path, pattern = ps_file_pattern)
ps_shortnames <- sub("prop-spawn-", "", ps_filenames )
ps_shortnames <- toupper(sub(".csv", "", ps_shortnames))
ps_shortnames[ps_shortnames == "SOG"] <- "SoG"
ps_filenames <- file.path(data_path, ps_filenames)
ps <- lapply(ps_filenames, function(x){
  read_csv(x)})
names(ps) <- en2fr(ps_shortnames, french)

#Weight-at-age
major_wa <- get_wa(major_models,
                   major_regions_full,
                   gear)
minor_wa <- get_wa(minor_models,
                   minor_regions_full,
                   gear)
#Proportion-at-age
major_pa <- get_pa(major_models,
                   major_regions_full,
                   gear)
minor_pa <- get_pa(minor_models,
                   minor_regions_full,
                   gear)

#Survey Indices
major_surv <- get_surv_ind(major_models,
                           major_regions_full,
                           surv_type)
surv_yr_rng <- range(major_surv$year)

minor_surv <- get_surv_ind(minor_models,
                           minor_regions_full,
                           surv_type)

# Input catch for table 1
inp_catch <- read_csv(here::here("data/input-data.csv"))

#' Get model output values for a given region
#'
#' @param region the short form for the region as defined in the `major_regions_short` vector`
#' @param majors if TRUE, use major models list (`major_models`), if FALSE use minor models list (`minor_models`)
#' @param french if TRUE, use french
#'
#' @return a list of values as required for the report
#'
#' @examples
#' hg_vars <- get_vars("HG")
get_vars <- function(region, majors = TRUE, french = FALSE){
  if(majors){
    model_ind <- match(en2fr(region, french), major_regions_short)
    models <- major_models
  }else{
    model_ind <- match(en2fr(region, french), minor_regions_short)
    models <- minor_models
  }
  sbt <- models[[model_ind]]$mcmccalcs$sbt.quants
  sbt_yrs <- as.numeric(colnames(sbt))
  ## Final year spawning biomass - vector length 4 - 1 = lower, 2 = median, 3 = upper, 4 = mpd
  final_yr_sbt <- sbt[, sbt_yrs == assess_yr] * 1000
  refs <- models[[model_ind]]$mcmccalcs$r.quants
  sbo <- refs[rownames(refs) == "sbo",][2:4] * 1000
  ## Probability that final year biomass is less than 0.3B0 - vector length 3 - 1 = lower, 2 = median, 3 = upper
  prob_less_03sbo <- refs[rownames(refs) == paste0("psb", assess_yr, "/0.3sbo"),][2]
  proj <- models[[model_ind]]$mcmccalcs$proj.quants
  ## Projected biomass for next year - vector length 3 - 1 = lower, 2 = median, 3 = upper
  proj_sbt <- proj[, paste0("B", assess_yr + 1)] * 1000
  ## Probability that next year (projected) biomass is less than 0.3B0 - vector length 3 - 1 = lower, 2 = median, 3 = upper
  prob_proj_less_03sbo <- refs[rownames(refs) == paste0("psb", assess_yr + 1, "/0.3sbo"),][2]

  list(final_yr_sbt = final_yr_sbt,
       sbo = sbo,
       prob_less_03sbo = prob_less_03sbo,
       proj_sbt = proj_sbt,
       prob_proj_less_03sbo = prob_proj_less_03sbo)
}
# -----------------------------------------------------------------------------
# Haida Gwaii-dependent values
hg_vars <- get_vars("HG", french = french)
hg_final_yr_sbt <- hg_vars[["final_yr_sbt"]]
hg_sbo <- hg_vars[["sbo"]]
hg_prob_less_03sbo <- hg_vars[["prob_less_03sbo"]]
hg_proj_sbt <- hg_vars[["proj_sbt"]]
hg_prob_proj_less_03sbo <- hg_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Prince Rupert District-dependent values
prd_vars <- get_vars("PRD", french = french)
prd_final_yr_sbt <- prd_vars[["final_yr_sbt"]]
prd_sbo <- prd_vars[["sbo"]]
prd_prob_less_03sbo <- prd_vars[["prob_less_03sbo"]]
prd_proj_sbt <- prd_vars[["proj_sbt"]]
prd_prob_proj_less_03sbo <- prd_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Central Coast-dependent values
cc_vars <- get_vars("CC", french = french)
cc_final_yr_sbt <- cc_vars[["final_yr_sbt"]]
cc_sbo <- cc_vars[["sbo"]]
cc_prob_less_03sbo <- cc_vars[["prob_less_03sbo"]]
cc_proj_sbt <- cc_vars[["proj_sbt"]]
cc_prob_proj_less_03sbo <- cc_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# Strait of Georgia District-dependent values
sog_vars <- get_vars("SoG", french = french)
sog_final_yr_sbt <- sog_vars[["final_yr_sbt"]]
sog_sbo <- sog_vars[["sbo"]]
sog_prob_less_03sbo <- sog_vars[["prob_less_03sbo"]]
sog_proj_sbt <- sog_vars[["proj_sbt"]]
sog_prob_proj_less_03sbo <- sog_vars[["prob_proj_less_03sbo"]]
# -----------------------------------------------------------------------------
# West Coast Vancouver Island-dependent values
wcvi_vars <- get_vars("WCVI", french = french)
wcvi_final_yr_sbt <- wcvi_vars[["final_yr_sbt"]]
wcvi_sbo <- wcvi_vars[["sbo"]]
wcvi_prob_less_03sbo <- wcvi_vars[["prob_less_03sbo"]]
wcvi_proj_sbt <- wcvi_vars[["proj_sbt"]]
wcvi_prob_proj_less_03sbo <- wcvi_vars[["prob_proj_less_03sbo"]]


## Number of mcmc samples, min and max median biomass
mcmc_num_samples <- nrow(major_models[[1]]$mcmc$params)
mcmc_burnin <- mcmc_num_samples - nrow(major_models[[1]]$mcmccalcs$p.dat)
mcmc_length <- "5 million"
mcmc_samp_freq <- 1000
mcmc_ci <- "90\\%"

