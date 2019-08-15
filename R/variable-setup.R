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
assess_yr <- as.numeric(substr(Sys.Date(), 1, 4))
last_assess_yr <- assess_yr - 1
forecast_yr <- assess_yr + 1
start_yr <- 1951
start_yr_age_comps <- 1951
end_yr <- 2016
last_data_yr <- 2016
this_season <- paste(assess_yr - 1, assess_yr, sep = "/")

# -----------------------------------------------------------------------------
# Haida Gwaii-dependent values
model_ind <- match(en2fr("HG", french), major_regions_short)
sbt <- major_models[[model_ind]]$mcmccalcs$sbt.quants
sbt_yrs <- as.numeric(colnames(sbt))
## Final year spawning biomass - vector length 4 - 1 = lower, 2 = median, 3 = upper, 4 = mpd
hg_final_yr_sbt <- sbt[,sbt_yrs == assess_yr] * 1000
hg_refs <- major_models[[model_ind]]$mcmccalcs$r.quants
hg_sbo <- hg_refs[rownames(hg_refs) == "sbo",][2:4] * 1000
## Probability that final year biomass is less than 0.3B0 - vector length 3 - 1 = lower, 2 = median, 3 = upper
hg_prob_less_03sbo <- hg_refs[rownames(hg_refs) == paste0("psb", assess_yr, "/0.3sbo"),][2:4]

# -----------------------------------------------------------------------------
# Prince Rupert District-dependent values

# -----------------------------------------------------------------------------
# Central Coast-dependent values

# -----------------------------------------------------------------------------
# Strait of Georgia District-dependent values

# -----------------------------------------------------------------------------
# West Coast Vancouver Island-dependent values

# Age to highlight in figure
ageShow <- 3

# Age class of plus group for proportion-at-age
age_plus <- 10

# Age of recruitment
ageRec <- 2

# Number of years to calculate running/rolling mean
nRoll <- 5

# Number of years to calculate running/rolling mean of recruitment deviations
nRollDev <- 3

# Spawn survey method changed from surface (1951--1987) to dive (1988--present)
new_surv_yr <- 1988

# Intended harvest rate
intendU <- 0.2

# First year of intended harvest rate
intendUYrs <- 1983

# Figure width
#figWidth <- 6.5

# Type of smoothing line
smLine <- "loess"

# Limits for the weight-at-age plot
wa_ylim <- c(0.05, 0.15)

# 1996 fixed cutoff values (t*10^3)
#fixedCutoffs <- list( HG=10.7, PRD=12.1, CC=17.6, SoG=21.2, WCVI=18.8 )

# High-productivity years
hiProdYrs <- list(HG = 1994:1997,
                  PRD = 1994:2002,
                  CC = 1990:1999,
                  SoG = 1988:2016,
                  WCVI = 1988:1996 )

# Proportion of B_0 for LRP
propB0 <- 0.3

# Multiplyer of LRP for USR
multLRP <- 2

# Years for calculating mean catch (i.e., recent)
recentCatchYrs <- 35

# Years for calculating mean catch (i.e., early)
earlyCatchYrs <- 1951:1965

# First year to show for minor stock area timseries plots
firstYrMinor <- 1978



