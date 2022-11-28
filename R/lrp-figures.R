# UJpdate figures for the LRP manuscript:
# Kronlund, Forrest, Cleary, and Grinnell. In prep. An evidence-based approach
#   for selecting a limit reference point for Pacific Herring (Clupea pallasii)
#   stocks in British Columbia, Canada

# First, compile the PDF on the command line:
#   bookdown::render_book("index.Rmd")
# Then `source` this file

# Libraries
library(zoo)
library(ggrepel)

# Suppress summarise info and allow overlaps
options(dplyr.summarise.inform = FALSE, ggrepel.max.overlaps = Inf)

# Output folder
ms_out <- "lrp_ms"

# Make it if it's not there
if(!ms_out %in% list.files()) dir.create(ms_out)

# Fixed cut-offs
cut_off_values <- c(
  "HG" = 10.7, "PRD" = 12.1, "CC" = 17.6, "SoG" = 21.2, "WCVI" = 18.8
)

# Parameters
first_yr_dive <- 1988 # First year of the dive survey
yrs <- 1951:2021 # Years to include
last_yr_prod <- max(yrs) - 1 # Last year to include production

# Years with historic and recent LP-LB states
LPLB <- list(
  HG = list(early = 1965:1969, recent = 2000:2010),
  PRD = list(early = 1967:1972, recent = NA),
  CC = list(early = 1964:1969, recent = 2006:2011),
  SOG = list(early = 1965:1970, recent = NA),
  WCVI = list(early = 1966:1969, recent = 2004:2014)
)

# Get the data: requires a models object, catch data, quantile for biomass
# threshold, window for rolling mean, fixed cutoff values, and region names
get_lrp_data <- function(models,
                         ct,
                         lplb = LPLB,
                         q_value = 0.2,
                         n_roll = 3,
                         cut_offs = cut_off_values,
                         reg_names_short = major_regions_short,
                         reg_names_long = major_regions_full) {
  # Wrangle catch
  ct <- ct %>%
    rename(Year = year, Region = region) %>%
    mutate(Region = as.character(Region)) %>%
    group_by(Region, Year) %>%
    summarise(Catch = sum(value)) %>%
    ungroup() %>%
    select(Region, Year, Catch)
  # Loop over models
  for (i in 1:length(models)) {
    # Get LP-LB
    shade = lplb[[i]]
    # Get spawning biomass
    sbt <- models[[i]]$mcmccalcs$sbt.quants %>%
      t() %>%
      as_tibble(rownames = "year") %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% yrs)
    names(sbt) <- c("Year", "Lower", "Median", "Upper", "MPD")
    # Calculate quantile (for shading)
    quant <- quantile(x = sbt$Median, p = q_value)
    # Wrangle biomass
    sbt <- sbt %>%
      mutate(
        # Order is important here: regions are ordered in the SR code
        Region = reg_names_long[i],
        Reg = reg_names_short[i],
        Cutoff = cut_offs[which(names(cut_offs) == reg_names_short[i])],
        # Median SB_0
        SB0 = models[[i]]$mcmccalcs$r.quants["sbo", "\\textbf{50\\%}"],
        # Is biomass above or below the quantile?
        Below = ifelse(Median < quant, "Below", "NotBelow")
      ) %>%
      select(Region, Reg, Year, Median, Cutoff, SB0, Below) %>%
      rename(Biomass = Median)
    # Get depletion
    dt <- models[[i]]$mcmccalcs$depl.quants %>%
      t() %>%
      as_tibble(rownames = "year") %>%
      mutate(year = as.numeric(year)) %>%
      filter(year %in% yrs)
    names(dt) <- c("Year", "Lower", "Median", "Upper", "MPD")
    # Wrangle depletion
    dt <- dt %>%
      mutate(Region = reg_names_long[i]) %>%
      rename(Depletion = Median)
    # Merge biomass and depletion
    sbt <- sbt %>%
      full_join(y = dt, by = c("Region", "Year"))
    # Merge shading
    sbt <- sbt %>%
      mutate(
        LPLB = "No",
        LPLB = ifelse(Year %in% shade$early, "Early", LPLB),
        LPLB = ifelse(Year %in% shade$recent, "Recent", LPLB)
      )
    # Subset catch
    ct_sub <- ct %>%
      filter(Region == reg_names_long[i]) %>%
      complete(Year = yrs, fill = list(Region = reg_names_long[i], Catch = 0))
    # Merge biomass and catch
    dat <- sbt %>%
      full_join(y = ct_sub, by = c("Region", "Year")) %>%
      arrange(Year) %>%
      # Calculate production and production rate
      mutate(
        BiomassNext = lead(Biomass),
        CatchNext = lead(Catch),
        # Calculate production
        Production = BiomassNext - Biomass + CatchNext,
        ProdSmooth = rollmean(
          x = Production, k = n_roll, align = "right", fill = NA
        ),
        ProdRate = Production / Biomass,
        # Calculate harvest rate
        HarvRate = Catch / (Catch + Biomass),
        Period = ifelse(Year < first_yr_dive, "Surface", "Dive")
      ) %>%
      select(
        Region, Reg, Year, Period, Biomass, Depletion, Catch, HarvRate,
        Production, ProdSmooth, ProdRate, SB0, Cutoff, Below, LPLB
      )
    # If it's the first model start the output
    if (i == 1) {
      res <- dat
    } else { # End if first, otherwise append to output
      res <- rbind(res, dat)
    } # End if appending
  } # End loop over models
  # Update factor levels
  reg_names_short[reg_names_short == "SoG"] <- "SOG"
  # Set factor levels and sort by region and year
  res <- res %>%
    filter(Year %in% yrs) %>%
    mutate(
      Region = factor(x = Region, levels = reg_names_long),
      Reg = ifelse(Reg == "SoG", "SOG", Reg),
      Reg = factor(x = Reg, levels = reg_names_short),
      Period = factor(x = Period, levels = c("Surface", "Dive")),
      LPLB = factor(x = LPLB, levels = c("No", "Early", "Recent"))
    ) %>%
    arrange(Region, Year)
  # Return res
  return(res)
} # End get_lrp_data function

# Get the data
lrp_dat <- get_lrp_data(models = major_models, ct = major_catch)

# Figure 1: Region map
# Made by tweaking code in ../DataSummaries/Summary.R for `BCMap`:
# shapes$regCentDF <- shapes$regCentDF[shapes$regCentDF$Region %in% allRegions$major, ]
# shapes$regCentDF$Region[shapes$regCentDF$Region == "SoG"] <- "SOG"
# shapes$regAllDF <- shapes$regAllDF[1:922, ]
# And made by tweaking code in ../SpawnIndex/tr
# sf_reg <- sf_sec %>%
#   group_by(Region) %>%
#   summarise() %>%
#   ungroup() %>%
#   filter(Region %in% c("HG", "PRD", "CC", "SoG", "WCVI")) %>%
#   mutate(Region = ifelse(Region == "SoG", "SOG", Region)) %>%
#   sf_remove_holes()

# Figure 2: Spawning biomass and catch (based on `plot_biomass_catch`)
fig_2 <- ggplot(data = lrp_dat, mapping = aes(x = Year)) +
  geom_col(mapping = aes(y = Catch), fill = "grey", width = 0.67) +
  geom_path(mapping = aes(y = Biomass)) +
  geom_point(mapping = aes(y = Biomass, fill = LPLB), shape = 21) +
  geom_hline(
    mapping = aes(yintercept = SB0 * 0.1), linetype = "solid", colour = "red"
  ) +
  geom_hline(
    mapping = aes(yintercept = SB0 * 0.25), linetype = "solid", colour = "blue"
  ) +
  geom_hline(
    mapping = aes(yintercept = SB0 * 0.3), linetype = "solid", colour = "green"
  ) +
  geom_hline(
    mapping = aes(yintercept = SB0), linetype = "solid", colour = "black"
  ) +
  geom_vline(xintercept = first_yr_dive - 0.5, linetype = "dotted") +
  scale_x_continuous(
    breaks = seq(from = 1950, to = 2020, by = 10),
    labels = seq(from = 1950, to = 2020, by = 10)
  ) +
  facet_wrap(Reg ~ ., scales = "free_y", ncol = 1, strip.position = "right") +
  labs(y = "Spawning biomass (1,000 t)") +
  scale_fill_grey(start = 1, end = 0) +
  guides(fill = "none")

# Save as PNG
ggsave(file.path(ms_out, "Figure2.png"), plot = fig_2, dpi = 600,
       height = 8, width = 6)

# Figure 3: Production and production rate
fig_3 <- ggplot(data = lrp_dat, mapping = aes(x = Year)) +
  geom_path(mapping = aes(y = Production), na.rm = TRUE) +
  geom_point(
    mapping = aes(y = Production, fill = LPLB), shape = 21, na.rm = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = first_yr_dive - 0.5, linetype = "dotted") +
  scale_x_continuous(
    breaks = seq(from = 1950, to = 2020, by = 10),
    labels = seq(from = 1950, to = 2020, by = 10)
  ) +
  facet_wrap(~Reg, scales = "free_y", ncol = 1, strip.position = "right") +
  labs(y = "Surplus production (1,000 t)") +
  scale_fill_grey(start = 1, end = 0) +
  guides(fill = "none", shape = "none")

# Save as PNG
ggsave(file.path(ms_out, "Figure3.png"), plot = fig_3, dpi = 600,
       height = 8, width = 6)

# Figure 4: Production vs production rate (based on `plot_biomass_phase`)
fig_4 <- ggplot(
  data = lrp_dat %>% filter(Period == "Dive"),
  mapping = aes(x = Biomass, y = Production)
) +
  geom_path(na.rm = TRUE) +
  geom_point(
    data = lrp_dat %>% filter(Period == "Dive", Year != last_yr_prod),
    mapping = aes(color = Year), shape = 19, size = 2, na.rm = TRUE
  ) +
  geom_point(
    data = lrp_dat %>% filter(Period == "Dive", Year == last_yr_prod),
    shape = 24, size = 2, color = "black", fill = "white", na.rm = TRUE
  ) +
  geom_vline(
    mapping = aes(xintercept = SB0 * 0.1), linetype = "solid", colour = "red"
  ) +
  geom_vline(
    mapping = aes(xintercept = SB0 * 0.25), linetype = "solid", colour = "blue"
  ) +
  geom_vline(
    mapping = aes(xintercept = SB0 * 0.3), linetype = "solid", colour = "green"
  ) +
  geom_vline(
    mapping = aes(xintercept = SB0), linetype = "solid", colour = "black"
  ) +
  scale_color_gradient(low = "lightgrey", high = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(data = lrp_dat %>% filter(Period == "Dive", Year %% 2 == 0),
    aes(label = Year), segment.colour = "grey", size = 3, na.rm = TRUE,
    force = 10, force_pull = 0
  ) +
  guides(color = "none") +
  expand_limits(x = 0) +
  facet_wrap(~Reg, scales = "free", ncol = 2) +
  labs(x = "Spawning biomass (1,000 t)", y = "Surplus production (1,000 t)")

# Save as PNG
ggsave(file.path(ms_out, "Figure4.png"), plot = fig_4, dpi = 600,
       height = 8, width = 6)

# Write tables: supplementary info
write_supp_info <- function(dat, reg_names = major_regions_short) {
  # Loop over regions
  for(i in 1:length(reg_names)) {
    out <- dat %>%
      filter(Reg == reg_names[i]) %>%
      select(
        Year, Period, Biomass, Depletion, Catch, HarvRate, Production, ProdRate
      ) %>%
      rename(
        B = Biomass, D = Depletion, C = Catch, U = HarvRate, P = Production,
        'P/B' = ProdRate
      ) %>%
      mutate(Year = as.integer(Year)) %>%
      mutate_if(is.double, formatC, digits = 2, format = "f") %>%
      arrange(Year) %>%
      write_csv(file = file.path(ms_out, paste(reg_names[i], "csv", sep="")))
  } # End loop over regions
} # End write_supp_info function

# Write the tables
write_supp_info(dat = lrp_dat)
