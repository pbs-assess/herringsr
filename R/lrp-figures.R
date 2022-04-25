# UJpdate figures for the LRP manuscript:
# Kronlund, Forrest, Cleary, and Grinnell. In prep. An evidence-based approach
#   for selecting a limit reference point for Pacific Herring (Clupea pallasii)
#   stocks in British Columbia, Canada

# First, compile the PDF on the command line:
#   bookdown::render_book("index.Rmd")
# Then `source` this file

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Fixed cut-offs
cut_off_values <- c(
  "Haida Gwaii" = 10.7,
  "Prince Rupert District" = 12.1,
  "Central Coast" = 17.6,
  "Strait of Georgia" = 21.2,
  "West Coast of Vancouver Island" = 18.8
)

# Get the data
get_lrp_data <- function(models,
                         ct,
                         yrs = 1951:2021,
                         q_value = 0.2,
                         first_yr_dive = 1988,
                         cut_offs = cut_off_values,
                         reg_names = major_regions_full) {
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
        Region = reg_names[i],
        Cutoff = cut_offs[which(names(cut_offs) == reg_names[i])],
        # Median SB_0
        SB0 = models[[i]]$mcmccalcs$r.quants["sbo", "\\textbf{50\\%}"],
        # Is biomass above or below the quantile?
        Below = ifelse(Median < quant, "Below", "NotBelow")
      ) %>%
      select(Region, Year, Median, Cutoff, SB0, Below) %>%
      rename(Biomass = Median)
    # Subset catch
    ct_sub <- ct %>%
      filter(Region == reg_names[i]) %>%
      complete(Year = yrs, fill=list(Region = reg_names[i], Catch = 0))
    # Merge biomass and catch
    dat <- sbt %>%
      full_join(y = ct_sub, by = c("Region", "Year")) %>%
      arrange(Year) %>%
      # Calculate production and production rate
      mutate(
        BiomassNext = lead(Biomass),
        catchNext = lead(Catch),
        Production = BiomassNext - Biomass + catchNext,
        ProdRate = Production / Biomass,
        Depletion = Biomass / SB0,
        HarvRate = Catch / (Catch + Biomass),
        Period = ifelse(Year < first_yr_dive, "Surface", "Dive")
      ) %>%
      select(
        Region, Year, Period, Biomass, Depletion, Catch, HarvRate, Production,
        ProdRate, SB0, Cutoff, Below
      )
    # If it's the first model start the output
    if (i == 1) {
      res <- dat
    } else { # End if first, otherwise append to output
      res <- rbind(res, dat)
    } # End if appending
  } # End loop over models
  # Set factor levels and sort by region and year
  res <- res %>%
    mutate(
      Region = factor(x = Region, levels = reg_names),
      Period = factor(x = Period, levels = c("Surface", "Dive"))) %>%
    arrange(Region, Year)
  # Return res
  return(res)
} # End get_lrp_data function

# Get the data
lrp_dat <- get_lrp_data(models = major_models, ct = major_catch)

# Figure 1: Region map (OK for now)

# Figure 2: Spawning biomass and catch (based on `plot_biomass_catch`)
figure_2 <- function(dat) {
  # Plot
  p <- ggplot(data = dat, mapping = aes(x = Year)) +
    geom_col(mapping = aes(y = Catch)) +
    geom_line(mapping = aes(y = Biomass)) +
    geom_point(mapping = aes(y = Biomass, fill = Below), shape = 21) +
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
    geom_hline(
      mapping = aes(yintercept = Cutoff), linetype = "dashed", colour = "black"
    ) +
    scale_x_continuous(
      breaks = seq(from = 1950, to = 2020, by = 10),
      labels = seq(from = 1950, to = 2020, by = 10)
    ) +
    facet_wrap(vars(Region), scales = "free_y", ncol = 2) +
    labs(y = "Spawning biomass (1,000 t)") +
    scale_fill_grey(start = 0.5, end = 1) +
    guides(fill = "none")
  # Save as PNG
  ggsave("Figure2.png", plot = p, height = 6, width = 6)
} # End of figure_2 function

# Make figure 2 (spawning biomass and catch)
# fig_2 <- figure_2(dat = lrp_dat)

# Figure 3: Production and production rate
figure_3 <- function(models, ct) {

} # End of figure_3 function

# Make figure 3 (production and production rate)
# fig_3 <- figure_3(models = major_models, ct = major_catch)

# Figure 4: Production vs production rate (based on `plot_biomass_phase`)
