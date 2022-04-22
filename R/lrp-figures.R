# UJpdate figures for the LRP manuscript:
# Kronlund, Forrest, Cleary, and Grinnell. In prep. An evidence-based approach
#   for selecting a limit reference point for Pacific Herring (Clupea pallasii)
#   stocks in British Columbia, Canada

# First, compile the PDF on the command line:
#   bookdown::render_book("index.Rmd")
# Then `source` this file

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Parameters
proj_yr <- 2022 # exclude sb_t for this year
cut_offs <- c(HG = 10.7, PRD = 12.1, CC = 17.6, SoG = 21.2, WCVI = 18.8)

# Figure 1: Region map (OK for now)

# Figure 2: Spawning biomass and catch (based on `plot_biomass_catch`)
figure_2 <- function(models, ct) {
  # Loop over models and extract spawning biomass from models
  for(i in 1:length(models)) {
    dat <- models[[i]]$mcmccalcs$sbt.quants %>%
      t() %>%
      as_tibble(rownames = "year") %>%
      mutate(year = as.numeric(year)) %>%
      filter(year != proj_yr)
    names(dat) <- c("Year", "Lower", "Median", "Upper", "MPD")
    q20 <- quantile(x = dat$Median, p = 0.2)
    dat <- dat %>%
      mutate(
        Region = major_regions_full[i],
        Cutoff = cut_offs[i],
        SB0 = models[[i]]$mcmccalcs$r.quants["sbo", 3],
        Below20 = ifelse(Median < q20, "Below", "NotBelow")
      )
    if(i == 1) {
      sbt <- dat
    } else {
      sbt <- rbind(sbt, dat)
    }
    sbt <- sbt %>%
      mutate(Region = factor(x = Region, levels = major_regions_full))
  }
  # Wrangle catch
  ct <- ct %>%
    rename(Year = year, Region = region) %>%
    group_by(Year, Region) %>%
    summarise(Catch = sum(value)) %>%
    ungroup()
  # Plot
  p <- ggplot(
    data = ct,
    mapping = aes(x = Year, y = Catch)) +
    geom_col() +
    geom_line(data = sbt, mapping = aes(x = Year, y = Median)) +
    geom_point(
      data = sbt,
      mapping = aes(x = Year, y = Median, fill = Below20),
      shape = 21
    ) +
    geom_hline(
      data = sbt, aes(yintercept = SB0 * 0.1),
      linetype = "solid", colour = "red"
    ) +
    geom_hline(
      data = sbt, aes(yintercept = SB0 * 0.25),
      linetype = "solid", colour = "blue"
    ) +
    geom_hline(
      data = sbt, aes(yintercept = SB0 * 0.3),
      linetype = "solid", colour = "green"
    ) +
    geom_hline(
      data = sbt, aes(yintercept = SB0),
      linetype = "solid", colour = "black"
    ) +
    geom_hline(
      data = sbt, aes(yintercept = Cutoff),
      linetype = "dashed", colour = "blue"
    ) +
    scale_x_continuous(
      breaks = seq(from = 1950, to = 2020, by = 10),
      labels = seq(from = 1950, to = 2020, by = 10)
    ) +
    facet_wrap(vars(Region), scales = "free_y", ncol = 2) +
    labs(y = "Spawning biomass (1,000 t)") +
    scale_fill_grey(start = 0.5, end = 1) +
    guides(fill = "none")
  ggsave("Figure2.png", plot = p, height = 6, width = 6)
}

# Make figure 2 (spawning biomass and catch)
fig_2 <- figure_2(models = major_models, ct = major_catch)

# Figure 3: Production and production rate

# Figure 4: Production vs production rate (based on `plot_biomass_phase`)
