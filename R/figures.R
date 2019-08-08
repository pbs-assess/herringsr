#' Plot catch from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_catch()]
#' @param xlim Limits for the years shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
plot_catch <- function(df,
                       xlim = c(1000, 3000),
                       translate = FALSE){
  g <- ggplot(df, aes(x = year, y = value)) +
    geom_bar(stat = "identity", position = "stack", aes(fill = gear), width = 1) +
    scale_x_continuous(limits = xlim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    scale_fill_grey(start = 0, end = 0.8) +
    theme(legend.position = "top") +
    labs(x = en2fr("Year", translate),
         y = paste(en2fr("Catch", translate), " (1000 t)"),
         fill = en2fr("Gear", translate)) +
    facet_wrap( ~ region, ncol = 2, dir = "v", scales = "free_y" )
  g
}

#' Plot weight-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_wa()]
#' @param circle_age the age for which to add circles to plot
#' @param xlim Limits for the years shown on the plot
#' @param ylim limits for the weights shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
plot_wa <- function(df,
                    circle_age = 3,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1])
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "gear")) %>%
    as_tibble() %>%
    rename(Year = year,
           Age = variable,
           Weight = value) %>%
    select(-c(area, group, sex)) %>%
    group_by(region, Age) %>%
    mutate(muWeight = rollmean(x = Weight, k = 5, align = "right", na.pad = TRUE)) %>%
    ungroup() %>%
    mutate(Age = factor(Age))
  dfm_circle_age <- dfm %>%
    filter(Age == circle_age) %>%
    filter(!is.na(muWeight))
  dfm <- dfm %>%
    filter(Age != circle_age)
  g <- ggplot(dfm) +
    geom_line(aes(x = Year, y = muWeight, group = Age)) +
    geom_point(data = dfm_circle_age,
               aes(x = Year, y = Weight),
               shape = 1,
               size = 2) +
    geom_line(data = dfm_circle_age,
              aes(x = Year, y = muWeight),
              size = 2) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(x = en2fr("Year", translate),
         y = paste0(en2fr("Weight-at-age", translate), " (kg)")) +
    facet_wrap( ~ region, ncol = 2, dir = "v", scales = "free_y" )
  g
}

#' Plot proportions-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_pa()]
#' @param age_plus age plus group
#' @param conf confidence value for the envelope
#' @param xlim limits for the years shown on the plot
#' @param ylim limits for the ages shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
plot_pa <- function(df,
                    age_plus = 10,
                    conf = 0.9,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1])
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "Gear")) %>%
    as_tibble() %>%
    rename(Region = region,
           Year = year,
           Age = variable,
           Number = value) %>%
    select(-c(area, group, sex)) %>%
    mutate(Age = as.numeric(as.character(Age)),
           Age = ifelse(Age > age_plus, age_plus, Age)) %>%
    group_by(Region, Year, Age) %>%
    summarize(Number = sum(Number)) %>%
    mutate(Proportion = Number / ifelse(all(is.na(Number)), NA, sum(Number, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Age = factor(Age))

  # Determine weighted mean and approximate CI age by year
  dfm_ci <- dfm %>%
    select(Region, Year, Age, Proportion) %>%
    mutate(Age = as.numeric(Age)) %>%
    group_by(Region, Year) %>%
    summarize(MeanAge = weighted.mean(x = Age, w = Proportion),
              sBar = qnorm(1 - (1 - conf) / 2) * sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
              Lower = exp(log(MeanAge) - log(sBar)),
              Upper = exp(log(MeanAge) + log(sBar))) %>%
    ungroup() %>%
    mutate(GroupID = consecutive_group(Year))

  g <- ggplot(dfm, aes(x = Year)) +
    geom_point(aes(y = Age, size = ifelse(Proportion, Proportion, NA))) +
    geom_path(data = dfm_ci, aes(y = MeanAge, group = GroupID), size = 2) +
    geom_ribbon(data = dfm_ci,
                aes(ymin = Lower, ymax = Upper, group = GroupID),
                alpha = 0.25) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(size = en2fr("Proportion", translate)) +
    ylab(en2fr("Age", translate)) +
    xlab(en2fr("Year", translate)) +
    facet_wrap(~ Region, ncol = 2, dir = "v", scales = "free_y" ) +
    theme(legend.position="top")
  g
}
