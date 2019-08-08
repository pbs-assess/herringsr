#' Plot catch from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_catch()]
#' @param xlim Limits for the years shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot geom_bar scale_x_continuous expand_limits scale_fill_grey theme
#'  labs facet_wrap
#' @export
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

