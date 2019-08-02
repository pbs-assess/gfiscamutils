#' Plot weight-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_wa()]
#' @param circle_age the age for which to add circles to plot
#' @param xlim Limits for the years shown on the plot
#' @param ylim limits for the weights shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
#' @importFrom dplyr filter as_tibble rename select group_by ungroup
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_cartesian expand_limits
#'  xlab ylab facet_wrap
#' @importFrom zoo rollmean
#' @export
plot_wa <- function(df,
                    circle_age = 3,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1])
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "Gear")) %>%
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
    ylab(paste0(en2fr("Weight-at-age", translate), " (kg)")) +
    xlab(en2fr("Year", translate)) +
    facet_wrap( ~ region, ncol = 2, dir = "v", scales = "free_y" )
  g
}
