#' Plot a normal curve
#'
#' @param nsamp Number of points across x-axis
#' @param mean Mean of normal
#' @param sd Standard deviation of normal
#' @param xlim x-axis limits (2-element vector)
#' @param fill Fill color for under the curve
#' @param mean_line_col Mean line color
#' @param mean_line_lwd Mean line width
#' @param mean_line_lty Mean line type
#' @param sd_lines_col Standard deviation lines color
#' @param sd_lines_lwd Standard deviation lines width
#' @param sd_lines_lty Standard deviation lines type
#' @param alpha Opacity of fill from 0-1
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_norm <- function(nsamp = 100,
                      mean = 0,
                      sd = 0.5,
                      xlim = c(-6, 6),
                      fill = "steelblue",
                      mean_line_col = "white",
                      mean_line_lwd = 0.5,
                      mean_line_lty = 1,
                      sd_lines_col = "white",
                      sd_lines_lwd = 0.5,
                      sd_lines_lty = 2,
                      alpha = 0.75){

  g <- ggplot(data = data.frame(x = xlim), aes(x)) +
    stat_function(fun = dnorm,
                  n = nsamp,
                  args = list(mean = mean, sd = sd),
                  geom = "area",
                  fill = fill,
                  alpha = alpha,
                  lwd = 1) +
    ylab("") +
    scale_y_continuous(breaks = NULL) +
    geom_vline(xintercept = mean,
               color = mean_line_col,
               lwd = mean_line_lwd,
               lty = mean_line_lty) +
    geom_vline(xintercept = mean - sd,
               color = sd_lines_col,
               lwd = sd_lines_lwd,
               lty = sd_lines_lty) +
    geom_vline(xintercept = mean + sd,
               color = sd_lines_col,
               lwd = sd_lines_lwd,
               lty = sd_lines_lty)

  g
}
