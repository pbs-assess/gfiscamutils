#' Plot a distribution function. Mostly for use with making prior plots
#' for MCMC parameters. See [plot_priors_posts()]
#'
#' @param nsamp Number of points across x-axis
#' @param mean Mean of normal
#' @param sd Standard deviation of normal
#' @param xlim x-axis limits (2-element vector)
#' @param fill Fill color for under the curve
#' @param mode_line_col Mode line color
#' @param mode_line_lwd Mode line width
#' @param mode_line_lty Mode line type
#' @param mean_line_col Mean line color
#' @param mean_line_lwd Mean line width
#' @param mean_line_lty Mean line type
#' @param sd_lines_col Standard deviation lines color
#' @param sd_lines_lwd Standard deviation lines width
#' @param sd_lines_lty Standard deviation lines type
#' @param alpha Opacity of fill from 0-1
#' @param show_mode_line Logical. If `TRUE`, show a vertical line at the mode
#' of the distribution
#' @param show_mean_line Logical. If `TRUE`, show a vertical line at
#' the mean
#' @param show_sd_lines Logical. If `TRUE`, show a vertical lines one standard
#' deviation away from the mean
#' @param ... Arguments passed to [cowplot::plot_grid()]
#'
#' @return a [ggplot2::ggplot()] object
#' @export
plot_fun <- function(fun = dnorm,
                     nsamp = 100,
                     param_lst = list(mean = 0, sd = 0.5),
                     xlim = c(-2, 2),
                     fill = "steelblue",
                     title = "",
                     mode_line_col = "red",
                     mode_line_lwd = 0.5,
                     mode_line_lty = 1,
                     mean_line_col = "black",
                     mean_line_lwd = 0.5,
                     mean_line_lty = 1,
                     sd_lines_col = "black",
                     sd_lines_lwd = 0.5,
                     sd_lines_lty = 2,
                     alpha = 0.75,
                     show_mode_line = FALSE,
                     show_mean_line = TRUE,
                     show_sd_lines = TRUE){

  g <- ggplot(data = data.frame(x = xlim), aes(x)) +
    stat_function(fun = fun,
                  n = nsamp,
                  args = param_lst,
                  geom = "area",
                  fill = fill,
                  alpha = alpha,
                  lwd = 1) +
    xlim(xlim) +
    ylab("") +
    ggtitle(title) +
    scale_y_continuous(breaks = NULL) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(face = "bold.italic", hjust = 0.5))

  # Normal and lognormal case
  mode <- mean <- param_lst[[1]]
  sd <- param_lst[[2]]

  if(identical(fun, dbeta)){
    # The mode, mean, and sd calculation
    mode <- (param_lst[[1]] - 1) / (param_lst[[1]] + param_lst[[2]] - 2)
    mean <- param_lst[[1]] / (param_lst[[1]] + param_lst[[2]])
    sd <- sqrt((param_lst[[1]] * param_lst[[2]]) /
                 ((param_lst[[1]] + param_lst[[2]]) ^ 2 * (param_lst[[1]] + param_lst[[2]] + 1)))
  }
  if(identical(fun, dgamma)){
    # The mode, mean, and sd calculation
    mode <- (param_lst[[1]] - 1) / param_lst[[2]]
    mean <- param_lst[[1]] / param_lst[[2]]
    sd <- sqrt(param_lst[[1]] / (param_lst[[2]] ^ 2))
  }

  if(show_mean_line){
    g <- g + geom_vline(xintercept = mean,
                        color = mean_line_col,
                        lwd = mean_line_lwd,
                        lty = mean_line_lty)
  }
  if(show_sd_lines){
    g <- g +
      geom_vline(xintercept = mean - sd,
                 color = sd_lines_col,
                 lwd = sd_lines_lwd,
                 lty = sd_lines_lty) +
      geom_vline(xintercept = mean + sd,
                 color = sd_lines_col,
                 lwd = sd_lines_lwd,
                 lty = sd_lines_lty)
  }
  if(show_mode_line){
    g <- g + geom_vline(xintercept = mode,
                        color = mode_line_col,
                        lwd = mode_line_lwd,
                        lty = mode_line_lty)
  }


  g
}
