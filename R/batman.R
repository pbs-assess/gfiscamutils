#' Call the Batman
#'
#' https://charlotte-ngs.github.io/BatmanPlot/BatmanPlotPost.html
#'
#' @param line_thick Line thickness
#' @importFrom ggplot2 ggplot stat_function geom_path aes element_blank labs
#' @return a ggplot2 object
batman <- function(line_thick = 5){
  sc <- function(x) sqrt(1 - x ^ 2)                                         # semicircle
  el <- function(x) 3 * sc(abs(x) / 7)                                      # ellipse
  nl <- function(x) (-1) * el(x)                                            # negative of el
  sh <- function(x) 4.2 - 0.5 * abs(x) - 2.8 * sc(0.5 * abs(x) - 0.5)       # shoulders
  bf <- function(x) sc(abs(2 - abs(x)) - 1) - x ^ 2 / 11 + 0.5 * abs(x) - 3 # bottom
  cr  <- data.frame(x = c(0, 0.5, 0.8, 1), y = c(1.7, 1.7, 2.6, 0.9))       # cowl right
  cl  <- data.frame(x = -cr$x, y = cr$y)                                    # cowl left

  plot_create <- function(fun, xmin, xmax){
    rp <- stat_function(fun = fun, xlim = c(xmin,xmax), size = line_thick)
    lp <- stat_function(fun = fun, xlim = c(-xmax, -xmin), size = line_thick)
    result_plot <- list(left_plot = lp, right_plot = rp)
    return(result_plot)
  }

  g <- ggplot(data.frame(x = c(-7, 7), y = c(-3, 3)), aes(x, y)) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(x = "", y = "")

  upper_wing_plot <- plot_create(fun = el, xmin = 3, xmax = 7)
  lower_wing_plot <- plot_create(fun = nl, xmin = 4, xmax = 7)
  g <- g +
    upper_wing_plot$left_plot +
    upper_wing_plot$right_plot +
    lower_wing_plot$left_plot +
    lower_wing_plot$right_plot

  sh_plot <- plot_create(fun = sh, xmin = 1, xmax = 3)
  g <- g +
    sh_plot$left_plot +
    sh_plot$right_plot

  bt_plot <- plot_create(fun = bf, xmin = 0, xmax = 4)
  g <- g +
    bt_plot$left_plot +
    bt_plot$right_plot +
    geom_path(data = cr,
              na.rm = TRUE,
              size = line_thick) +
    geom_path(data = cl,
              na.rm = TRUE,
              size = line_thick)
  g
}
