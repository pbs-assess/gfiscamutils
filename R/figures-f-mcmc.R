#' Plot fishing mortality time series plots for MCMC output with credible
#' intervals
#'
#' @rdname plot_ts_mcmc
#' @family Time series plotting functions
#' @export
plot_f_mcmc <- function(...){
  plot_ts_mcmc(...,
               quant_df = "ft_quants",
               facet_wrap_var = "gear",
               y_label = "Fishing mortality")
}
