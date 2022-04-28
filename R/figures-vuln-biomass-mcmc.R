#' Plot MCMC vulnerable biomass for iSCAM models
#'
#' @description
#' Plot vulnerable biomass time series plots for MCMC output with credible
#' intervals
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @export
plot_vuln_mcmc <- function(...){
  plot_ts_mcmc(...,
               quant_df = "vbt_quants",
               facet_wrap_var = "gear",
               y_label = "Vulnerable biomass ('000 t)")
}
