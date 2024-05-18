#' Plot MCMC fishing mortalities for iSCAM models
#'
#' @description
#' Plot fishing mortality time series plots for MCMC output with credible
#' intervals
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param type 'i' means instantaneous, 'e' means exploitation rate (Ut)
#' @export
plot_f_mcmc <- function(..., type = c("i", "e")){

  type <- match.arg(type)

  exp_rate_label <- tr("Exploitation rate")
  plot_ts_mcmc(...,
               quant_df = ifelse(type == "i", "ft_quants", "ut_quants"),
               facet_wrap_var = "gear",
               y_label = if(type == "i")
                 tr("Fishing mortality") else
                   bquote(.(exp_rate_label) ~ U[t]*" = "*1-e^-F))
}
