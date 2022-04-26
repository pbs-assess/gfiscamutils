#' Plot fishing mortality time series plots for MCMC output with credible
#' intervals
#'
#' @rdname plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param type 'i' means instantaneous, 'e' means exploitation rate (Ut)
#' @export
plot_f_mcmc <- function(..., type = c("i", "e")){

  type <- match.arg(type)
  plot_ts_mcmc(...,
               quant_df = ifelse(type == "i", "ft_quants", "ut_quants"),
               facet_wrap_var = "gear",
               y_label = if(type == "i")
                 "Fishing mortality" else
                   bquote("Explotation rate" ~ U[t]*" = "*1-e^-F))
}
