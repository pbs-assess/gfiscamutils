#' Plot a grid of MCMC recruitment plots for iSCAM models
#'
#' @description
#' Plot a grid of recruitment plots, each with one model only so that
#' the R0 line and credible intervals can be viewed
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param ... Arguments passed to [plot_biomass_mcmc()] and [plot_recr_mcmc()]
#' @return A [cowplot::plot_grid()] object
#' @export
plot_recr_grid_mcmc <- function(models,
                                ...){

  if(class(models) != mdl_lst_cls){
    if(class(models) == mdl_cls){
      warning("Single model detected, not using grid, calling plot_biomass_mcmc() directly")
      return(plot_biomass_mcmc(models, ...))
    }
    stop("The `models` list is not a `gfiscamutils::mdl_lst_cls` class",
         call. = FALSE)
  }
  g_lst <- map(models, function(.x, ...){
    plot_recr_mcmc(.x, ...)
  }, ...)

  plot_grid(plotlist = g_lst)
}
