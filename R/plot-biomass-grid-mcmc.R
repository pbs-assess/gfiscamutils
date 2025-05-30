#' Plot a grid of MCMC biomass plots for iSCAM models
#'
#' @description
#' Plot a grid of biomass plots, each with one model only so that the reference
#' point lines and credible intervals can be compared correctly.
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param ... Arguments passed to [plot_biomass_mcmc()]
#' @return A [cowplot::plot_grid()] object
#' @export
plot_biomass_grid_mcmc <- function(models, ...){

  if(class(models) != mdl_lst_cls){
    if(class(models) == mdl_cls){
      warning("Single model detected, not using grid, calling plot_biomass_mcmc() directly")
      return(plot_biomass_mcmc(models, ...))
    }
    stop("The `models` list is not a `gfiscamutils::mdl_lst_cls` class",
         call. = FALSE)
  }
  g_lst <- map(models, function(.x, ...){
    plot_biomass_mcmc(.x, ...)
  }, ...)

  plot_grid(plotlist = g_lst)
}
