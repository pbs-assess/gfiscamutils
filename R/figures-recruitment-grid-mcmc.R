#' Plot a grid of recruitment plots, each with one model only so that
#' the R0 line and credible intervals can be viewed
#'
#' @rdname plot_biomass_mcmc
#' @param ... Further arguments to pass to [cowplot::plot_grid()]
#'
#' @family Biomass plotting functions
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
