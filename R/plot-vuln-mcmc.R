#' Plot MCMC vulnerable biomass for iSCAM models
#'
#' @description
#' Plot vulnerable biomass time series plots for MCMC output with credible
#' intervals
#'
#' @inheritParams plot_ts_mcmc
#' @param inc_sbt Logical. If `TRUE`, put all gear's vulnerable biomass
#' on one plot along with the spawning biomass (sbt). If `FALSE`, make
#' a panel plot with one vulnerable biomass plot per panel, with no
#' spawning biomass
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @family Time series plotting functions
#'
#' @export
plot_vuln_mcmc <- function(model,
                           append_base_txt = tr("Spawning biomass"),
                           xlim = NULL,
                           ylim = NULL,
                           leg_loc = NULL,
                           angle_x_labels = FALSE){


    if(!is_iscam_model(model)){
      stop("The `model` is not a ", mdl_cls, " class", call. = FALSE)
    }
    num_vbt <- length(model$mcmccalcs$vbt)
    # Hack the vbt series into sbt series in new model objects so that
    # we can use the `plot_ts_mcmc()` function to make this plot
    # with two different time series types
    hacked_vbt <- map(seq_len(num_vbt), ~{
      tmp <- model
      tmp$mcmccalcs$sbt_quants <- tmp$mcmccalcs$vbt_quants[[.x]]
      gear_names <- model$dat$fleet_gear_names
      attributes(tmp)$model_desc <- paste(gear_names[.x],
                                           tr("Vulnerable biomass"))
      tmp
    })
    models <- c(list(model), hacked_vbt)
    class(models) <- mdl_lst_cls
    plot_ts_mcmc(models,
                 quant_df = "sbt_quants",
                 angle_x_labels = angle_x_labels,
                 append_base_txt = append_base_txt,
                 leg_loc = leg_loc,
                 xlim = xlim,
                 ylim = ylim,
                 y_label = ifelse(fr(),
                                  "Biomasse (milliers de tonnes)",
                                  "Biomass (thousand t)"))
}
