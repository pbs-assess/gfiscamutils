#' Plot MCMC vulnerable biomass for iSCAM models
#'
#' @description
#' Plot vulnerable biomass time series plots for MCMC output with credible
#' intervals
#'
#' @inheritParams plot_ts_mcmc
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' minimum and the ceiling of the maximum value (including CI)
#' @param units One of "1000 t" or "kt". The text that will appear in the
#' y-axis label. The "1000 t" text will be changed to "1,000 t" or "1 000 t"
#' for English or French respectively
#' @family Time series plotting functions
#'
#' @export
plot_vuln_mcmc <- function(model,
                           append_base_txt = tr("Spawning biomass"),
                           xlim = NULL,
                           ylim = NULL,
                           leg_loc = NULL,
                           angle_x_labels = FALSE,
                           units = c("kt", "1000 t")){

  units <- match.arg(units)
  if(fr()){
    if(units == "1000 t"){
      units <- "1 000 t"
    }
  }else{
    if(units == "1000 t"){
      units <- "1,000 t"
    }
  }

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
                 y_label = paste0(tr("Biomass"), " (", units, ")"))
}
