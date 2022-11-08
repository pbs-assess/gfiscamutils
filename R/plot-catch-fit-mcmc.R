#' Plot catch fit against catch data
#'
#' @inheritParams plot_ts_mcmc
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @family Time series plotting functions
#'
#' @export
plot_catch_fit_mcmc <- function(model,
                                append_base_txt = " Spawning biomass",
                                xlim = NULL,
                                ylim = NULL,
                                leg_loc = c(0.95, 0.95),
                                angle_x_labels = FALSE,
                                probs = c(0.025, 0.5, 0.975)){

  if(!is_iscam_model(model)){
    stop("The `model` is not a ", mdl_cls, " class", call. = FALSE)
  }

  perc <- probs * 100
  perc[!perc %% 1] <- f(perc[!perc %% 1])
  perc <- paste0(perc, "%")

  ct_dat <- model$dat$catch |>
    as_tibble() |>
    group_by(year) |>
    summarize(value = sum(value))
  yrs <- ct_dat$year |> as.numeric()
  lst <- list()
  val_row <- ct_dat$value |> vec2df(nms = as.character(yrs))
  lst[[1]] <- val_row
  lst[[2]] <- val_row
  lst[[3]] <- val_row
  lst[[4]] <- val_row
  df <- do.call("rbind", lst) |> as.matrix()
  rownames(df) <- c(perc, "MPD")
  dat_model <- model

  dat_model$mcmccalcs$ct_quants <- df
  attributes(dat_model)$model_desc = "Catch data"
  attributes(model)$model_desc = "Catch fit"
  models <- c(list(model), list(dat_model))
  class(models) <- mdl_lst_cls

  plot_ts_mcmc(models,
               quant_df = "ct_quants",
               angle_x_labels = angle_x_labels,
               append_base_txt = append_base_txt,
               leg_loc = leg_loc,
               xlim = xlim,
               ylim = ylim,
               y_label = "Catch (thousand t)")
}
