#' Plot catch fit against catch data
#'
#' @inheritParams plot_ts_mcmc
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @family Time series plotting functions
#'
#' @export
plot_catch_fit_mcmc <- function(model,
                                append_base_txt = "",
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

  # List of length = number of fleets, catch tibble with year and value
  ct_dat <- model$dat$catch |>
    as_tibble() |>
    split(~ gear) |>
    map(~{.x |> select(year, value)})

  # List of length = number of fleets, tibble of catch values, fake quants
  # with year as colnames
  ct_dat_byfleet <- ct_dat |>
    map(~{
        lst <- list()
        val_row <- vec2df(.x$value, nms = as.character(.x$year))
        lst[[1]] <- val_row
        lst[[2]] <- val_row
        lst[[3]] <- val_row
        lst[[4]] <- val_row
        df <- do.call("rbind", lst) |> as.matrix()
        rownames(df) <- c(perc, "MPD")
        df
      })

  ct_fit_byfleet <- model$mcmccalcs$ct_quants

  # List of length = number of fleets, tibble of catch fits
  # with year as colnames
  if(length(ct_fit_byfleet) != length(ct_dat_byfleet)){
    stop("Number of fleets in catch data not the same as in the catch ",
         "fit output", call = FALSE)
  }

  fleet_nms <- model$dat$fleet_gear_names

  # List of length = number of fleets, of models, 1 for each fleet
  # containing the catch data replacing mcmccalcs$ct_quants
  models_dat_byfleet <- ct_dat_byfleet |>
    imap(~{
      tmp <- model
      tmp$mcmccalcs$ct_quants <- .x
      if(french){
        attributes(tmp)$model_desc <- paste0("Données sur les captures - ",
                                             fleet_nms[as.numeric(.y)])
      }else{
        attributes(tmp)$model_desc <- paste0("catch data - ",
                                             fleet_nms[as.numeric(.y)])
      }
      tmp
    })

  # List of length = number of fleets, of models, 1 for each fleet
  # containing the catch fits replacing mcmccalcs$ct_quants
  models_fit_byfleet <- ct_fit_byfleet |>
    imap(~{
      tmp <- model
      tmp$mcmccalcs$ct_quants <- .x
      if(french){
        attributes(tmp)$model_desc <- paste0("Attraper l'ajustement - ",
                                             fleet_nms[as.numeric(.y)])
      }else{
        attributes(tmp)$model_desc <- paste0("Catch fit - ",
                                             fleet_nms[as.numeric(.y)])
      }
      tmp
    })

  models <- c(models_dat_byfleet,
              models_fit_byfleet)
  class(models) <- mdl_lst_cls

  plot_ts_mcmc(models,
               quant_df = "ct_quants",
               angle_x_labels = angle_x_labels,
               append_base_txt = append_base_txt,
               leg_loc = leg_loc,
               xlim = xlim,
               ylim = ylim,
               y_label = ifelse(fr(),
                                "Captures (milliers de tonnes)",
                                "Catch (thousand t)"),
               legend_title = ifelse(fr(),
                                     "Flotte/Type",
                                     "Fleet/Type"))
}
