#' Plot a model's time series (either spawning biomass or relaive spawning
#' biomass) and its projections
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param model An ISCAM model object (class [mdl_cls])
#' @param model_name Name of the input model
#' @param rel Logical. Make plot relative to initial estimate (B0), also known
#' as depletion
#' @param label_digits Number of digits to show in the catch labels
#' @param label_append_text Text to append to the labels
#' @param ... Arguments passed to [plot_ts_mcmc()]
#'
#' @export
plot_biomass_proj_mcmc <- function(model,
                                   model_name = "Base model",
                                   rel = FALSE,
                                   probs = c(0.025, 0.5, 0.975),
                                   x_space = 0.5,
                                   y_space = ifelse(rel, 0.05, 0.5),
                                   show_bo_lines = FALSE,
                                   bo_refpt_colors = c("red", "green"),
                                   line_width = 1,
                                   point_size = 2,
                                   label_digits = 2,
                                   label_append_text = " kt",
                                   ...){

  if(!is_iscam_model(model)){
    stop("`model` is not an ISCAM model, it should be class ",
         "gfiscamutils::mdl_cls which is 'iscam_model'",
         call. = FALSE)
  }

  # Get projections
  if(rel){
    series <- model$mcmccalcs$depl_quants |>
      as_tibble(rownames = "quants") |>
      filter(quants != "MPD")
    proj <- model$mcmccalcs$proj_depl_quants
  }else{
    series <- model$mcmccalcs$sbt_quants |>
      as_tibble(rownames = "quants") |>
      filter(quants != "MPD")
    proj <- model$mcmccalcs$proj_sbt_quants
  }

  # Make a list of time series matrices, one for each catch
  series_lst <- proj |>
    split(~catch) |>
    map(~{
      # Remove last year from the projections (they are the same in projections
      # and time series)
      #x <- .x[-(.x$year %in% names(series)), ]
      x <- .x
      yrs <- x |> pull(year)
      x <- x |>
        select(-c(catch, year)) |>
        t() |>
        as_tibble(rownames = "quants")
      names(x) <- c("quants", yrs)
      quant_nms <- x |> pull(quants)
      x <- x |>
        select(-quants) |>
        as.matrix()
      rownames(x) <- quant_nms
      x
    })

  # Make a copy of the model with `sbt` or `depl` replaced so the plotting function
  # is tricked into thinking these are separate models
  models <- imap(series_lst, ~{
    mdl <- model
    if(rel){
      mdl$mcmccalcs$depl_quants <- .x
    }else{
      mdl$mcmccalcs$sbt_quants <- .x
    }
    mdl
  })
  models <- c(list(model), models)
  names(models)[1] <- model_name

  models <- imap(models, ~{
    attributes(.x)$model_desc <- .y
    .x
  })
  class(models) <- mdl_lst_cls

  g <- plot_ts_mcmc(models,
                    quant_df = ifelse(rel,
                                      "depl_quants",
                                      "sbt_quants"),
                    y_label = ifelse(rel,
                                     ifelse(fr(), "Biomasse relative de frai", "Relative Spawning biomass"),
                                     ifelse(fr(), "Biomasse reproductrice (milliers de t)", "Spawning biomass (thousand t)")),
                    x_space = x_space,
                    y_space = y_space,
                    probs = probs,
                    line_width = line_width,
                    point_size = point_size,
                    ...)

  # Get end point coords
  labels <- map_dbl(series_lst, ~{
    .x <- as_tibble(.x, rownames = "quants")
    nc <- ncol(.x)
    .x <- .x |>
      filter(quants == "50%") |>
      pull() |>
      round(digits = label_digits)
  }) |>
    enframe() |>
    mutate(year = max(g$data$year) + 0.5) |>
    mutate(label = paste0(name, label_append_text))

  g <- g +
    geom_text(aes(x = year, y = value, label = label), data = labels, inherit.aes = FALSE)

  if(rel && show_bo_lines){
    g <- g +
      geom_hline(aes(yintercept = yintercept),
                 data = data.frame(yintercept = 0.2),
                 color = bo_refpt_colors[1],
                 lty = 1,
                 lwd = 1) +
      geom_hline(aes(yintercept = yintercept),
                 data = data.frame(yintercept = 0.4),
                 color = bo_refpt_colors[2],
                 lty = 2,
                 lwd = 1)
  }

  g
}
