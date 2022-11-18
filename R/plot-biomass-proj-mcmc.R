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
#' @param label_font_size Size of the catch label font. See `size` argument in
#' [ggplot2::geom_text()]
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
                                   bo_refpts = c(0.2, 0.4),
                                   bo_refpt_colors = c("red", "green"),
                                   line_width = 1,
                                   point_size = 2,
                                   label_digits = 2,
                                   label_append_text = " kt",
                                   label_font_size = 8,
                                   angle_x_labels = FALSE,
                                   ylim = NULL,
                                   xlim = NULL,
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

  if(!is.null(xlim)){
    # Remove years past max xlim
    series_lst   <- series_lst |>
      map(~{
        row_nms <- rownames(.x)
        x <- .x |> as_tibble()
        x <- x[, as.numeric(names(x)) %in% xlim[1]:xlim[2]] |>
          as.matrix()
        rownames(x) <- row_nms
        x
      })
  }

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
                    xlim = c(xlim[1], xlim[2] + 1),
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
    geom_text(aes(x = year,
                  y = value,
                  label = label),
              data = labels,
              inherit.aes = FALSE,
              position = position_nudge(x = 0.3, y = 0),
              size = label_font_size)

  # Create tags for B0 lines, and breaks and labels for y-axis
  if(is.null(ylim)){
    if(rel){
      ymax <- max(select(g$data, -c(model, year)))
    }else{
      ymax <- max(select(g$data, -c(model, year)),
                  select(tso_quants, -c(model, year)))
    }
    upper_bound <- ceiling(ymax)
    lims <- c(0, upper_bound)
  }else{
    upper_bound <- ylim[2]
    lims <- ylim
  }
  brk <- seq(0, upper_bound, upper_bound / 10)
  lbl <- brk
  cols <- rep("black", length(brk))


  if(rel && show_bo_lines){
    g <- g +
      geom_hline(aes(yintercept = yintercept),
                 data = data.frame(yintercept = bo_refpts[1]),
                 color = bo_refpt_colors[1],
                 lty = 1,
                 lwd = 1) +
      geom_hline(aes(yintercept = yintercept),
                 data = data.frame(yintercept = bo_refpts[2]),
                 color = bo_refpt_colors[2],
                 lty = 2,
                 lwd = 1)

    # Add the text labels to the y-axis ticks for the reference point levels
    if(any(bo_refpts %in% brk)){
      wch <- which(brk %in% bo_refpts)
      brk <- brk[-wch]
    }
    brk <- sort(c(brk, bo_refpts))
    lbl <- brk
    wch <- which(brk %in% bo_refpts)
    if(length(wch) != 2){
      stop("Problem with the `bo_refpts` vector. ",
           "See function code", call. = FALSE)
    }
    lbl[wch][1] <- as.expression(bquote(.(bo_refpts[1]) ~ B[0]))
    lbl[wch][2] <- as.expression(bquote(.(bo_refpts[2]) ~ B[0]))
    cols <- rep("black", length(brk))
    cols[wch] <- bo_refpt_colors
  }

  g <- g +
    scale_y_continuous(limits = lims,
                       breaks = brk,
                       labels = lbl,
                       expand = expansion(add = y_space)) +
    theme(axis.text.y = element_text(color = cols),
          axis.ticks.y = element_line(color = cols))

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  # Move the B0 and BMSY lines and shaded areas behind the models
  # This is necessary to have a generic plotting function(plot_ts_mcmc)
  # as that plot has to be made first to make the code simpler
  g <- move_layers(g, "GeomHline", 0L)
  g <- move_layers(g, "GeomRect", 0L)

  g
}
