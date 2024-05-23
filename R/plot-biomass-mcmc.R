#' Plot MCMC biomass for iSCAM models
#'
#' @description
#' Plot the MCMC spawning biomass trajectories for iscam models in either
#' absolute or relative form with or without reference point credible
#' intervals and medians
#'
#' @details
#' The reference point lines and credible intervals are output from the
#' first model in the list, as such they cannot be used to judge the other
#' model's status with respect to reference points. To see those, plot with
#' [plot_biomass_grid_mcmc()] instead. This will show one panel per model,
#' each with its own reference points.
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param rel Logical. Make plot relative to initial estimate (B0), also known
#' as depletion
#' @param show_bo Logical. If `TRUE` and `rel == FALSE`, show the median
#' initial value on the plot (B0) as a point with the credible intervals as
#' bars
#' @param refpts_ribbon Logical. If `TRUE`, make the first model's reference
#' points lines (`show_bo_lines` and/or `show_bmsy_lines` must be `TRUE`)
#' plotted an envelope of the credible interval, surrounding the median lines
#' for the reference points
#' @param refpts_alpha The transparency of the envelope shown for reference
#' points between 0 and 1. Only used when `refpts_ribbon` is `TRUE` and
#' `show_bo_lines` and/or `show_bmsy_lines` are `TRUE`
#' @param show_bo_lines Show the B0 lines at values given by `bo_refpts` for
#' the first model in the `models` list
#' @param show_bmsy_lines Show the BMSY lines at values given by `bmsy_refpts`
#' for the first model in the `models` list
#' @param bo_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.2B0 and 0.4B0 by default
#' @param bo_dodge The amount to offset the initial value (B0 or R0) values
#' from each other so the values and uncertainty can be easily seen for
#' multiple models
#' @param bmsy_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.4BMSY and 0.8BMSY by default
#' @param bo_refpt_colors A vector of two colors representing the LRP and USR
#' for B0. Used to display reference point lines if `show_bo_lines` is `TRUE`
#' @param bmsy_refpt_colors A vector of two colors representing the LRP and USR
#' for BMSY. Used to display reference point lines if `show_bmsy_lines` is `TRUE`
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @param ylim A vector of two values, representing the minimum and maximum
#' values for the plot on the y-axis. If `NULL`, defaults will be zero for the
#' minimum and the ceiling of the maximum value (including CI)
#' @param ... Arguments passed to [plot_ts_mcmc()]
#'
#' @importFrom tibble rownames_to_column
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @importFrom gginnards move_layers
#' @export
plot_biomass_mcmc <- function(models,
                              rel = FALSE,
                              show_bo = TRUE,
                              probs = c(0.025, 0.5, 0.975),
                              refpts_ribbon = TRUE,
                              refpts_alpha = 0.2,
                              bo_dodge = 0.1,
                              x_space = 0.5,
                              y_space = ifelse(rel, 0.05, 0.5),
                              show_bo_lines = FALSE,
                              bo_refpts = c(0.2, 0.4),
                              show_bmsy_lines = FALSE,
                              bmsy_refpts = c(0.4, 0.8),
                              bo_refpt_colors = c("red", "green"),
                              bmsy_refpt_colors = c("salmon", "darkgreen"),
                              angle_x_labels = FALSE,
                              ylim = NULL,
                              line_width = 1,
                              point_size = 2,
                              ...){

  g <- plot_ts_mcmc(
    models,
    quant_df = ifelse(
      rel,
      "depl_quants",
      "sbt_quants"),
    y_label = ifelse(
      rel,
      ifelse(fr(),
             "Biomasse relative de frai", "Relative Spawning biomass"),
      ifelse(fr(),
             "Biomasse reproductrice (milliers de t)", "Spawning biomass (thousand t)")),
    x_space = x_space,
    y_space = y_space,
    probs = probs,
    line_width = line_width,
    point_size = point_size,
    ...)

  if(is_iscam_model(models)){
    model_desc <- attributes(models)$model_desc
    models <- list(models)
    class(models) <- mdl_lst_cls
    names(models) <- model_desc
  }

  start_yr <- min(g$data$year)
  end_yr <- max(g$data$year)
  # Only used when `rel` is `TRUE`
  bo_yr <- start_yr - 1

  tso_quants <- map(models, ~{
    .x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "sbo"]
    }) |>
    bind_rows() |>
    mutate(model = names(models), year = ifelse(show_bo, start_yr - 1, start_yr)) |>
    select(model, year, everything()) |>
    convert_prob_cols_language()

  bmsy_quants <- map(models, ~{
    .x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "bmsy"]
    }) |>
    bind_rows() |>
    mutate(model = names(models), year = start_yr) |>
    select(model, year, everything()) |>
    convert_prob_cols_language()

  # Remove data prior to first year and change B0 to first year
  tso_quants <- tso_quants |>
    mutate(year = ifelse(show_bo, start_yr - 1, start_yr))
  bmsy_quants <- bmsy_quants |>
    mutate(year = start_yr)

  # 'Dodge' B0 points manually
  if((nrow(tso_quants) - 1) * bo_dodge >= 1){
    warning("`bo_dodge` value of ", bo_dodge, " makes B0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }

  tso_quants <- tso_quants %>%
    mutate(year = seq(from = first(year), by = bo_dodge, length.out = nrow(.)))

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")

  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(tso_quants), value = TRUE)

    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC ",
      "output data\n", .x)
    }
    mtch
  })

  tso_base <- tso_quants |>
    slice(1)

  if(show_bo_lines){
    # Show the B0 lines for the first model with CI, behind model lines
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    tso_multiples <- imap(bo_refpts, ~{
      j <- tso_base |>
        mutate(!!sym(quants[1]) := !!sym(quants[1]) * .x,
               !!sym(quants[2]) := !!sym(quants[2]) * .x,
               !!sym(quants[3]) := !!sym(quants[3]) * .x) |>
        mutate(multiplier = .x)
      j
    }) |>
      bind_rows()

    if(rel){
      # Need special calculation for the Credible interval for the
      # relative biomass
      mdl_yr <- tso_base |>
        select(model, year)
      tso_base_quants <- tso_base |>
        select(-model, -year) |>
        unlist()
      tso_mult_list <- map(bo_refpts, ~{
        .x * tso_base_quants / tso_base_quants[2]
      })

      # Make data frame rows for B0 ref points.
      # This is convoluted because if done in a simpler way, the french
      # values with commas will prevent the columns from being cast to
      # numeric
      model_yr_cols <- tibble(model = mdl_yr$model,
                              year = mdl_yr$year)
      row1_prob_cols <- imap_dfc(tso_mult_list[[1]], ~{
        tibble(!!.y := .x)
      })
      row1 <- model_yr_cols |> bind_cols(row1_prob_cols)
      row2_prob_cols <- imap_dfc(tso_mult_list[[2]], ~{
        tibble(!!.y := .x)
      })
      row2 <- model_yr_cols |> bind_cols(row2_prob_cols)

      tso_multiples <- row1 |>
        bind_rows(row2) |>
        mutate(multiplier = tso_multiples$multiplier)
    }

    if(refpts_ribbon){
      if(!rel){
        g <- g +
          geom_rect(data = tso_multiples,
                    aes(xmin = ifelse(rel || !show_bo, start_yr, bo_yr),
                        xmax = end_yr),
                    alpha = refpts_alpha,
                    fill = bo_refpt_colors)
      }
      g <- g +
        geom_hline(data = tso_multiples[1, ],
                   aes(yintercept = !!sym(quants[2])),
                   color = bo_refpt_colors[1],
                   lty = 1,
                   lwd = 1) +
        geom_hline(data = tso_multiples[2, ],
                   aes(yintercept = !!sym(quants[2])),
                   color = bo_refpt_colors[2],
                   lty = 2,
                   lwd = 1)
    }else{
      if(!rel){
        g <- g +
          geom_hline(data = tso_multiples,
                     aes(yintercept = !!sym(quants[1])),
                     color = bo_refpt_colors,
                     lty = 4,
                     lwd = 1) +
          geom_hline(data = tso_multiples,
                     aes(yintercept = !!sym(quants[3])),
                     color = bo_refpt_colors,
                     lty = 4,
                     lwd = 1)
      }
      g <- g +
        geom_hline(data = tso_multiples,
                   aes(yintercept = !!sym(quants[2])),
                   color = bo_refpt_colors,
                   lty = c(1, 2),
                   lwd = 1)
    }
  }

  if(show_bmsy_lines){

    # Show the BMSY lines for the first model with CI, behind model lines
    bmsy_base <- bmsy_quants %>%
      slice(1)
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    if(rel){
      bmsy_multiples <- imap(bmsy_refpts, ~{
        bmsy_base %>%
          mutate(!!sym(quants[1]) := !!sym(quants[1]) /
                   unlist(tso_base[1, quants[1]]) * .x,
                 !!sym(quants[2]) := !!sym(quants[2]) /
                   unlist(tso_base[1, quants[2]]) * .x,
                 !!sym(quants[3]) := !!sym(quants[3]) /
                   unlist(tso_base[1, quants[3]]) * .x) %>%
          mutate(multiplier = .x)
      }) %>%
        bind_rows
    }else{
      bmsy_multiples <- imap(bmsy_refpts, ~{
        bmsy_base %>%
          mutate(!!sym(quants[1]) := !!sym(quants[1]) * .x,
                 !!sym(quants[2]) := !!sym(quants[2]) * .x,
                 !!sym(quants[3]) := !!sym(quants[3]) * .x) %>%
          mutate(multiplier = .x)
      }) %>%
        bind_rows
    }

    if(refpts_ribbon){
      g <- g +
        geom_rect(data = bmsy_multiples,
                  aes(xmin = ifelse(rel || !show_bo, start_yr, bo_yr),
                      xmax = end_yr),
                  alpha = refpts_alpha,
                  fill = bmsy_refpt_colors) +
        geom_hline(data = bmsy_multiples[1, ],
                   aes(yintercept = !!sym(quants[2])),
                   color = bmsy_refpt_colors[1],
                   lty = 1,
                   lwd = 1) +
        geom_hline(data = bmsy_multiples[2, ],
                   aes(yintercept = !!sym(quants[2])),
                   color = bmsy_refpt_colors[2],
                   lty = 2,
                   lwd = 1)
    }else{
      g <- g +
        geom_hline(data = bmsy_multiples,
                   aes(yintercept = !!sym(quants[1])),
                   color = bmsy_refpt_colors,
                   lty = 4) +
        geom_hline(data = bmsy_multiples,
                   aes(yintercept = !!sym(quants[2])),
                   color = bmsy_refpt_colors,
                   lty = 1) +
        geom_hline(data = bmsy_multiples,
                   aes(yintercept = !!sym(quants[3])),
                   color = bmsy_refpt_colors,
                   lty = 4)
    }
  }

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

  if(show_bo_lines){
    # Add the text labels to the y-axis ticks for the reference point levels
    if(any(bo_refpts %in% brk)){
      wch <- which(brk %in% bo_refpts)
      brk <- brk[-wch]
    }
    brk <- sort(c(brk, tso_multiples[[quants[2]]]))
    lbl <- brk
    wch <- which(brk %in% tso_multiples[[quants[2]]])
    if(length(wch) != 2){
      stop("Could not find the B0 reference points in the tso_multiplier ",
           "data frame. See function code", call. = FALSE)
    }
    lbl[wch][1] <- as.expression(bquote(.(tso_multiples$multiplier[1]) ~ B[0]))
    lbl[wch][2] <- as.expression(bquote(.(tso_multiples$multiplier[2]) ~ B[0]))
    cols <- rep("black", length(brk))
    cols[wch] <- bo_refpt_colors
  }
  if(show_bmsy_lines){
    # Add the text labels to the y-axis ticks for the reference point levels
    if(show_bo_lines){
      # The labels and breaks have already be created so start with those
      brk <- sort(c(brk, bmsy_multiples[[quants[2]]]))
      wch <- which(brk %in% bmsy_multiples[[quants[2]]])
      if(length(wch) != 2){
        stop("Could not find the B0 reference points in the tso_multiplier ",
             "data frame. See function code for case where both ",
             "show_bo_lines and show_msy_lines are enabled",
             call. = FALSE)
      }
      lbl <- append(lbl,
               as.expression(bquote(.(bmsy_multiples$multiplier[1]) ~ B[MSY])),
               after = wch[1] - 1)
      lbl <- append(lbl,
               as.expression(bquote(.(bmsy_multiples$multiplier[2]) ~ B[MSY])),
               after = wch[2] - 1)
      cols <- append(cols, bmsy_refpt_colors[1], after = wch[1] - 1)
      cols <- append(cols, bmsy_refpt_colors[2], after = wch[2] - 1)
    }else{
      # Start from non-modified labels and breaks
      brk <- sort(c(brk, bmsy_multiples[[quants[2]]]))
      lbl <- brk
      wch <- which(brk %in% bmsy_multiples[[quants[2]]])
      if(length(wch) != 2){
        stop("Could not find the BMSY reference points in the bmsy_multiplier ",
             "data frame. See function code", call. = FALSE)
      }
      lbl[wch][1] <- as.expression(bquote(.(bmsy_multiples$multiplier[1]) ~ B[MSY]))
      lbl[wch][2] <- as.expression(bquote(.(bmsy_multiples$multiplier[2]) ~ B[MSY]))
      cols <- rep("black", length(brk))
      cols[wch] <- bmsy_refpt_colors
    }
  }

  g <- g +
    scale_y_continuous(limits = lims,
                       breaks = brk,
                       labels = lbl,
                       expand = expansion(add = y_space)) +
    theme(axis.text.y = element_text(color = cols),
          axis.ticks.y = element_line(color = cols))

  if(rel || !show_bo){
    g <- g +
      scale_x_continuous(limits = c(start_yr, end_yr),
                         breaks = start_yr:end_yr,
                         labels = start_yr:end_yr,
                         expand = expansion(add = x_space))
  }else{
    g <- g +
      scale_x_continuous(limits = c(bo_yr, end_yr),
                         breaks = bo_yr:end_yr,
                         labels = c(expression(B[0]), start_yr:end_yr),
                         expand = expansion(add = x_space))
  }

  if(!rel && show_bo){
    # If the upper bound of the line is past the upper bound of the plot,
    # the line will fail to plot so modify the line limit in the data
    tso_quants <- tso_quants |>
      mutate(!!sym(quants[3]) := ifelse(!!sym(quants[3]) > upper_bound,
                                        upper_bound,
                                        !!sym(quants[3])))

    g <- g +
      geom_pointrange(data = tso_quants,
                      aes(color = model),
                      lwd = line_width,
                      fatten = point_size,
                      size = point_size)
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g <- g +
    guides(colour = guide_legend(override.aes = list(size = 0.5, fatten = 1)))

  # Move the B0 and BMSY lines and shaded areas behind the models
  # This is necessary to have a generic plotting function(plot_ts_mcmc)
  # as that plot has to be made first to make the code simpler
  g <- move_layers(g, "GeomHline", 0L)
  g <- move_layers(g, "GeomRect", 0L)

  g
}
