#' Plot the MCMC spawning biomass trajectories for iscam models in either
#' absolute or relative form.
#'
#' @details Cannot make more than one model have a shaded credible interval. It is
#' too difficult to tell what's going on with colors overlapping.If called with
#' `rel = TRUE`, `show_bo` will be overridden to be `FALSE`. It makes no sense
#' to show B0 when a relative plot is asked for, it is always 1.
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param model_names Names to use for the models in the plots. The names of
#' the list items in `models` will be used if they are present and this will
#' be ignored. If the list item names are not defined, temporary names will be used
#' (Temporary model 1, Temporary model 2, etc.)
#' @param rel Logical. Make plot relative to initial estimate (B0), also known as depletion
#' @param show_bo Logical. If `TRUE` and `rel == FALSE`, show the initial value
#' on the plot (B0)
#' @param legend_title Title for legend
#' @param xlim The x limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param ylim The y limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param line_width Width of all median lines on the plot
#' @param point_size Point size for all median points on the plot
#' @param first_model_ribbon Logical. If `TRUE`, give the first model a shaded
#' credible interval instead of dotted lines
#' @param refpts_ribbon Logical. If `TRUE`, make the first model's reference points lines
#' (`show_bo_lines` and/or `show_bmsy_lines` must be `TRUE`) plotted an envelope
#' of the credible interval, surrounding the median lines for the reference points
#' @param alpha The opacity between 0 to 1 of the envelope shown when `first_model_ribbon == TRUE`
#' @param refpts_alpha The opacity between 0 to 1 of the envelope shown for referece points
#' when `refpts_ribbon == TRUE` and `show_bo_lines` and/or `show_bmsy_lines` are `TRUE`
#' @param palette A palette value that is in [RColorBrewer::brewer.pal.info]
#' @param base_color A color to prepend to the brewer colors which are set by `palette`.
#' This is called `base_color` because it is likely to be a base model
#' @param bo_dodge The amount to offset the initial value (B0 or R0) values from each
#' other so the values and uncertainty can be easily seen for multiple models
#' @param x_space The amount of x-interval space to pad the left and right of the plot
#' with. To remove all padding, make this 0
#' @param append_base_txt Text to append to the first model's name for display on the
#' plot legend
#' @param show_bo_lines Show the B0 lines at values given by `bo_refpts` for the
#' first model in the `models` list
#' @param show_bmsy_lines Show the BMSY lines at values given by `bmsy_refpts` for the
#' first model in the `models` list
#' @param bo_refpts Vector of two proportional values for the limit reference point
#' and Upper stock reference. Values are 0.2B0 and 0.4B0 by default
#' @param bmsy_refpts Vector of two proportional values for the limit reference point
#' and Upper stock reference. Values are 0.4BMSY and 0.8BMSY by default
#' @param bo_refpt_colors A vector of two colors representing the LRP and USR for B0.
#' Used to display reference point lines if `show_bo_lines == TRUE`
#' @param bmsy_refpt_colors A vector of two colors representing the LRP and USR for BMSY.
#' Used to display reference point lines if `show_bmsy_lines == TRUE`
#' @param ind_letter A letter to place in the upper left corner of the plot. If `NULL`,
#' nothing will be shown
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param leg_loc See the `legend.position` argument in [ggplot2::theme()]. Use "none"
#' to remove legend completely
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @param ... Other graphical arguments
#'
#' @family Time series plotting functions
#' @return A [ggplot2::ggplot()] object
#' @importFrom tibble rownames_to_column
#' @importFrom RColorBrewer brewer.pal
#' @importFrom forcats fct_relevel
#' @export
plot_biomass_mcmc <- function(models,
                              model_names = NULL,
                              rel = FALSE,
                              show_bo = TRUE,
                              legend_title = "Models",
                              xlim = NULL,
                              ylim = NULL,
                              line_width = 1,
                              point_size = 2,
                              first_model_ribbon = TRUE,
                              refpts_ribbon = TRUE,
                              alpha = 0.2,
                              refpts_alpha = alpha,
                              palette = "Paired",
                              base_color = "black",
                              bo_dodge = 0.1,
                              x_space = 0.5,
                              append_base_txt = NULL,
                              show_bo_lines = FALSE,
                              bo_refpts = c(0.2, 0.4),
                              show_bmsy_lines = FALSE,
                              bmsy_refpts = c(0.4, 0.8),
                              bo_refpt_colors = c("red", "green"),
                              bmsy_refpt_colors = c("salmon", "darkgreen"),
                              ind_letter = NULL,
                              leg_loc = NULL,
                              probs = c(0.025, 0.5, 0.975),
                              angle_x_labels = FALSE,
                              ...){

  if(class(models) == mdl_cls){
    models <- list(models)
    class(models) <- mdl_lst_cls
  }

  if(class(models) != mdl_lst_cls){
    stop("The `models` list is not a gfiscamutils::mdl_lst_cls class. If you are trying to plot ",
         "a single model, modify it like this first:\n\n",
         "model <- list(model)\n",
         "class(model) <- mdl_lst_cls\n")
  }

  if(!palette %in% rownames(brewer.pal.info)){
    stop("`palette` name not found in `RColorBrewer::brewer.pal.info`")
  }

  palette_info <- brewer.pal.info[rownames(brewer.pal.info) == palette, ]

  if(length(models) > palette_info$maxcolors){
    stop("Cannot plot more than ", palette_info$maxcolors, " models because that is the ",
         "maximum number for the ", palette, " palette")
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  # Set up model names for the legend
  if(is.null(model_names)){
    if(is.null(names(models))){
      names(models) <- paste0("model ", seq_along(models))
    }
  }else{
    if(length(model_names) != length(models)){
      stop("`model_names` is not the same length as the `models` list")
    }
    names(models) <- model_names
  }

  if(rel){
    show_bo <- FALSE
  }
  start_yr <- map_dbl(models, ~{.x$dat$start.yr}) %>% min
  end_yr <- map_dbl(models, ~{.x$dat$end.yr}) %>% max
  if(is.null(xlim)){
    xlim <- c(start_yr, end_yr)
  }
  len <- end_yr - start_yr + 1
  bind_yrs <- start_yr:end_yr

  val <- ifelse(rel, "depl_quants", "sbt_quants")
  ts_quants <- map(models, ~{
    j <- .x$mcmccalcs[[val]]
    if(len < ncol(j)){
      j <- j[, 1:len]
    }
    j
  })
  tso_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "sbo"]})
  bmsy_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "bmsy"]})

  nms <- names(ts_quants)
  if(is.null(nms)){
    if(is.null(model_names)){
      nms <- paste0("Temporary model ", seq_len(length(ts_quants)), append_base_txt)
    }else{
      if(length(model_names) != length(ts_quants)){
        stop("`model_names` is not the same length as the number of models supplied in `models`")
      }else{
        nms <- model_names
        nms[1] <- paste0(nms[1], append_base_txt)
      }
    }
    names(ts_quants) <- nms
    names(tso_quants) <- nms
    names(bmsy_quants) <- nms
  }else{
    names(ts_quants)[1] <- paste0(names(ts_quants)[1], append_base_txt)
    names(tso_quants)[1] <- paste0(names(tso_quants)[1], append_base_txt)
  }

  nms <- names(ts_quants)
  tso_quants <- tso_quants %>%
    bind_rows() %>%
    mutate(model = nms, year = ifelse(show_bo, start_yr - 1, start_yr)) %>%
    select(model, year, everything())
  bmsy_quants <- bmsy_quants %>%
    bind_rows() %>%
    mutate(model = nms, year = start_yr) %>%
    select(model, year, everything())

  ts_quants <- imap(ts_quants, ~{
    .x %>%
      t() %>%
      as.data.frame %>%
      rownames_to_column(var = "year") %>%
      mutate(model = .y) %>%
      select(model, year, everything()) %>%
      mutate(year = as.numeric(year))
  }) %>%
    bind_rows %>%
    select(-MPD)

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(ts_quants), value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data: ", .x)
    }
    mtch
  })

  y_label <- ifelse(rel, "Relative Spawning biomass", "Spawning biomass ('000 tonnes)")
  if(!is.null(xlim)){
    # Remove data prior to first year and change B0/R0 to first
    tso_quants <- tso_quants %>%
      mutate(year = ifelse(show_bo, xlim[1] - 1, xlim[1]))
    bmsy_quants <- bmsy_quants %>%
      mutate(year = xlim[1])
    ts_quants <- ts_quants %>%
      filter(year %in% xlim[1]:xlim[2])
  }
  ts_quants <- ts_quants %>%
    mutate(model = fct_relevel(model, nms))

  # 'Dodge' B0 points manually
  if((nrow(tso_quants) - 1) * bo_dodge >= 1){
    warning("`bo_dodge` value of ", bo_dodge, " makes B0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }
  tso_quants <- tso_quants %>%
    mutate(year = seq(from = first(year), by = bo_dodge, length.out = nrow(.)))

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  g <- ts_quants %>%
    ggplot(aes(x = year,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab("Year") +
    ylab(y_label) +
    scale_color_manual(values = model_colors)

  tso_base <- tso_quants %>%
    slice(1)
  if(show_bo_lines){
    # Show the B0 lines for the first model with CI, behind model lines
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    tso_multiples <- imap(bo_refpts, ~{
      tso_base %>%
        mutate(!!sym(quants[1]) := !!sym(quants[1]) * .x,
               !!sym(quants[2]) := !!sym(quants[2]) * .x,
               !!sym(quants[3]) := !!sym(quants[3]) * .x) %>%
        mutate(multiplier = .x)
    }) %>%
      bind_rows

    if(rel){
      # Need special calculation for the Credible interval for the
      # relative biomass
      mdl_yr <- tso_base %>%
        select(model, year)
      tso_base_quants <- tso_base %>%
        select(-model, -year) %>%
        unlist
      tso_mult_list <- map(bo_refpts, ~{
        .x * tso_base_quants / tso_base_quants[2]
      })
      row1 <- c(unlist(mdl_yr), tso_mult_list[[1]]) %>% t() %>% as_tibble()
      row2 <- c(unlist(mdl_yr), tso_mult_list[[2]]) %>% t() %>% as_tibble()
      tso_multiples <- row1 %>%
        bind_rows(row2) %>%
        mutate(multiplier = tso_multiples$multiplier,
               year = as.numeric(year),
               !!sym(quants[1]) := as.numeric(!!sym(quants[1])),
               !!sym(quants[2]) := as.numeric(!!sym(quants[2])),
               !!sym(quants[3]) := as.numeric(!!sym(quants[3])))
    }

    if(refpts_ribbon){
      g <- g +
        geom_rect(data = tso_multiples,
                  aes(xmin = ifelse(show_bo, start_yr - 1, start_yr), xmax = end_yr),
                  alpha = refpts_alpha,
                  fill = bo_refpt_colors) +
        geom_hline(data = tso_multiples,
                   aes(yintercept = !!sym(quants[2])),
                   color = bo_refpt_colors, lty = 1, lwd = 1)
    }else{
      g <- g +
        geom_hline(data = tso_multiples,
                   aes(yintercept = !!sym(quants[1])),
                   color = bo_refpt_colors,
                   lty = 4) +
        geom_hline(data = tso_multiples,
                   aes(yintercept = !!sym(quants[2])),
                   color = bo_refpt_colors,
                   lty = 1) +
        geom_hline(data = tso_multiples,
                   aes(yintercept = !!sym(quants[3])),
                   color = bo_refpt_colors,
                   lty = 4)
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
          mutate(!!sym(quants[1]) := !!sym(quants[1]) / unlist(tso_base[1, quants[1]]) * .x,
                 !!sym(quants[2]) := !!sym(quants[2]) / unlist(tso_base[1, quants[2]]) * .x,
                 !!sym(quants[3]) := !!sym(quants[3]) / unlist(tso_base[1, quants[3]]) * .x) %>%
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
                  aes(xmin = ifelse(show_bmsy_lines, start_yr - 1, start_yr), xmax = end_yr),
                  alpha = refpts_alpha,
                  fill = bmsy_refpt_colors) +
        geom_hline(data = bmsy_multiples,
                   aes(yintercept = !!sym(quants[2])),
                   color = bmsy_refpt_colors, lty = 1, lwd = 1)
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

  if(rel){
    g <- g + scale_x_continuous(limits = c(xlim[1], xlim[2]),
                                breaks = min(xlim):max(xlim),
                                labels = xlim[1]:xlim[2],
                                expand = expansion(add = x_space))
  }else{
    if(show_bo){
      g <- g + scale_x_continuous(limits = c(xlim[1] - 1, xlim[2]),
                                  breaks = (min(xlim) - 1):max(xlim),
                                  labels = c(expression(B[0]), xlim[1]:xlim[2]),
                                  expand = expansion(add = x_space))
    }else{
      g <- g + scale_x_continuous(limits = c(xlim[1], xlim[2]),
                                  breaks = min(xlim):max(xlim),
                                  labels = xlim[1]:xlim[2],
                                  expand = expansion(add = x_space))
    }
  }

  # Create tags for B0 lines, and breaks and labels for y-axis
  if(rel){
    ymax <- max(select(ts_quants, -c(model, year)))
  }else{
    ymax <- max(select(ts_quants, -c(model, year)),
                select(tso_quants, -c(model, year)))
  }
  if(ymax <= 1){
    upper_bound <- 1
  }else{
    upper_bound <- ifelse(ymax <= 10,
                          max(ymax %/% 2) * 2 + 2,
                          max(ymax %/% 100) * 100 + 100)
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
      stop("Could not find the B0 reference points in the tso_multiplier data frame. See function code")
    }
    lbl[wch][1] <- as.expression(bquote(.(tso_multiples$multiplier[1]) ~ B[0]))
    lbl[wch][2] <- as.expression(bquote(.(tso_multiples$multiplier[2]) ~ B[0]))
    cols <- rep("black", length(brk))
    cols[wch] <- bo_refpt_colors
  }
  if(show_bmsy_lines){
    # Add the text labels to the y-axis ticks for the reference point levels
    if(show_bo_lines){
      # The labels and breaks have already be created so we have to go from those
      brk <- sort(c(brk, bmsy_multiples[[quants[2]]]))
      wch <- which(brk %in% bmsy_multiples[[quants[2]]])
      if(length(wch) != 2){
        stop("Could not find the B0 reference points in the tso_multiplier data frame. ",
             "See function code for case where both show_bo_lines and show_msy_lines are enabled")
      }
      lbl <- append(lbl, as.expression(bquote(.(bmsy_multiples$multiplier[1]) ~ B[MSY])), after = wch[1] - 1)
      lbl <- append(lbl, as.expression(bquote(.(bmsy_multiples$multiplier[2]) ~ B[MSY])), after = wch[2] - 1)
      cols <- append(cols, bmsy_refpt_colors[1], after = wch[1] - 1)
      cols <- append(cols, bmsy_refpt_colors[2], after = wch[2] - 1)
    }else{
      # Start from non-modified labels and breaks
      brk <- sort(c(brk, bmsy_multiples[[quants[2]]]))
      lbl <- brk
      wch <- which(brk %in% bmsy_multiples[[quants[2]]])
      if(length(wch) != 2){
        stop("Could not find the BMSY reference points in the bmsy_multiplier data frame. See function code")
      }
      lbl[wch][1] <- as.expression(bquote(.(bmsy_multiples$multiplier[1]) ~ B[MSY]))
      lbl[wch][2] <- as.expression(bquote(.(bmsy_multiples$multiplier[2]) ~ B[MSY]))
      cols <- rep("black", length(brk))
      cols[wch] <- bmsy_refpt_colors
    }
  }

  if(is.null(ylim)){
    g <- g +
      scale_y_continuous(limits = c(0, upper_bound),
                         breaks = brk,
                         labels = lbl,
                         expand = c(0, 0)) +
      theme(axis.text.y = element_text(color = cols),
            axis.ticks.y = element_line(color = cols))
  }else{
    g <- g +
      scale_y_continuous(limits = c(0, NA),
                         breaks = brk,
                         labels = lbl,
                         expand = c(0, 0)) +
      theme(axis.text.y = element_text(color = cols),
            axis.ticks.y = element_line(color = cols)) +
      coord_cartesian(ylim = ylim)
  }

  if(first_model_ribbon){
    first_model_nm <- as.character(ts_quants$model[1])
    ts_quants_first <- ts_quants %>%
      filter(model == first_model_nm)
    g <- g +
      geom_ribbon(data = ts_quants_first, fill = model_colors[1], alpha = alpha) +
      geom_line(aes(color = model), size = line_width) +
      geom_point(aes(color = model), size = point_size) +
      geom_line(aes(y = !!sym(quants[1]), color = model), size = 0.5, lty = 2) +
      geom_line(aes(y = !!sym(quants[3]), color = model), size = 0.5, lty = 2)
  }else{
    g <- g +
      geom_line(aes(color = model), size = line_width) +
      geom_point(aes(color = model), size = point_size) +
      geom_line(aes(y = !!sym(quants[1]), color = model), size = 0.5, lty = 2) +
      geom_line(aes(y = !!sym(quants[3]), color = model), size = 0.5, lty = 2)
  }

  if(!rel && show_bo){
    g <- g +
      geom_pointrange(data = tso_quants, aes(color = model))
  }

  if(!is.null(leg_loc)){
    g <- g +
      theme(legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "white"))
  }

  g <- g + labs(color = legend_title)

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
