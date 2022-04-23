#' Plot the MCMC time series trajectories for iscam models, including spawning biomass
#' and recruitment for both absolute and relative cases.
#'
#' @rdname plot_biomass_mcmc
#'
#' @family Biomass plotting functions
#' @return A [ggplot2::ggplot()] object
#' @export
plot_biomass_mpd <- function(models,
                             rel = FALSE,
                             show_bo = TRUE,
                             legend_title = "Models",
                             xlim = NULL,
                             ylim = NULL,
                             line_width = 1,
                             point_size = 2,
                             alpha = 0.2,
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
                             text_title_size = 12,
                             angle_x_labels = FALSE,
                             ...){

  single_model <- FALSE
  if(class(models) == mdl_cls){
    single_model <- TRUE
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
  if(!is.null(append_base_txt)){
    length(append_base_txt) <- length(models)
    # If `append_base_txt` is shorter than the number of models, append empty strings
    # for remainder of items
    append_base_txt[which(is.na(append_base_txt))] <- ""
  }
  # Set up model names for the legend/title
  names(models) <- map_chr(models, ~{
    as.character(attributes(.x)$model_desc)
  })
  names(models) <- paste0(names(models), append_base_txt)

  mpd <- map(models, ~{.x$mpd})
  nms <- names(mpd)

  start_yr <- min(map_dbl(models, ~{.x$dat$start.yr})) + 1
  end_yr <- max(map_dbl(models, ~{.x$dat$end.yr}))
  if(is.null(xlim)){
    xlim <- c(start_yr, end_yr)
  }else{
    if(start_yr > xlim[1]){
      stop("Start year in xlim comes before the data start year")
    }
    if(end_yr < xlim[2]){
      stop("End year in xlim comes after the data end year")
    }
    start_yr <- xlim[1]
    end_yr <- xlim[2]
  }
  bind_yrs <- start_yr:end_yr

  sbt <- map(mpd, ~{.x$sbt}) %>%
    map(~{
      length(.x) = end_yr - start_yr + 1
      .x
    }) %>%
    do.call(rbind, .)

  # Get initial estimate, sbo
  init <- map(mpd, ~{.x$sbo})
  init_vals <- tibble(year = start_yr - 1,
                      model = names(init),
                      sbt = map_dbl(init, ~{.x})) %>%
    mutate(year = start_yr - 1)

  # Get Bmsy for lines, no years
  bmsy <- imap(models, ~{.x$mpd$bmsy})
  bmsy_vals <- tibble(model = names(bmsy),
                      sbt = map_dbl(bmsy, ~{.x}))

  vals <- bind_yrs %>%
    as_tibble() %>%
    `names<-`("year") %>%
    bind_cols(t(sbt)) %>%
    pivot_longer(cols = -year, names_to = "model", values_to = "sbt")

  if(rel){
    init_vals_tmp <- init_vals %>%
      select(-year)
    vals <- vals %>%
      left_join(init_vals_tmp, by = "model") %>%
      mutate(sbt = sbt.x / sbt.y) %>%
      select(-sbt.x, -sbt.y)
    y_label <- "Relative Spawning biomass"
  }else{
    y_label <- "Spawning biomass ('000 tonnes)"
  }

  vals <- vals %>%
    filter(year %in% xlim[1]:xlim[2])
  init_vals <- init_vals %>%
    mutate(year = xlim[1] - 1)

  init_vals <- init_vals %>%
    mutate(year = seq(from = first(year), by = bo_dodge, length.out = nrow(.)))

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  g <- vals %>%
    ggplot(aes(x = year,
               y = sbt,
               color = model)) +
    xlab("Year") +
    ylab(y_label) +
    scale_color_manual(values = model_colors)

  if(show_bo_lines){
    # Show the B0 lines for the first model, behind model lines
    tso_base <- init_vals %>%
      slice(1)
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    tso_multiples <- imap(bo_refpts, ~{
      tso_base %>%
        mutate(val = sbt * .x) %>%
        mutate(multiplier = .x)
    }) %>%
      bind_rows

    if(rel){
      tso_multiples[1, ]$sbt <- 1
      tso_multiples[2, ]$val <- 1
      tso_multiples[1, ]$val <- bo_refpts[1]
      tso_multiples[2, ]$val <- bo_refpts[2]
    }
    g <- g +
      geom_hline(data = tso_multiples,
                 aes(yintercept = val),
                 color = bo_refpt_colors,
                 lty = 1)
  }

  if(show_bmsy_lines){
    # Show the BMSY lines for the first model, behind model lines
    bmsy_base <- bmsy_vals %>%
      slice(1)
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    bmsy_multiples <- imap(bmsy_refpts, ~{
      bmsy_base %>%
        mutate(val = ifelse(rel, sbt / tso_base[1, ]$sbt * .x, sbt * .x)) %>%
        mutate(multiplier = .x)
    }) %>%
      bind_rows

    g <- g +
      geom_hline(data = bmsy_multiples,
                 aes(yintercept = val),
                 color = bmsy_refpt_colors,
                 lty = 1)
  }

  # Reset the x-axis limits based on type of plot
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

  # Create tags for B0 and BMSY lines, and breaks and labels for y-axis
  if(rel){
    ymax <- max(select(vals, -c(model, year)))
  }else{
    ymax <- max(select(vals, -c(model, year)),
                select(init_vals, -c(model, year)))
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
    brk <- sort(c(brk, tso_multiples$val))
    lbl <- brk
    wch <- which(brk %in% tso_multiples$val)
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
      brk <- sort(c(brk, bmsy_multiples$val))
      wch <- which(brk %in% bmsy_multiples$val)
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
      brk <- sort(c(brk, bmsy_multiples$val))
      lbl <- brk
      wch <- which(brk %in% bmsy_multiples$val)
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
                         labels = lbl) +
      theme(axis.text.y = element_text(color = cols),
            axis.ticks.y = element_line(color = cols)) +
      coord_cartesian(ylim = ylim)
  }

  # 'Dodge' B0 points manually
  if((nrow(init_vals) - 1) * bo_dodge >= 1){
    warning("`bo_dodge` value of ", bo_dodge, " makes B0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }

  # Add biomass or recruitment trajectory lines and points
  g <- g +
    geom_point(data = vals,
               aes(color = model),
               size = point_size) +
    geom_line(data = vals,
              size = line_width)

  if(!rel){
    # Add initial biomass points (B0)
    g <- g +
      geom_point(data = init_vals, size = point_size)
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
    if(single_model){
      g <- g + ggtitle(names(models)) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
    }
  }else{
    g <- g +
      theme(legend.justification = leg_loc,
            legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "white")) +
      labs(color = legend_title)
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
