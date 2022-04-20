#' Plot the MCMC time series trajectories for iscam models, including spawning biomass
#' and recruitment for both absolute and relative cases.
#'
#' @rdname plot_biomass_mcmc
#'
#' @family Biomass plotting functions
#' @return A [ggplot2::ggplot()] object
#' @export
plot_biomass_mpd <- function(models,
                             model_names = NULL,
                             type = c("sbt", "rt"),
                             rel = FALSE,
                             show_initial = TRUE,
                             legend_title = "Models",
                             xlim = NULL,
                             ylim = NULL,
                             line_width = 1,
                             point_size = 2,
                             alpha = 0.2,
                             offset = 0.1,
                             palette = "Paired",
                             base_color = "#000000",
                             bo_dodge = 0.1,
                             x_space = 0.5,
                             append_base_txt = NULL,
                             show_bo_lines = FALSE,
                             show_bmsy_lines = FALSE,
                             bo_refpt_colors = c("red", "green"),
                             bmsy_refpt_colors = c("salmon", "darkgreen"),
                             ind_letter = NULL,
                             leg_loc = NULL,
                             probs = c(0.025, 0.5, 0.975),
                             ...){

  type <- match.arg(type)

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

  mpd <- map(models, ~{.x$mpd})
  nms <- names(mpd)
  if(is.null(nms)){
    if(is.null(model_names)){
      nms <- paste0("Temporary model ", seq_len(length(ts_quants)), append_base_txt)
    }else{
      if(length(model_names) != length(models)){
        stop("`model_names` is not the same length as the number of models supplied in `models`")
      }else{
        nms <- model_names
        nms[1] <- paste0(nms[1], append_base_txt)
      }
    }
    names(mpd) <- nms
  }else{
    names(mpd)[1] <- paste0(names(mpd)[1], append_base_txt)
  }

  sbt <- map(mpd, ~{.x[[type]]})
  start_yr <- map_dbl(models, ~{.x$dat$start.yr}) %>% min
  end_yr <- map_dbl(models, ~{.x$dat$end.yr}) %>% max
  if(is.null(xlim)){
    if(type == "rt"){
      start_yr <- start_yr + 1
      end_yr <- end_yr + 1
    }
    xlim <- c(start_yr, end_yr)
  }
  if(type == "sbt"){
    bind_yrs <- start_yr:end_yr
  }else if(type == "rt"){
    bind_yrs <- (start_yr + 1):(end_yr + 1)
  }

  sbt <- map(sbt, ~{
    length(.x) = end_yr - start_yr + 1
    .x
  }) %>%
    do.call(rbind, .)

  # Get initial estimate, ro, sbo
  init_type <- switch(type,
                      "sbt" = "sbo",
                      "rt" = "ro")
  init <- map(mpd, ~{.x[[init_type]]})
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
    if(type == "sbt"){
      # Recruitment not relative even if `rel == TRUE`
      init_vals_tmp <- init_vals %>%
        select(-year)
      vals <- vals %>% left_join(init_vals_tmp, by = "model") %>%
        mutate(sbt = sbt.x / sbt.y) %>%
        select(-sbt.x, -sbt.y)
    }
    y_label <- switch(type,
                      "sbt" = "Relative Spawning biomass",
                      "rt" = "Recruitment (millions)")
  }else{
    y_label <- switch(type,
                      "sbt" = "Spawning biomass ('000 tonnes)",
                      "rt" = "Recruitment (millions)")
  }

  if(!is.null(xlim)){
    vals <- vals %>%
    filter(year %in% xlim[1]:xlim[2])
    init_vals <- init_vals %>%
      mutate(year = xlim[1] - 1)
  }

  # 'Dodge' B0 points manually
  if((nrow(init_vals) - 1) * bo_dodge >= 1){
    warning("`bo_dodge` value of ", bo_dodge, " makes B0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }
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
    #guides(color = guide_legend(title = legend_title)) +
    scale_color_manual(values = model_colors) #+
    #scale_x_continuous(breaks = seq(min(bind_yrs), max(bind_yrs), 5))

  if(show_bo_lines && type == "sbt"){
    # Show the B0 lines for the first model, behind model lines
    tso_base <- init_vals %>%
      slice(1)
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    tso_multiples <- imap(c(0.2, 0.4), ~{
      tso_base %>%
        mutate(val = sbt * .x) %>%
        mutate(multiplier = .x)
    }) %>%
      bind_rows

    g <- g +
      geom_hline(data = tso_multiples,
                 aes(yintercept = val),
                 color = bo_refpt_colors,
                 lty = 1)
  }

  if(show_bmsy_lines && type == "sbt"){
    # Show the BMSY lines for the first model, behind model lines
    bmsy_base <- bmsy_vals %>%
      slice(1)
    # Only two lines allowed, Limit Reference Point (LRP) and Upper Stock
    # Reference (USR)
    bmsy_multiples <- imap(c(0.4, 0.8), ~{
      bmsy_base %>%
        mutate(val = sbt * .x) %>%
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
    if(show_initial){
      g <- g + scale_x_continuous(limits = c(xlim[1] - 1, xlim[2]),
                                  breaks = (min(xlim) - 1):max(xlim),
                                  labels = c(ifelse(type == "sbt", expression(B[0]), expression(R[0])), xlim[1]:xlim[2]),
                                  expand = expansion(add = x_space))
    }else{
      g <- g + scale_x_continuous(limits = c(xlim[1], xlim[2]),
                                  breaks = min(xlim):max(xlim),
                                  labels = xlim[1]:xlim[2],
                                  expand = expansion(add = x_space))
    }
  }

  if(is.null(ylim) && type == "sbt"){
    # Set up the y-axis limits to fit the data
    ymax <- max(select(vals, -c(model, year)),
                select(init_vals, -c(model, year)))
    brk <- seq(0, ymax, 50)
    lbl <- seq(0, ymax, 50)
    cols <- rep("black", length(brk))
    if(show_bo_lines){
      # Add the text labels to the y-axis ticks for the reference point levels
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

    g <- g +
      scale_y_continuous(limits = c(0, NA),
                         breaks = brk,
                         labels = lbl) +
      theme(axis.text.y = element_text(color = cols),
            axis.ticks.y = element_line(color = cols))
  }else{
    g <- g + scale_y_continuous(limits = ylim, expand = c(0, 0))
  }

  # Add biomass or recruitment trajectory lines and points
  g <- g +
    geom_line(aes(color = model), size = line_width) +
    geom_point(data = vals,
               aes(color = model),
               size = point_size)

  if(!rel){
    # Add initial biomass points (B0)
    g <- g +
      geom_point(data = init_vals, size = point_size)
  }

  if(!is.null(leg_loc)){
    # Add a legend
    g <- g +
      theme(legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "black"))
  }

  # Change the legend title
  g <- g + labs(color = legend_title)

  g

}
