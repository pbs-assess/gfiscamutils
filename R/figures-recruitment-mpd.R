#' Plot the MCMC time series trajectories for iscam models, including spawning biomass
#' and recruitment for both absolute and relative cases.
#'
#' @rdname plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param show_ro Show the initial recruitment, R0 median line and credible interval
#' @param ro_ribbon See `refpts_ribbon` in [plot_biomass_mcmc()]
#' @param ro_alpha See `refpts_alpha` in [plot_biomass_mcmc()]
#' @param r_dodge See `bo_dodge` in [plot_biomass_mcmc()]
#' @return A [ggplot2::ggplot()] object
#' @export
plot_recr_mpd <- function(models,
                          model_names = NULL,
                          show_ro = TRUE,
                          ro_color = base_color,
                          legend_title = "Models",
                          xlim = NULL,
                          ylim = NULL,
                          line_width = 1,
                          point_size = 2,
                          alpha = 0.2,
                          palette = "Paired",
                          base_color = "#000000",
                          r_dodge = 0.1,
                          x_space = 0.5,
                          append_base_txt = NULL,
                          ind_letter = NULL,
                          leg_loc = NULL,
                          probs = c(0.025, 0.5, 0.975),
                          text_title_size = 12,
                          angle_x_labels = FALSE,
                          ...){

  single_model <- FALSE
  if(is_iscam_model(models)){
    single_model <- TRUE
    models <- list(models)
    class(models) <- mdl_lst_cls
  }

  if(!is_iscam_model_list(models)){
    stop("The `models` list is not a gfiscamutils::mdl_lst_cls class.")
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
  if(is.null(nms)){
    if(is.null(model_names)){
      nms <- paste0("Temporary model ", seq_len(length(mpd)), append_base_txt)
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

  # Get recruitment time series MPD values
  recr <- map(mpd, ~{
    tmp <- .x$rt
    length(tmp) <- end_yr - start_yr + 1
    tmp
  }) %>%
    do.call(rbind, .)

  # Reformat for ggplot
  recr <- bind_yrs %>%
    as_tibble() %>%
    `names<-`("year") %>%
    bind_cols(t(recr)) %>%
    pivot_longer(cols = -year, names_to = "model", values_to = "recr")

  # Get initial estimate, ro
  init <- map(mpd, ~{.x$ro})
  init_recr <- tibble(year = start_yr - 1,
                      model = names(init),
                      recr = map_dbl(init, ~{.x})) %>%
    mutate(year = start_yr - 1)

  recr <- recr %>%
    filter(year %in% xlim[1]:xlim[2])
  recr <- recr %>%
    mutate(model = fct_relevel(model, nms))

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  g <- recr %>%
    ggplot(aes(x = year,
               y = recr,
               color = model)) +
    xlab("Year") +
    ylab("Recruitment (millions)") +
    scale_color_manual(values = model_colors)

  if(show_ro){
    # Show the R0 line for the first model, behind model lines
    recr_base <- init_recr %>%
      slice(1)
    g <- g +
      geom_hline(data = recr_base,
                 aes(yintercept = recr),
                 color = ro_color,
                 lty = 1) +
      scale_x_continuous(limits = c(xlim[1] - 1, xlim[2]),
                         breaks = (min(xlim) - 1):max(xlim),
                         labels = c(expression(R[0]), xlim[1]:xlim[2]),
                         expand = expansion(add = x_space))
  }else{
    g <- g + scale_x_continuous(limits = c(xlim[1], xlim[2]),
                                breaks = min(xlim):max(xlim),
                                labels = xlim[1]:xlim[2],
                                expand = expansion(add = x_space))
  }

  # Create tags for R0 line, and breaks and labels for y-axis
  ymax <- max(select(recr, -c(model, year)),
              select(init_recr, -c(model, year)))
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
  if(show_ro){
    # Add the text labels to the y-axis ticks for the reference point levels
    brk <- sort(c(brk, recr_base$recr))
    lbl <- brk
    wch <- which(brk %in% recr_base$recr)
    if(length(wch) != 1){
      stop("Could not find R0 in the `recr_base` data frame. See function code")
    }
    lbl[wch][1] <- as.expression(bquote(~ R[0]))
    cols <- rep("black", length(brk))
    cols[wch] <- ro_color
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

  # Must dodge values in the data frame, segments can't be dodged
  if((nrow(init_recr) - 1) * r_dodge >= 1){
    warning("`r_dodge` value of ", r_dodge, " makes R0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }

  g <- g +
      geom_point(data = recr,
                 aes(color = model),
                 size = point_size) +
      geom_line(data = recr,
                size = line_width)

  if(show_ro){
    # Dodge R0 values
    init_recr_dodge <- init_recr %>%
      mutate(year = year + (row_number() - 1) * r_dodge)
    g <- g +
      geom_point(data = init_recr_dodge, aes(color = model))
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
