#' Plot MCMC recruitments for iSCAM models
#'
#' @description
#' Plot the MCMC recruitment time series trajectories with credible intervals
#' for iscam models.
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param show_ro Show the initial recruitment, R0 median line and credible interval
#' @param ro_ribbon See `refpts_ribbon` in [plot_biomass_mcmc()]
#' @param ro_alpha See `refpts_alpha` in [plot_biomass_mcmc()]
#' @param r_dodge See `bo_dodge` in [plot_biomass_mcmc()]
#' @export
plot_recr_mcmc <- function(models,
                           show_ro = TRUE,
                           ro_color = base_color,
                           legend_title = "Models",
                           xlim = NULL,
                           ylim = NULL,
                           line_width = 1,
                           point_size = 2,
                           ro_ribbon = TRUE,
                           ro_alpha = 0.3,
                           palette = "Paired",
                           base_color = "black",
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

  start_yr <- map_dbl(models, ~{.x$dat$start.yr + 1}) %>% min
  end_yr <- map_dbl(models, ~{.x$dat$end.yr + 1}) %>% max
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

  # Main time series values
  ts_quants <- map(models,
                   ~{.x$mcmccalcs$rt_quants})
  # Initial values (R0)
  tso_quants <- map(models,
                    ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "ro"]})

  tso_quants <- tso_quants %>%
    bind_rows() %>%
    mutate(model = names(models)) %>%
    mutate(year = start_yr - 1) %>%
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
    select(-MPD) %>%
    as_tibble()

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(ts_quants), value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data: ", .x)
    }
    mtch
  })

  # Set up x and y axes
  if(!is.null(xlim)){
    ts_quants <- ts_quants %>%
      filter(year %in% xlim[1]:xlim[2])
  }
  ts_quants <- ts_quants %>%
    mutate(model = fct_relevel(model, names(models)))

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  # Dodge years for recruitment values. Need to do this before setting xlim values
  # because only the first model will be plotted in the last year if dodge is not
  # accounted for
  dodge_val <- 0
  ts_dodge <- ts_quants %>%
    split(~model) %>%
    map(~{
      x <- .x %>% mutate(year = year + dodge_val)
      dodge_val <<- dodge_val + 0.1
      x
    }) %>%
    bind_rows

  g <- ts_quants %>%
    ggplot(aes(x = year,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab(en2fr("Year")) +
    ylab(ifelse(fr(), "Recrutement (millions)", "Recruitment (millions)")) +
    scale_color_manual(values = model_colors)

  if(show_ro){
    # Show the R0 line for the first model with CI, behind model lines
    tso_base <- tso_quants %>%
      slice(1) %>%
      mutate(model = as.factor(model))

    if(ro_ribbon){
      g <- g +
        geom_rect(data = tso_base,
                  aes(xmin = ifelse(show_ro, start_yr - 1, start_yr),
                      xmax = max(ts_dodge$year)),
                  alpha = ro_alpha,
                  fill = ro_color) +
        geom_hline(data = tso_base,
                   aes(yintercept = !!sym(quants[2])),
                   color = ro_color, lty = 1, lwd = 1)
    }else{
      g <- g +
        geom_hline(data = tso_base,
                   aes(yintercept = !!sym(quants[1])),
                   color = ro_color,
                   lty = 3) +
        geom_hline(data = tso_base,
                   aes(yintercept = !!sym(quants[2])),
                   color = ro_color,
                   lty = 1) +
        geom_hline(data = tso_base,
                   aes(yintercept = !!sym(quants[3])),
                   color = ro_color,
                   lty = 3)
    }
    g <- g + scale_x_continuous(limits = c(xlim[1] - 1, max(ts_dodge$year)),
                                breaks = (min(xlim) - 1):max(xlim),
                                labels = c(expression(R[0]), xlim[1]:xlim[2]),
                                expand = expansion(add = x_space))
  }else{
    g <- g + scale_x_continuous(limits = c(xlim[1], max(ts_dodge$year)),
                                breaks = min(xlim):max(xlim),
                                labels = xlim[1]:xlim[2],
                                expand = expansion(add = x_space))
  }

  # Create tags for R0 line, and breaks and labels for y-axis
  ymax <- max(select(ts_quants, -c(model, year)),
              select(tso_quants, -c(model, year)))
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
    # Add the text labels to the y-axis ticks for R0
    brk <- sort(c(brk, tso_base[[quants[2]]]))
    lbl <- brk
    wch <- which(brk %in% tso_base[[quants[2]]])
    if(length(wch) != 1){
      stop("Could not find R0 in the tso_base data frame. See function code")
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
  if((nrow(tso_quants) - 1) * r_dodge >= 1){
    warning("`r_dodge` value of ", r_dodge, " makes R0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }

  g <- g +
    geom_point(data = ts_dodge,
               aes(color = model),
               size = point_size) +
    geom_segment(data = ts_dodge,
                 aes(xend = year,
                     y = !!sym(quants[1]),
                     yend = !!sym(quants[3]),
                     color = model),
                 size = line_width)

  if(show_ro){
    # Dodge R0 values
    tso_quants <- tso_quants %>%
      mutate(year = year + (row_number() - 1) * r_dodge)
    g <- g +
      geom_pointrange(data = tso_quants, aes(color = model))
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
    if(single_model){
      if(!is.null(text_title_size)){
        g <- g + ggtitle(tex(names(models))) +
          theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
      }
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
