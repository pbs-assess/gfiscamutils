#' Plot MPD index fits or residuals for iSCAM models
#'
#' @description
#' Plot MPD index fits or residuals for a single or group of models by gear
#'
#' @inheritParams plot_index_mcmc
#' @family Time series plotting functions
#'
#' @export
plot_index_mpd <- function(models,
                           type = c("fits", "resids"),
                           surv_index,
                           gear = NULL,
                           start_year = 1995,
                           end_year = 2021,
                           append_base_txt = NULL,
                           legend_title = "Models",
                           palette = "Paired",
                           base_color = "black",
                           dodge = 0.3,
                           index_line_width = 0.5,
                           index_point_size = 2,
                           index_color = "chocolate3",
                           fit_line_width = 0.5,
                           fit_point_size = 2,
                           leg_loc = c(1, 1),
                           angle_x_labels = FALSE){

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

  # surv_abbrev will be in order of the gears in the models
  surv_abbrev_lst <- map(models, ~{
    .x$dat$index_abbrevs
  })
  surv_name_lst <- map(models, ~{
    .x$dat$index_gear_names
  })

  surv_abbrevs <- surv_abbrev_lst %>%
    flatten() %>%
    map_chr(~{.x}) %>%
    unique()
  surv_names <- surv_name_lst %>%
    flatten() %>%
    map_chr(~{.x}) %>%
    unique()

  if(length(surv_names) != length(surv_abbrevs)){
    stop("The total number of unique 'IndexGears' and 'IndexAbbrevs' defined in the data files ",
         "for the `models` do not match. It is likely you defined some of these slightly ",
         "differently in different data files. They must match exactly.")
  }

  if(!is.null(gear)){
    valid_gear_nums <- seq_along(surv_names)
    if(!all(gear %in% valid_gear_nums)){
      gear_tab_str <- surv_names %>%
        enframe(name = "gear") %>%
        rename(gear_name = value) %>%
        capture.output() %>%
        paste(collapse = "\n")

      stop("One or more of the gear numbers you requested is outside the ",
           "range of possible gears.\n",
           "Gears passed in were: ", paste(gear, collapse = ", "), "\n",
           "Gear numbers for this (combination of) model(s) are:\n\n",
           gear_tab_str,
           call. = FALSE)
    }
    surv_abbrevs <- surv_abbrevs[gear]
    surv_names <- surv_names[gear]
  }

  # Add survey names to the table with a left join by survey_abbrev
  surv_abbrevs_df <- surv_abbrevs %>%
    enframe(name = NULL)
  surv_names_df <- surv_names %>%
    enframe(name = NULL)
  surv_df <- surv_abbrevs_df %>%
    cbind(surv_names_df) %>%
    `names<-`(c("survey_abbrev", "survey_name"))
  surv_index_df <- surv_index %>%
    filter(year %in% start_year:end_year) %>%
    filter(survey_abbrev %in% !!surv_abbrevs) %>%
    left_join(surv_df, by = "survey_abbrev")

  surv_indices <- map_df(surv_abbrevs, ~{
    surv_index_df %>%
      filter(survey_abbrev == .x) %>%
      select(year, biomass, lowerci, upperci, survey_name)
  })

  vals <- imap(models, ~{
    ind_vals <- if(type == "fits")
      .x$mpd$it_hat else
        .x$mpd$epsilon
    if(is.null(ind_vals)){
      return(NULL)
    }
    out <- list()
    for(i in seq_len(nrow(ind_vals))){
      yrs <- .x$dat$indices[[i]][, "iyr"]
      tmp <- ind_vals[i, ]
      out[[i]] <- tmp[!is.na(tmp)]
      names(out[[i]]) <- yrs
      out[[i]] <- out[[i]] %>%
        enframe(name = "year", value = "biomass") %>%
        mutate(model = .y) %>%
        mutate(survey_name = .x$dat$index_gear_names[i]) %>%
        mutate(year = as.numeric(year))
    }
    out %>%
      bind_rows()
  }) %>%
    bind_rows()

  # Remove any missing indices from the `surv_names` vector and
  # the `surv_indices` data frame
  surv_names <- surv_names[surv_names %in% unique(vals$survey_name)]
  surv_indices <- surv_indices %>%
    filter(survey_name %in% unique(vals$survey_name)) %>%
    mutate(survey_name = fct_relevel(survey_name, !!surv_names))

  vals <- vals %>%
    filter(survey_name %in% surv_names) %>%
    mutate(model = factor(model, names(models[names(models) %in% model]))) %>%
    mutate(survey_name = factor(survey_name, !!surv_names))

  # Filter out for the years provided
  vals <- vals %>%
    filter(year %in% start_year:end_year)

  if(type == "fits"){
    # Rescale values
    surv_indices <- surv_indices %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_name == "Discard CPUE", x,  x / 1e6)
                })
  }

  # Dodge year points a little, cumulative for each model
  dodge_amt <- if(type == "fits") dodge else 0
  vals <- vals %>%
    split(~model) %>%
    imap(~{
      tmp <- .x %>% mutate(year = year + dodge_amt)
      dodge_amt <<- dodge_amt + dodge
      tmp
    }) %>%
    bind_rows()

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  has_dcpue <- "Discard CPUE" %in% unique(surv_indices$survey_name)
  only_dcpue <- has_dcpue && length(unique(surv_indices$survey_name)) == 1

  if(type == "fits"){
    g <- ggplot(surv_indices,
                aes(x = year, y = biomass)) +
      geom_line(size = index_line_width,
                color = index_color,
                linetype = "dotted") +
      geom_point(size = index_point_size,
                 color = index_color) +
      geom_errorbar(aes(ymin = lowerci, ymax = upperci),
                    width = errbar_width,
                    size = index_line_width,
                    color = index_color) +
      geom_line(data = vals,
                aes(color = model),
                size = fit_line_width) +
      geom_point(data = vals,
                 aes(color = model),
                 size = fit_point_size)+
      facet_wrap(~survey_name,
                 scales = "free_y") +
      xlab("Year") +
      ylab(ifelse(has_dcpue,
                  ifelse(only_dcpue,
                         "Index (kg/hr)",
                         "Index ('000 t, kg/hr for Discard CPUE)"),
                  "Index ('000 t)")) +
      scale_color_manual(values = model_colors) +
      guides(color = guide_legend(title = legend_title)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else if(type == "resids"){
    g <- vals %>%
      ggplot(aes(x = year,
                 y = biomass, color = model)) +
      stat_identity(yintercept = 0,
                    geom = "hline",
                    inherit.aes = FALSE,
                    linetype = "longdash") +
      geom_point(size = fit_point_size) +
      geom_segment(aes(x = year,
                       xend = year,
                       y = 0,
                       yend = biomass)) +
      facet_wrap(~survey_name,
                 scales = "free_y") +
      xlab("Year") +
      ylab("Log standardized residual") +
      scale_color_manual(values = model_colors) +
      guides(color = guide_legend(title = legend_title)) +
      scale_x_continuous(breaks = ~{pretty(.x, n = 5)})
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
  }else if(leg_loc[1] == "facet"){
    g <- g %>% move_legend_to_empty_facet()
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
