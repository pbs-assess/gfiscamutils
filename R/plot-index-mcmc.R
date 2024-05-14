#' Plot MCMC index fits or residuals for iSCAM models
#'
#' @description
#' Plot MCMC index fits or residuals for a single or group of models by gear
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param surv_index The `survey_index` data frame which is `dat$survey_index`
#' if `dat` is the output from the [gfdata::get_survey_index()] function
#' @param type Either 'fits' or 'resids' for model fits or residuals respectively
#' @param gear A vector of gear numbers to show panels for. If `NULL`, all will be shown.
#' If a gear number out of range is included, a lookup table with gear numbers and names
#' will be shown
#' @param index_line_width The index data error bar and connecting line width
#' @param index_point_size The index data point size
#' @param index_color The color used for the observed index lines and points
#' @param fit_line_width The model fit error bar and connecting line width
#' @param fit_point_size The model fit point size
#' @param errbar_width The width of the top and bottom crossbar of the errorbars
#'
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom tibble enframe
#' @importFrom purrr flatten map_chr map_df map2
#' @importFrom dplyr mutate_at
#' @importFrom ggplot2 geom_ribbon facet_wrap scale_color_brewer
#' @importFrom utils capture.output
#' @export
plot_index_mcmc <- function(models,
                            type = c("fits", "resids"),
                            surv_index,
                            gear = NULL,
                            start_year = 1996,
                            end_year = 2021,
                            append_base_txt = NULL,
                            legend_title = "Models",
                            palette = iscam_palette,
                            base_color = "black",
                            dodge = 0.3,
                            index_line_width = 0.5,
                            index_point_size = 2,
                            #index_color = "chocolate3",
                            index_color = "darkgrey",
                            fit_line_width = 0.5,
                            fit_point_size = 2,
                            errbar_width = 0.5,
                            leg_loc = c(1, 1),
                            text_title_size = 12,
                            angle_x_labels = FALSE){

  type <- match.arg(type)

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
      gear_table_str <- surv_names %>%
        enframe(name = "gear") %>%
        rename(gear_name = value) %>%
        capture.output() %>%
        paste(collapse = "\n")

      stop("One or more of the gear numbers you requested is outside the ",
           "range of possible gears.\n",
           "Gears passed in were: ", paste(gear, collapse = ", "), "\n",
           "Gear numbers for this combination of models are:\n\n",
           gear_table_str,
           call. = FALSE)
    }
    surv_abbrevs <- surv_abbrevs[gear]
    surv_names <- surv_names[gear]
  }

  surv_abbrevs[surv_abbrevs == "HS MSA"] <- "OTHER HS MSA"

  # Add survey names to the table with a left join by survey_abbrev
  surv_abbrevs_df <- surv_abbrevs %>%
    enframe(name = NULL)
  surv_names_df <- surv_names %>%
    enframe(name = NULL)
  surv_df <- surv_abbrevs_df %>%
    cbind(surv_names_df) %>%
    `names<-`(c("survey_abbrev", "survey_name"))

  surv_index_df <- surv_index |>
    filter(year %in% start_year:end_year) |>
    filter(survey_abbrev %in% !!surv_abbrevs) |>
    #filter(grepl(paste(surv_abbrevs, collapse = "|"), survey_abbrev)) %>%
    left_join(surv_df, by = "survey_abbrev")

  surv_indices <- map_df(surv_abbrevs, ~{
    surv_index_df %>%
      filter(survey_abbrev == .x) %>%
      select(year, biomass, lowerci, upperci, survey_name)
  })

  vals <- imap(models, ~{
    ind_vals <- if(type == "fits")
      .x$mcmccalcs$it_quants else
        .x$mcmccalcs$std_epsilon_quants
    if(is.null(ind_vals)){
      return(NULL)
    }
    ind_vals %>%
      mutate(model = .y)
  })

  if(all(map_lgl(vals, is.null))){
    stop("None of the models supplied have MCMC index ",
         if(type == "fits") "fits" else "residuals",
         call. = FALSE)
  }

  # Remove any NULL list items (no index fits found in model)
  vals <- vals[!sapply(vals, is.null)] %>%
    bind_rows() %>%
    select(model, survey_name, year, biomass, lowerci, upperci)

  # Remove all rows where `survey_name` is `NULL` which is caused by it not being in the gear list
  vals <- vals %>%
    filter(!is.na(survey_name))

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
    vals <- vals %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_name == "Discard CPUE", x / 1e6, x)
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

  if(type == "fits"){
    # Rescale values
    vals <- vals %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_name == "Discard CPUE", x, x / 1e6)
                })
    surv_indices <- surv_indices %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_name == "Discard CPUE", x, x / 1e6)
                })
  }

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  has_dcpue <- "Discard CPUE" %in% unique(surv_indices$survey_name)
  only_dcpue <- has_dcpue && length(unique(surv_indices$survey_name)) == 1

  # Remove zeroes from the fit data frame
  vals <- vals |> filter(biomass != 0 & lowerci != 0 & upperci !=0)

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
      facet_wrap(~survey_name, scales = "free_y") +
      geom_line(data = vals,
                aes(color = model),
                size = fit_line_width) +
      geom_point(data = vals,
                 aes(color = model),
                 size = fit_point_size) +
      geom_errorbar(data = vals,
                    aes(color = model,
                        ymin = lowerci,
                        ymax = upperci),
                    width = errbar_width,
                    size = fit_line_width) +
      xlab(tr("Year")) +
      ylab(ifelse(has_dcpue,
                  ifelse(only_dcpue,
                         ifelse(fr(), "Indice (kg/heure)", "Index (kg/hr)"),
                         ifelse(fr(),
                                "Indice ('000 t, kg/heure pour la CPUE des rejets)",
                                "Index ('000 t, kg/hr for Discard CPUE)")),
                  ifelse(fr(), "Indice ('000 t)", "Index ('000 t)"))) +
      scale_color_manual(values = model_colors,
                         labels = map(models, ~{tex(as.character(attributes(.x)$model_desc))})) +
      guides(color = guide_legend(title = legend_title,
                                  byrow = TRUE)) +
      theme(legend.title=element_blank(),
            legend.spacing.y = unit(0.0001, "npc"),
            legend.box.background = element_rect(color = NA)) +
      scale_x_continuous(breaks = ~{pretty(.x, n = 5)})
  }else if(type == "resids"){

    g <- vals %>%
      ggplot(aes(x = year,
                 y = biomass)) +
      stat_identity(yintercept = 0,
                    geom = "hline",
                    inherit.aes = FALSE,
                    linetype = "longdash") +
      geom_point(aes(color = model),
                 size = fit_point_size) +
      geom_errorbar(aes(ymin = lowerci,
                        ymax = upperci,
                        color = model),
                    width = errbar_width,
                    size = fit_line_width) +
      facet_wrap(~survey_name,
                 scales = "free_y") +
      xlab(tr("Year")) +
      ylab(ifelse(fr(),
                  "Résidu normalisé logarithmique",
                  "Log standardized residual")) +
      scale_color_manual(values = model_colors,
                         labels = map(models, ~{tex(as.character(attributes(.x)$model_desc))})) +
      guides(color = guide_legend(title = legend_title,
                                  byrow = TRUE)) +
      theme(legend.title=element_blank(),
            legend.spacing.y = unit(0.0001, "npc"),
            legend.box.background = element_rect(color = NA)) +
      scale_x_continuous(breaks = ~{pretty(.x, n = 5)})
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
    if(single_model){
      if(!is.null(text_title_size)){
        g <- g + ggtitle(tex(names(models))) +
          theme(plot.title = element_text(hjust = 0.5,
                                          size = text_title_size))
      }
    }
  }else if(leg_loc[1] == "facet"){
    g <- g %>% move_legend_to_empty_facet()
  }else if(leg_loc[1] == "outside"){
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
