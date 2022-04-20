#' Plot MCMC index fits for a single or group of models
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param model_names Names to use for the models in the plots. The names of
#' the list items in `models` will be used if they are present and this will
#' be ignored. If the list item names are not defined, temporary names will be used
#' @param surv_index The `survey_index` data frame which is the `dat` object in the output
#' from the [read_data_file()] function
#' @param start_year Year to start plot
#' @param end_year Year to end plot
#' @param legend_title Title text for the legend
#' @param palette A palette value that is in [RColorBrewer::brewer.pal.info]
#' @param base_color A color to prepend to the brewer colors which are set by `palette`.
#' This is called `base_color` because it is likely to be a base model
#' @param dodge A small value added to each year for each model. This is added cumulatively,
#' so each model fit appears more to the right than the previous one
#' @param index_line_width The index data error bar and connecting line width
#' @param index_point_size The index data point size
#' @param fit_line_width The model fit error bar and connecting line width
#' @param fit_point_size The model fit point size
#' @param errbar_width The width of the top and bottom crossbar of the errorbars
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @family Time series plotting functions
#' @importFrom RColorBrewer brewer.pal.info
#' @importFrom tibble enframe
#' @importFrom purrr flatten map_chr map_df map2
#' @importFrom dplyr mutate_at
#' @importFrom ggplot2 geom_ribbon facet_wrap scale_color_brewer
#' @export
plot_index_mcmc <- function(models,
                            model_names = NULL,
                            type = c("fits", "resids"),
                            surv_index,
                            start_year = 1995,
                            end_year = 2021,
                            legend_title = "Models",
                            palette = "Paired",
                            base_color = "#000000",
                            dodge = 0.3,
                            index_line_width = 0.5,
                            index_point_size = 2,
                            fit_line_width = 0.5,
                            fit_point_size = 2,
                            errbar_width = 0.5,
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
      select(year, biomass, lowerci, upperci, survey_name, survey_abbrev)
  })

  vals <- imap(models, ~{
    ind_vals <- if(type == "fits") .x$mcmccalcs$it_quants else .x$mcmccalcs$epsilon_quants
    if(is.null(ind_vals)){
      return(NULL)
    }
    ind_vals %>%
      mutate(model = .y) %>%
      left_join(surv_df, by = "survey_abbrev")
  })

  if(all(map_lgl(vals, is.null))){
    stop("None of the models supplied have MCMC index ", if(type == "fits") "fits" else "residuals")
  }

  # Remove any NULL list items (no index fits found in model)
  vals <- vals[!sapply(vals, is.null)] %>%
    bind_rows() %>%
    select(model, survey_name, survey_abbrev, year, biomass, lowerci, upperci)

  # Remove any missing indices from the `surv_abbrevs` vector and
  # the `surv_indices` data frame
  surv_abbrevs <- surv_abbrevs[surv_abbrevs %in% unique(vals$survey_abbrev)]
  surv_names <- surv_names[surv_names %in% unique(vals$survey_name)]
  surv_indices <- surv_indices %>%
    filter(survey_name %in% unique(vals$survey_name)) %>%
    mutate(survey_name = fct_relevel(survey_name, !!surv_names))

  vals <- vals %>%
    mutate(model = factor(model, names(models[names(models) %in% model]))) %>%
    mutate(survey_name = factor(survey_name, !!surv_names))

  # Filter out for the years provided
  vals <- vals %>%
    filter(year %in% start_year:end_year)

  if(type == "fits"){
    vals <- vals %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_abbrev == "DCPUE", x, x / 1e6)
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
                  ifelse(.$survey_abbrev == "DCPUE", x / 1e6,  x)
                })
    surv_indices <- surv_indices %>%
      mutate_at(vars(biomass, lowerci, upperci),
                function(x){
                  ifelse(.$survey_abbrev == "DCPUE", x,  x / 1e6)
                })
  }

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  if(type == "fits"){
    g <- ggplot(surv_indices,
                aes(x = year, y = biomass)) +
      geom_line(size = index_line_width) +
      geom_point(size = index_point_size) +
      geom_errorbar(aes(ymin = lowerci, ymax = upperci),
                    width = errbar_width,
                    size = index_line_width) +
      facet_wrap(~survey_name, scales = "free_y") +
      geom_line(data = vals,
                aes(color = model),
                size = fit_line_width) +
      geom_point(data = vals,
                 aes(color = model),
                 size = fit_point_size) +
      geom_errorbar(data = vals,
                    aes(color = model, ymin = lowerci, ymax = upperci),
                    width = errbar_width,
                    size = fit_line_width) +
      xlab("Year") +
      ylab("Index (thousand tonnes, DCPUE ~ kg/hr)") +
      scale_color_manual(values = model_colors) +
      guides(color = guide_legend(title = legend_title)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_x_continuous(breaks = ~{pretty(.x, n = 5)})
  }else if(type == "resids"){
    g <- vals %>%
      ggplot(aes(x = year, y = biomass, color = model)) +
      stat_identity(yintercept = 0,
                    geom = "hline",
                    inherit.aes = FALSE,
                    linetype = "longdash") +
      geom_point(size = fit_point_size) +
      geom_errorbar(aes(ymin = lowerci, ymax = upperci),
                    width = errbar_width,
                    size = fit_line_width) +
      facet_wrap(~survey_name, scales = "free_y") +
      xlab("Year") +
      ylab("Log standardized residual") +
      scale_color_manual(values = model_colors) +
      guides(color = guide_legend(title = legend_title)) +
      scale_x_continuous(breaks = ~{pretty(.x, n = 5)})
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
