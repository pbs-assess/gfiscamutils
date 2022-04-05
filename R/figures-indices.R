#' Plot the residuals for multiple models
#'
#' @param models A list of iSCAM models
#' @param gear One of "SYN QCS", "SYN HS", "SYN WCVI", "SYN WCHG", "DCPUE"
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 element_text geom_boxplot geom_errorbar geom_hline stat_boxplot
#' @importFrom ggplot2 ggtitle scale_color_manual scale_fill_manual aes_string scale_x_discrete
#' @importFrom dplyr group_by mutate_all slice ungroup
#' @importFrom purrr map_dfr
#' @export
 plot_index_resids_mpd <- function(models,
                                   gear = "SYN QCS",
                                   angle_x_labels = FALSE){

  surv_abbrevs <- map(models, ~{
    .x$dat$index_abbrevs
  })
  gear_vec <- unique(unlist(surv_abbrevs))
  if(!gear %in% gear_vec){
    stop("gear ", gear, " is not in the list of available gears: ",
         paste(gear_vec, collapse = ", "), call. = FALSE)
  }

  model_names <- names(models)
  model_names <- factor(model_names, levels = model_names)

  resids <- map2(seq_along(models), surv_abbrevs, ~{
    model_name <- model_names[.x]
    gr <- match(.y, gear)
    gear_ind <- which(!is.na(match(.y, gear)))
    if(!length(gear_ind)){
      return(NA)
    }
    resid <- models[[.x]]$mpd$epsilon[gear_ind, ]
    resid <- resid[!is.na(resid)] %>% as_tibble()
    names(resid) <- "value"

    yrs <- as_tibble(models[[.x]]$dat$indices[[gear_ind]]) %>% select(iyr)
    resid <- bind_cols(yrs, resid) %>%
      rename(year = iyr) %>%
      mutate(model = model_name)
    resid
  })
  names(resids) <- names(models)
  resids <- resids[!is.na(resids)]
  resids <- resids %>% bind_rows()
  resids <- resids %>%
    rename(Year = year, Model = model, `Log standardized residual` = value)

  g <- ggplot(resids, aes(Year, `Log standardized residual`, color = Model)) +
    geom_point(shape = 19, size = 3) +
    geom_line() +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 0.3) +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    scale_x_continuous(breaks = resids$Year, labels = as.character(resids$Year))

  if(angle_x_labels){
    g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}

#' Plot index fits for multiple models and indices, for iSCAM MPD models
#'
#' @param models A list of iSCAM models, as output by [arrowtooth::model_setup()]
#' @param model_names A vector of model names to show in plots of the same length as `model`
#' @param surv_index Output from [gfdata::get_survey_index()]
#' @param start_year First year in plot
#' @param end_year Last year in plot
#' @param legend_title Title to use for the legend
#' @param palette The [RColorBrewer::brewer.pal.info] palette to use for line and point color
#' @param line_width The width of the lines
#' @param point_size The size of the points
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom purrr flatten map_chr map_df map2
#' @importFrom dplyr mutate_at
#' @importFrom ggplot2 geom_ribbon facet_wrap scale_color_brewer
#' @export
plot_index_fit_mpd <- function(models,
                               model_names = factor(names(models), levels = names(models)),
                               surv_index,
                               start_year = 1996,
                               end_year = 2021,
                               legend_title = "Bridge model",
                               palette = "Paired",
                               line_width = 0.5,
                               point_size = 1){

  # surv_abbrev will be in order of the gears in the models
  surv_abbrevs <- map(models, ~{
    .x$dat$index_abbrevs
  })

  surv_abbrev <- surv_abbrevs %>%
    flatten() %>%
    map_chr(~{.x}) %>%
    unique()

  surv_index <- surv_index %>%
    filter(year %in% start_year:end_year) %>%
    filter(survey_abbrev %in% surv_abbrev)

  surv_indices <- map_df(surv_abbrev, ~{
    surv_index %>%
      filter(survey_abbrev == .x) %>%
      select(year, biomass, lowerci, upperci, survey_abbrev)
  })

  if(is.null(model_names)){
    model_names <- paste0("model ", seq_along(models))
    model_names <- factor(model_names, levels = model_names)
  }
  i <- 0
  fits <- map2(seq_along(models), surv_abbrevs, ~{
    i <<- i + 1
    x <- t(models[[.x]]$mpd$it_hat) %>% as_tibble()
    if(length(x) != length(.y)){
      stop("Did you forget to modify gear name names in the IndexGears entry in your dat file for model '",
           names(models)[.x], "'?",
           call. = FALSE)
    }
    k <- map2(x, .y, ~{
      j <- .x %>% as_tibble() %>% filter(!is.na(value))
      yrs <- surv_indices %>% filter(survey_abbrev == .y) %>% pull(year)
      length(yrs) <- nrow(j)
      as_tibble(yrs) %>%
        bind_cols(j) %>%
        `names<-`(c("year", "biomass")) %>%
        mutate(survey_abbrev = .y)
    }) %>%
      map_df(~{.x}) %>%
      mutate(model = model_names[.x]) %>%
      mutate(biomass = ifelse(survey_abbrev == "DCPUE", biomass, biomass * 1e6))
    k
  }) %>%
    map_df(~{.x})

  # Re-order the legend and facets
  model_names <- factor(model_names, levels = model_names)
  surv_abbrev <- factor(surv_abbrev, levels = surv_abbrev)
  fits <- fits %>%
    mutate(model = fct_relevel(model, levels(!!model_names))) %>%
    mutate(survey_abbrev = fct_relevel(survey_abbrev, levels(!!surv_abbrev)))
  surv_indices <- surv_indices %>%
    mutate(survey_abbrev = fct_relevel(survey_abbrev, levels(!!surv_abbrev)))

  # Rescale values
  surv_indices <- surv_indices %>%
    mutate_at(vars(biomass, lowerci, upperci),
              function(x){
                ifelse(.$survey_abbrev == "DCPUE", x,  x / 1e6)
              })
  fits <- fits %>%
    mutate(biomass = ifelse(survey_abbrev == "DCPUE", biomass, biomass / 1e6))

  ggplot(surv_indices, aes(x = year, y = biomass)) +
    #geom_ribbon(aes(ymin = lowerci, ymax = upperci), fill = "red", alpha = 0.3) +
    #geom_line(size = line_width, color = "red") +
    geom_errorbar(aes(ymin = lowerci, ymax = upperci), alpha = 0.3) +
    geom_point() +
    geom_line(data = fits, aes(color = model), size = line_width) +
    geom_point(data = fits, aes(color = model), size = point_size)+
    facet_wrap(~survey_abbrev, scales = "free_y") +
    xlab("Year") +
    ylab("Index (thousand tonnes, DCPUE ~ kg/hr)") +
    scale_color_brewer(palette = palette) +
    guides(color = guide_legend(title = legend_title)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
