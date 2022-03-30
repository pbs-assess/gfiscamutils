#' Plot biomass for MPD models against each other. Typically used for bridge models
#'
#' @param models Model list as output by [model_setup()]
#' @param model_names A vector of model names to show in plots of the same length as `model`
#' @param type Which value to plot. sbt = spawning biomass (and sbo), rt = recruitment (and ro)
#' @param rel If `TRUE`, plot relative to initial value
#' @param legend_title Title to use for the legend
#' @param palette The [RColorBrewer::brewer.pal.info] palette to use for line and point color
#' @param line_width The width of the lines
#' @param point_size The size of the points

#' @return A [ggplot2::ggplot()] object
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 scale_color_viridis_d xlab ylab ylim
#' @importFrom purrr map_dbl
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' \dontrun{
#' library(here)
#' library(gfiscamutils)
#' bridge_models_text <- c("2015 Base model",
#'                         "Update data to 2019",
#'                         "Remove HS MSA survey",
#'                         "Add Discard CPUE index",
#'                         "Convert model to split sex",
#'                         "Change fishing year to Feb 21 - Feb 20")
#' bridge_models_text <- factor(bridge_models_text, levels = bridge_models_text)
#' drs <- arrowtooth::set_dirs(base_model_dir = "base",
#'                             bridge_models_dirs = bridge_models_dirs,
#'                             sens_models_dirs = NULL)
#' models <- arrowtooth::model_setup(main_dirs = drs,
#'                                   bridge_models_text = bridge_models_text,
#'                                   overwrite_rds_files = TRUE)
#' plot_biomass_mpd(models$bridge_models, bridge_models_text)
#' }
plot_ts_mpd <- function(models,
                        model_names = factor(names(models), levels = names(models)),
                        type = "sbt",
                        rel = FALSE,
                        legend_title = "Bridge model",
                        palette = "Paired",
                        line_width = 1,
                        point_size = 2){

  if(!type %in% c("sbt", "rt")){
    stop("type '", type, "' is not one of the implemented time series", call. = FALSE)
  }

  if(class(models) != mdl_lst_cls){
    stop("The `models` list is not a gfiscamutils::mdl_lst_cls class. If you are trying to plot ",
         "a single model, modify it like this first:\n\n",
         "model <- list(model)\n",
         "class(model) <- mdl_lst_cls\n")
  }

  mpd <- map(models, ~{.x$mpd})
  sbt <- map(mpd, ~{.x[[type]]})
  start_year <- map_dbl(models, ~{.x$dat$start.yr}) %>% min
  end_year <- map_dbl(models, ~{.x$dat$end.yr}) %>% max
  sbt <- map(sbt, ~{
    length(.x) = end_year - start_year + 1
    .x
  }) %>%
    do.call(rbind, .)

  # Get initial estimate, ro, sbo
  init_type <- switch(type,
                      "sbt" = "sbo",
                      "rt" = "ro")
  init <- map(mpd, ~{.x[[init_type]]})
  init_df <- tibble(year = start_year - 1,
                    model = names(init),
                    sbt = map_dbl(init, ~{.x}))
  init_vals <- init_df %>% select(-year)
browser()
  if(type == "sbt"){
    bind_yrs <- start_year:end_year
  }else if(type == "rt"){
    bind_yrs <- (start_year + 1):(end_year + 1)
  }
  val <- bind_yrs %>%
    as_tibble() %>%
    `names<-`("year") %>%
    bind_cols(t(sbt)) %>% pivot_longer(cols = -year, names_to = "model", values_to = "sbt")

  if(rel){
    val <- val %>% left_join(init_vals, by = "model") %>%
      mutate(sbt = sbt.x / sbt.y) %>%
      select(-sbt.x, -sbt.y)
  }
  if(is.null(model_names)){
    model_names <- paste0("model ", seq_along(models))
    model_names <- factor(model_names, levels = model_names)
  }
  val <- val %>%
    mutate(model = fct_relevel(model, levels(model_names)))

  if(rel){
    y_label <- switch(type,
                      "sbt" = "Relative Spawning biomass",
                      "rt" = "Relative Recruitment")
  }else{
    y_label <- switch(type,
                      "sbt" = "Spawning biomass ('000 tonnes)",
                      "rt" = "Recruitment (millions)")
  }

  g <- ggplot(val, aes(x = year, y = sbt, color = model)) +
    xlab("Year") +
    ylab(y_label) +
    geom_line(size = line_width) +
    geom_point(size = point_size) +
    guides(color = guide_legend(title = legend_title)) +
    scale_color_brewer(palette = palette) +
    scale_x_continuous(breaks = seq(min(bind_yrs), max(bind_yrs), 5))

  if(!rel){
    g <- g +
      geom_point(data = init_df, size = point_size)
  }
  g <- g +
    ylim(0, NA)
  g
}

#' Plot the biomass trajectories for iscam models
#'
#' Plot the biomass with credibility intervals for the mcmc
#' cases of the models
#'
#' @param models A list of iscam model objects
#' @param model.names A vector of names to show on the legend
#' @param ylim The y limits for the plot
#' @param opacity How opaque the credibility envelopes are
#' @param offset The amount on the x-axis to offset each point and line for
#'   multiple models
#' @param append.base.txt Text to append to the name of the first model
#' @param show.bmsy.line Show the reference lines 0.4 and 0.8bmsy
#' @param show.bo.line Show the reference lines 0.2 and 0.4bo
#' @param ind.letter A letter to show on the plot (for panel plots)
#' @param leg Position of the legend. NULL means no legend is shown
#' @param ... Other graphical arguments
#'
#' @return Nothing
#' @importFrom ggdist geom_lineribbon
#' @export
plot_ts_mcmc <- function(models,
                         #model_names = factor(names(models), levels = names(models)),
                         type = "sbt",
                         rel = FALSE,
                         legend_title = "Bridge model",
                         palette = "Paired",
                         line_width = 1,
                         point_size = 2,
                         lineribbon = FALSE,
                         alpha = 0.5,
                         offset = 0.1,
                         append_base_txt = NULL,
                         show_bmsy_line = FALSE,
                         show_bo_line = FALSE,
                         ind_letter = NULL,
                         leg = NULL,
                         ...){

  if(!type %in% c("sbt", "rt")){
    stop("type '", type, "' is not one of the implemented time series", call. = FALSE)
  }

  if(class(models) != mdl_lst_cls){
    stop("The `models` list is not a gfiscamutils::mdl_lst_cls class. If you are trying to plot ",
         "a single model, modify it like this first:\n\n",
         "model <- list(model)\n",
         "class(model) <- mdl_lst_cls\n")
  }

  start_yr <- map_dbl(models, ~{.x$dat$start.yr}) %>% min
  end_yr <- map_dbl(models, ~{.x$dat$end.yr}) %>% max
  len <- end_yr - start_yr + 1
  bind_yrs <- start_yr:end_yr

  if(type == "sbt"){
    ts_quants <- map(models, ~{
      j <- .x$mcmccalcs$sbt_quants
      if(len < ncol(j)){
        j <- j[, 1:len]
      }
      j
    })
    tso_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "sbo"]})
  }else if(type == "rt"){
    ts_quants <- map(models, ~{.x$mcmccalcs$rt_quants})
    tso_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "ro"]})
    bind_yrs <- bind_yrs + 1
  }

  # TODO tso_quants muyst be same format as ts_quants
  tso_quants <- tso_quants %>%
    bind_rows() %>%
    bind_cols(as_tibble(names(tso_quants))) %>%
    select(value, everything()) %>%
    rename(model = value) %>%
    pivot_longer(!model, names_to = "quantile", values_to = "value") %>%
    mutate(year = start_yr - 1) %>%
    select(model, quantile, year, value)

  ts_quants <- imap(ts_quants, ~{
    .x %>%
      t() %>%
      as.data.frame %>%
      add_rownames(var = "year") %>%
      mutate(model = .y) %>%
      select(model, year, everything()) %>%
      mutate(year = as.numeric(year))
    # .x %>%
    #   as.data.frame %>%
    #   add_rownames(var = "quantile") %>%
    #   pivot_longer(!quantile, names_to = "year", values_to = "value") %>%
    #   mutate(model = .y) %>%
    #   select(model, quantile, year, value)
  }) %>%
    bind_rows

  if(rel){
    y_label <- switch(type,
                      "sbt" = "Relative Spawning biomass",
                      "rt" = "Relative Recruitment")
  }else{
    y_label <- switch(type,
                      "sbt" = "Spawning biomass ('000 tonnes)",
                      "rt" = "Recruitment (millions)")
  }
  browser()

  g <- ts_quants %>%
    ggplot(aes(x = year, y = `50%`, ymin = `2.5%`, ymax = `97.5%`, fill = model)) +
    xlab("Year") +
    ylab(y_label) +
    scale_color_brewer(palette = palette)

  if(lineribbon){
    g <- g + geom_lineribbon(alpha = 0.5)
  }else{
    g <- g +
      geom_line(aes(color = model), size = line_width) +
      geom_line(aes(y = `2.5%`, color = model), size = 0.5, lty = 2) +
      geom_line(aes(y = `97.5%`, color = model), size = 0.5, lty = 2)

  }
  browser()

}
