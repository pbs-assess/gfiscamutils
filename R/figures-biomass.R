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

#' Plot the MCMC time series trajectories for iscam models, including spawning biomass
#' and recruitment for both absolute and relative cases.
#'
#' @param models A list of iscam model objects
#' @param model_names Names to use for the models in the plots. The names of
#' the list items in `models` will be used if they are present and this will
#' be ignored. If the list item names are not defined, temporary names will be used
#' (Temporary model 1, Temporary model 2, etc.)
#' @param type Either 'sbt' for Spawning biomass or 'rt' for Recruitment
#' @param rel Logical. Make plot relative to initial estimate (B0 or R0 depending
#' on the choice for `type`
#' @param legend_title Title for legend
#' @param xlim The x limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param ylim The y limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param line_width Width of all median lines on the plot
#' @param point_size Point size for all median points on the plot
#' @param lineribbon Logical. If `TRUE`, make the first model plotted an envelope
#' of the credible interval, surrounding the median line
#' @param alpha The opacity between 0 to 1 of the envelope shown when `lineribbon == TRUE`
#' @param offset The amount on the x-axis to offset each point and line for
#' multiple models. Used for recruitment plots
#' @param bo_dodge The amount to offset the initial value (B0 or R0) values from each
#' other so the values and uncertainty can be easily seen for multiple models
#' @param x_space The amount of x-interval space to pad the left and right of the plot
#' with. To remove all padding, make this 0
#' @param append_base_txt Text to append to the first model's name for display on the
#' plot legend
#' @param show_bmsy_line Show the Bmsy lines (0.4 and 0.8 Bmsy)
#' @param ind_letter A letter to place in the upper left corner of the plot. If `NULL`,
#' nothing will be shown
#' @param leg A vector of two values representing the X/Y coordinates inside the plot to
#' place the legend. If `NULL`, the default placement is used
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param ... Other graphical arguments
#'
#' @return Nothing
#' @export
plot_ts_mcmc <- function(models,
                         model_names = NULL,
                         type = "sbt",
                         rel = FALSE,
                         legend_title = "Models",
                         xlim = NULL,
                         ylim = NULL,
                         line_width = 1,
                         point_size = 2,
                         lineribbon = FALSE,
                         alpha = 0.2,
                         offset = 0.1,
                         bo_dodge = 0.1,
                         x_space = 0.5,
                         append_base_txt = NULL,
                         show_bmsy_line = FALSE,
                         ind_letter = NULL,
                         leg_loc = NULL,
                         probs = c(0.025, 0.5, 0.975),
                         ...){

  if(!type %in% c("sbt", "rt")){
    stop("type '", type, "' is not one of the implemented time series", call. = FALSE)
  }

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

  if(length(models) > 13){
    stop("Cannot plot more than 13 models due to palette restrictions (See RColorBrewer 'Paired' palette)")
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  start_yr <- map_dbl(models, ~{.x$dat$start.yr}) %>% min
  end_yr <- map_dbl(models, ~{.x$dat$end.yr}) %>% max
  len <- end_yr - start_yr + 1
  bind_yrs <- start_yr:end_yr

  if(type == "sbt"){
    val <- ifelse(rel, "depl_quants", "sbt_quants")
    ts_quants <- map(models, ~{
      j <- .x$mcmccalcs[[val]]
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
  }else{
    names(ts_quants)[1] <- paste0(names(ts_quants)[1], append_base_txt)
    names(tso_quants)[1] <- paste0(names(tso_quants)[1], append_base_txt)
  }

  nms <- names(ts_quants)
  tso_quants <- tso_quants %>%
    bind_rows() %>%
    mutate(model = nms, year = start_yr - 1) %>%
    select(model, year, everything())

  ts_quants <- imap(ts_quants, ~{
    .x %>%
      t() %>%
      as.data.frame %>%
      add_rownames(var = "year") %>%
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

  if(rel){
    y_label <- switch(type,
                      "sbt" = "Relative Spawning biomass",
                      "rt" = "Relative Recruitment")
  }else{
    y_label <- switch(type,
                      "sbt" = "Spawning biomass ('000 tonnes)",
                      "rt" = "Recruitment (millions)")
  }

  if(is.null(xlim)){
    xlim <- c(start_yr, end_yr)
  }else{
    # Remove data prior to first year and change B0 to firs
    tso_quants <- tso_quants %>%
      mutate(year = xlim[1] - 1)
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

  # Color values below came from:
  # c("#000000", RColorBrewer::brewer.pal(name = "Paired", n = 12))
  g <- ts_quants %>%
    ggplot(aes(x = year,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab("Year") +
    ylab(y_label) +
    scale_color_manual(values = c("#000000",
                                  "#A6CEE3",
                                  "#1F78B4",
                                  "#B2DF8A",
                                  "#33A02C",
                                  "#FB9A99",
                                  "#E31A1C",
                                  "#FDBF6F",
                                  "#FF7F00",
                                  "#CAB2D6",
                                  "#6A3D9A",
                                  "#FFFF99",
                                  "#B15928"))

  if(rel){
    g <- g + scale_x_continuous(limits = c(xlim[1], xlim[2]),
                                breaks = min(xlim):max(xlim),
                                labels = xlim[1]:xlim[2],
                                expand = expansion(add = x_space))
  }else{
    g <- g + scale_x_continuous(limits = c(xlim[1] - 1, xlim[2]),
                                breaks = (min(xlim) - 1):max(xlim),
                                labels = c(expression(B[0]), xlim[1]:xlim[2]),
                                expand = expansion(add = x_space))
  }

  if(is.null(ylim)){
    g <- g + scale_y_continuous(limits = c(0, NA), expand = c(0, 0))
  }else{
    g <- g + scale_y_continuous(limits = ylim, expand = c(0, 0))
  }

  if(lineribbon){
    first_model_nm <- names(ts_quants)[1]
    ts_quants_first <- ts_quants %>%
      filter(model == first_model_nm)
    g <- g +
      geom_ribbon(data = ts_quants_first, alpha = alpha) +
      geom_line(aes(color = model), size = line_width) +
      geom_line(aes(y = !!sym(quants[1]), color = model), size = 0.5, lty = 2) +
      geom_line(aes(y = !!sym(quants[3]), color = model), size = 0.5, lty = 2)
  }else{
    g <- g +
      geom_line(aes(color = model), size = line_width) +
      geom_line(aes(y = !!sym(quants[1]), color = model), size = 0.5, lty = 2) +
      geom_line(aes(y = !!sym(quants[3]), color = model), size = 0.5, lty = 2)
  }
  if(!rel){
    g <- g +
      geom_pointrange(data = tso_quants, aes(color = model))
  }

  if(!is.null(leg_loc)){
    g <- g +
      theme(legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "black"))

  }

  g <- g + labs(color = legend_title)

  g
}
