#' Plot the MCMC time series trajectories for iscam models, including spawning biomass
#' and recruitment for both absolute and relative cases.
#'
#' @return Nothing
#' @importFrom tibble rownames_to_column
#' @importFrom forcats fct_relevel
#' @export
plot_biomass_mpd <- function(models,
                             model_names = NULL,
                             type = c("sbt", "rt"),
                             rel = FALSE,
                             show_initial = TRUE,
                             legend_title = "Models",
                             palette = "Paired",
                             xlim = NULL,
                             ylim = NULL,
                             line_width = 1,
                             point_size = 2,
                             line_ribbon = FALSE,
                             refpts_ribbon = TRUE,
                             alpha = 0.2,
                             refpts_alpha = alpha,
                             offset = 0.1,
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
  init_vals <- tibble(year = start_year - 1,
                      model = names(init),
                      sbt = map_dbl(init, ~{.x})) %>%
    mutate(year = start_year - 1)

  if(type == "sbt"){
    bind_yrs <- start_year:end_year
  }else if(type == "rt"){
    bind_yrs <- (start_year + 1):(end_year + 1)
  }
  vals <- bind_yrs %>%
    as_tibble() %>%
    `names<-`("year") %>%
    bind_cols(t(sbt)) %>%
    pivot_longer(cols = -year, names_to = "model", values_to = "sbt")
browser()
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
    browser()
    vals <- vals %>%
      mutate(year = ifelse(show_initial, xlim[1] - 1, xlim[1])) %>%
      filter(year %in% xlim[1]:xlim[2])
  }

  # 'Dodge' B0 points manually
  if((nrow(init_vals) - 1) * bo_dodge >= 1){
    warning("`bo_dodge` value of ", bo_dodge, " makes B0 values span a year or more. ",
            "This will cause overlapping in the plot with the main time series")
  }
  init_vals <- init_vals %>%
    mutate(year = seq(from = first(year), by = bo_dodge, length.out = nrow(.)))
browser()
  g <- ggplot(vals, aes(x = year, y = sbt, color = model)) +
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
