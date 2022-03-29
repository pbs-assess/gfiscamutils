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
  mpd <- map(models, ~{
    .x$mpd
  })
  sbt <- map(mpd, ~{
    .x[[type]]
  })
  start_year <- map_dbl(models, ~{
    .x$dat$start.yr
  }) %>%
    min
  end_year <- map_dbl(models, ~{
    .x$dat$end.yr
  }) %>%
    max
  sbt <- map(sbt, ~{
    length(.x) = end_year - start_year + 1
    .x
  }) %>%
    bind_rows

  # Get initial estimate, ro, sbo
  init_type <- switch(type,
                      "sbt" = "sbo",
                      "rt" = "ro")
  init <- map(mpd, ~{.x[[init_type]]})
  init_df <- tibble(year = start_year - 1,
                    model = names(init),
                    sbt = map_dbl(init, ~{.x}))

  init_vals <- init %>% as_tibble %>% t() %>% as_tibble(rownames = "model") %>% rename(sbt = V1)

  if(type == "sbt"){
    bind_yrs <- start_year:end_year
  }else if(type == "rt"){
    bind_yrs <- (start_year + 1):(end_year + 1)
  }
  val <- bind_yrs %>%
    as_tibble() %>%
    `names<-`("year") %>%
    bind_cols(sbt) %>% pivot_longer(cols = -year, names_to = "model", values_to = "sbt")

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
#' @export
make.biomass.mcmc.plot <- function(models,
                                   model_names = NULL,
                                   ylim,
                                   opacity = 75,
                                   offset = 0.1,
                                   append_base_txt = NULL,
                                   show_bmsy_line = FALSE,
                                   show_bo_line = FALSE,
                                   ind_letter = NULL,
                                   leg = NULL,
                                   ...){

  #par(mar = c(5.1, 5.1, 4.1, 3.1))

  if(!is.null(models$path)){
    # Single model, not in a list
    models <- list(models)
  }
  sbt_quants <- map(models, ~{.x$mcmccalcs$sbt_quants})
  ro_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "ro"]})
  sbo <- map(models, ~{.x$mcmccalcs$params[, colnames(.x$mcmccalcs$params) == "sbo"]})
  sbo_quants <- map(models, ~{.x$mcmccalcs$params_quants[, colnames(.x$mcmccalcs$params_quants) == "sbo"]})
  yrs <- map(sbt_quants, ~{as.numeric(colnames(.x))})

  xlim <- map(seq_along(yrs), ~{c(min(yrs[[.x]]), max(yrs[[.x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  if(is.null(dev.list())){
    # If layout() is used outside this function,
    #  it calls plot.new and will mess up the figures
    #  if we call it again
    plot.new()
  }
  plot.window(xlim = xlim, ylim = ylim, xlab = "", ylab = "")

  nul <- imap(yrs, ~{
    draw.envelope(yrs[[.y]],
                  sbt_quants[[.y]],
                  xlab = "",
                  ylab = "",
                  col = x,
                  las = 1,
                  xlim = xlim,
                  ylim = ylim,
                  opacity = opacity,
                  first = ifelse(x == 1, TRUE, FALSE),
                  ...)
    }, ...)
  # Add sbo points and ci bars
  nul <- imap(yrs, ~{
    points(yrs[[.y]][1] - (x - 1) * offset,
           sbo[[.y]][2],
           pch = 19,
           col = .y)
    arrows(yrs[[.y]][1] - (.y - 1) * offset,
           sbo[[.y]][1],
           yrs[[.y]][1] - (.y - 1) * offset,
           sbo[[.y]][3],
           lwd = 2,
           code = 0,
           col = .y)
  })

  if(show_bo_line){
    abline(h = 0.4 * sbo[[1]][2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.4SB"[0]),
          4,
          at = 0.4 * sbo[[1]][2],
          col = "red",
          las = 1)
  }

  #TODO - continue here
  if(show_bmsy_line){
    sbmsy_raw <- r.quants[[1]][rownames(r.quants[[1]]) == "bmsy", ]
    sbmsy <- as.numeric(sbmsy.raw[2:4])
    abline(h = 0.4 * sbmsy[2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.4B"[MSY]),
          4,
          at = 0.4 * sbmsy[2],
          col = "red",
          las = 1)
    abline(h = 0.8 * sbmsy[2],
           col = "green",
           lty = 1,
           lwd = 2)
    mtext(expression("0.8B"[MSY]),
          4,
          at = 0.8 * sbmsy[2],
          col = "green",
          las = 1)
  }
  mtext("Year", 1, line = 3)
  mtext("Biomass (1000 mt)", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    if(!is.null(append.base.txt)){
      model.names[[1]] <- paste0(model.names[[1]],
                                 append.base.txt)
    }
    legend(leg,
           model.names,
           bg = "transparent",
           col = 1:length(models),
           lty = 1,
           lwd = 2)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

#' Plot the relative biomass trajectories for iscam models
#'
#' Plot the relative biomass with credibility intervals for the mcmc
#' cases of the models
#'
#' @rdname make.biomass.mcmc.plot
#' @export
make.depletion.mcmc.plot <- function(models,
                                     model.names = NULL,
                                     ylim = c(0, 1),
                                     opacity = 75,
                                     append.base.txt = NULL,
                                     ind.letter = NULL,
                                     leg = NULL,
                                     ...
                                     ){

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  depl <- lapply(models,
                 function(x){
                   x$mcmccalcs$depl.quants})
  yrs <- lapply(depl,
                function(x){
                  as.numeric(colnames(x))})
  xlim <- lapply(1:length(yrs),
                 function(x){
                   c(min(yrs[[x]]), max(yrs[[x]]))})
  xlim <- do.call(rbind, xlim)
  xlim <- c(min(xlim), max(xlim))

  if(is.null(dev.list())){
    ## If layout() is used outside this function,
    ##  it calls plot.new and will mess up the figures
    ##  if we call it again
    plot.new()
  }
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")

  lapply(1:length(yrs),
         function(x){
           draw.envelope(yrs[[x]],
                         depl[[x]],
                         ylab = "",
                         xlab = "",
                         col = x,
                         las = 1,
                         xlim = xlim,
                         ylim = ylim,
                         opacity = opacity,
                         first = ifelse(x == 1, TRUE, FALSE),
                         ...)})
  mtext("Year", 1, line = 3)
  mtext("Depletion", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    if(!is.null(append.base.txt)){
      model.names[[1]] <- paste0(model.names[[1]],
                                 append.base.txt)
    }
    legend(leg,
           model.names,
           bg = "transparent",
           col = 1:length(models),
           lty = 1,
           lwd = 2)
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}

#' Plot the MPD biomass time series along with the associated retrospectives.
#'
#' Biomass can be absolute, or relative (i.e., depletion). Also plot estimated
#' unfished spawning biomass as points.
#'
#' @param model an iscam model object
#' @param depl logical; if TRUE, plot the depletion line(s)
#' @param xlim numeric vector of length 2; trim the x-axis (default NA)
#' @param offset horizontal offset for B0 points in depletion plot
#' @param leg show the legend? Logical
#' @param a_trans transparency for lines and points
#' @param translate logical; translate labels
#'
#' @return a ggplot object
#' @export
#' @importFrom ggplot2 aes geom_line scale_y_continuous coord_cartesian
#' scale_x_continuous geom_point position_dodge ylab labs scale_colour_viridis_d
#' guides guide_legend
#' @importFrom reshape2 melt
#' @importFrom dplyr rename rename_at vars contains funs bind_cols
#' @importFrom scales comma
#' @importFrom tibble as.tibble
#' @importFrom forcats fct_relevel
biomass.plot.mpd <- function(model,
                             depl = FALSE,
                             xlim = NA,
                             offset = 0.7,
                             leg = TRUE,
                             a_trans = 0.75,
                             translate = FALSE){

  if(class(model) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != mdl_cls){
      stop("The structure of the model list is incorrect.")
    }
  }
  base_model_lst <- list(model)
  class(base_model_lst) <- mdl_lst_cls
  models <- c(base_model_lst, model$retro)

  yrs <- lapply(models,
                function(x){
                  x$mpd$syr:(x$mpd$nyr + 1)})
  if(depl){
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt / x$mpd$sbo })
  }else{
    bt <- lapply(models,
                 function(x){
                   x$mpd$sbt})
  }
  bt <- lapply(1:length(bt),
               function(x){
                 tmp <- as.data.frame(t(bt[[x]]))
                 rownames(tmp) <- "Biomass (t)"
                 colnames(tmp) <-  yrs[[x]]
                 tmp})
  models.names <- paste0("-", 1:(length(bt) - 1), " ",
                         en2fr("Years", translate, case="lower"))
  models.names <- c(en2fr("Base model", translate, case="sentence"),
                    models.names)
  names(bt) <- models.names

  bt <- bind_rows(bt, .id = "Sensitivity") %>%
    melt() %>%
    as.tibble() %>%
    mutate(Year = variable, `Biomass (t)` = value) %>%
    select(-c(variable, value)) %>%
    mutate(Year = as.numeric(as.character(Year))) %>%
    mutate(Sensitivity = fct_relevel(Sensitivity,
                                     models.names,
                                     after = 0))

  bo <- lapply(models,
               function(x){
                 x$mpd$sbo
               })
  names(bo) <- models.names
  bo <- t(bind_cols(bo))
  bo <- cbind(rownames(bo), bo, min(bt$Year))
  colnames(bo) <- c("Sensitivity", "Biomass (t)", "Year")
  bo <- bo %>%
    as.tibble() %>%
    mutate(`Biomass (t)` = as.numeric(`Biomass (t)`),
           Year = as.numeric(Year))

  p <- ggplot(bt, aes(x = Year,
                      y = `Biomass (t)`,
                      #ymin = 0,
                      #ymax = max(`Biomass (t)`),
                      group = Sensitivity)) +
    geom_line(aes(color = Sensitivity),
              size = 1,
              na.rm = TRUE,
              alpha = a_trans) +
    scale_colour_viridis_d( ) +
    theme(legend.position = "top",
          #legend.justification = c(1, 1),
          legend.title = element_blank()) +
    guides(fill = guide_legend(nrow = 2)) +
    labs( x=en2fr("Year", translate, case="sentence"),
          y=paste( en2fr("Biomass", translate, case="sentence"), "(t)") ) +
    scale_y_continuous(labels = comma,
                       limits = c(0, NA)) +
    coord_cartesian(expand = TRUE) +
    scale_x_continuous(breaks = seq(0, 3000, 5))

  if(!is.na(xlim[1])){
    p <- p +
      xlim(xlim[1], xlim[2])
  }

  if(!depl){
    if(!is.na(xlim[1])){
      bo$Year <- xlim[1]
    }
    p <- p + geom_point(data = bo,
                        size = 2,
                        position = position_dodge(width = offset),
                        mapping = aes(color = Sensitivity),
                        show.legend = FALSE,
                        na.rm = TRUE,
                        alpha = a_trans)
  }

  if(depl){
    p <- p + ylab(en2fr("Relative biomass", translate, case="sentence"))
  }

  if(length(models) == 1){
    p <- p + theme(legend.position = "none")
  }

  p
}
