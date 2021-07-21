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
#' @importFrom ggplot2 geom_ribbon facet_wrap
#' @export
plot_index_fit_mpd <- function(models,
                               model_names = factor(names(models), levels = names(models)),
                               surv_index,
                               start_year = 1996,
                               end_year = 2019,
                               legend_title = "Bridge model",
                               palette = "Set3",
                               line_width = 0.5,
                               point_size = 2){

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

  fits <- map2(seq_along(models), surv_abbrevs, ~{
    x <- t(models[[.x]]$mpd$it_hat) %>% as_tibble()
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
    geom_ribbon(aes(ymin = lowerci, ymax = upperci), fill = "red", alpha = 0.3) +
    geom_line(size = line_width, color = "red") +
    geom_line(data = fits, aes(color = model), size = line_width) +
    geom_point(data = fits, aes(color = model), size = point_size)+
    facet_wrap(~survey_abbrev, scales = "free") +
    xlab("Year") +
    ylab("Index (thousand tonnes, DCPUE ~ kg/hr)") +
    scale_color_brewer(palette = palette) +
    guides(color = guide_legend(title = legend_title))
}

#' Plot the index fits for MPD runs only
#'
#' @param models A list of iscam model objects
#' @param model.names A vector of names to show on the legend
#' @param leg Position of the legend. NULL means no legend is shown
#' @param start.yr Year to start the plot on
#' @param end.yr Year to end the plot on
#' @param ind The index to plot
#' @param ylim The y limits for the plot
#' @param ind.letter A letter to show on the plot (for panel plots)
#' @param show.cv Show the CV bars on the plot? TRUE/FALSE
#'
#' @return Nothing
#' @export
make.index.fit.plot <- function(models,
                                model.names = NULL,
                                leg = NULL,
                                start.yr,
                                end.yr,
                                ind,
                                ylim = c(0, 20),
                                ind.letter = NULL,
                                show.cv = FALSE){

  if(length(ind) > 1){
    stop("The length of ind cannot be greater than 1.")
  }

  ## Get index names for the models
  index.dat <- lapply(models,
                      function(x){
                        as.data.frame(x$dat$indices[[ind]])
                      })
  index.fit <- lapply(models,
                      function(x){
                        x$mpd$it_hat[ind,]
                      })
  index.fit <- do.call(rbind, index.fit)

  index.fit <- apply(index.fit,
                     1,
                     function(x){
                       x[!is.na(x)]})

  xlim <- c(start.yr, end.yr)
  yrs <- lapply(index.dat,
                function(x){
                  x$iyr})
  index <- lapply(index.dat,
                  function(x){
                    x$it})
  cv <- lapply(index.dat,
               function(x){
                 1 / x$wt})
  ## Plot the fit first
  plot.new()
  plot.window(xlim = xlim,
              ylim = ylim,
              xlab = "",
              ylab = "")

  p.lines <- lapply(1:length(yrs),
                    function(x){
                      lines(yrs[[x]],
                            index.fit[,x],
                            lwd = 2,
                            xlim = xlim,
                            ylim = ylim,
                            col = x)})

  is.equal <- function(mylist) {
    check.eq <- sapply(mylist[-1],
                       function(x){
                         x == mylist[[1]]})
    as.logical(apply(check.eq, 1, prod))
  }
  ## If all the input data are equal, this is TRUE and
  ##  the crosses will be black
  inp.are.eq <- all(is.equal(index))

  ## Then the points and arrows for the index inputs
  p.pts <- lapply(1:length(yrs),
                  function(x){
                    points(yrs[[x]],
                           index[[x]],
                           pch = 3,
                           col = ifelse(inp.are.eq, 1, x),
                           lwd = 2)})
  if(show.cv){
    p.arrows <- lapply(1:length(yrs),
                       function(x){
                         arrows(yrs[[x]],
                                index[[x]] + cv[[x]] * index[[x]],
                                yrs[[x]],
                                index[[x]] - cv[[x]] * index[[x]],
                                code = 3,
                                angle = 90,
                                length = 0.0,
                                col = 1,
                                lwd = 2)})
  }
  axis(1,
       at = seq(min(xlim), max(xlim)),
       labels = seq(min(xlim), max(xlim)))
  axis(2)
  box()
  mtext("Year", 1, line = 3)
  mtext("1,000 t", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
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
