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
                                   model.names = NULL,
                                   ylim,
                                   opacity = 75,
                                   offset = 0.1,
                                   append.base.txt = NULL,
                                   show.bmsy.line = FALSE,
                                   show.bo.line = FALSE,
                                   ind.letter = NULL,
                                   leg = NULL,
                                   ...){

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  sbt.quants <- lapply(models,
                       function(x){
                         x$mcmccalcs$sbt.quants})
  r.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$r.quants})
  sbo.raw <- lapply(r.quants,
                    function(x){
                      x[rownames(x) == "sbo", ]})
  sbo <- lapply(sbo.raw,
                function(x){
                  as.numeric(x[,2:4])})
  yrs <- lapply(sbt.quants,
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
                         sbt.quants[[x]],
                         xlab = "",
                         ylab = "",
                         col = x,
                         las = 1,
                         xlim = xlim,
                         ylim = ylim,
                         opacity = opacity,
                         first = ifelse(x == 1, TRUE, FALSE),
                         ...)})
  ## Add sbo points and ci bars
  lapply(1:length(yrs),
         function(x){
           points(yrs[[x]][1] - (x - 1) * offset,
                  sbo[[x]][2],
                  pch = 19,
                  col = x)
           arrows(yrs[[x]][1] - (x - 1) * offset,
                  sbo[[x]][1],
                  yrs[[x]][1] - (x - 1) * offset,
                  sbo[[x]][3],
                  lwd = 2,
                  code = 0,
                  col = x)})

  if(show.bo.line){
    abline(h = 0.3 * sbo[[1]][2],
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.3SB"[0]),
          4,
          at = 0.3 * sbo[[1]][2],
          col = "red",
          las = 1)
  }
  if(show.bmsy.line){
    sbmsy.raw <- r.quants[[1]][rownames(r.quants[[1]]) == "bmsy", ]
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
#' @rdName make.biomass.mcmc.plot
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

#' @param base.model The base iscam model object
#' @param models A list of iscam model objects to plot against the base model
#' @param model.names A vector of names to show on the legend
#' @param ylim The y limits for the plot
#' @param offset The amount on the x-axis to offset each point and line for
#'   multiple models
#' @param show.bo.line Show the reference lines 0.2 and 0.4bo
#' @param ind.letter A letter to show on the plot (for panel plots)
#' @param leg Position of the legend. NULL means no legend is shown
#' @param color.brew.class The RColorBrewer class, e.g. "Paired"
#' @param ... Other graphical arguments
#'
#' @return Nothing
#' @export
#' @importFom grDevices colorRampPalette
make.biomass.retro.mpd.plot <- function(base.model,
                                        models,
                                        model.names = NULL,
                                        ylim,
                                        offset = 0.1,
                                        show.bo.line = FALSE,
                                        ind.letter = NULL,
                                        leg = TRUE,
                                        color.brew.class = "Paired",
                                        ...
                                        ){

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  sbt <- lapply(models,
                function(x){
                  x[[1]]$mpd$sbt})

  sbo <- base.model$mpd$sbo

  yrs <- lapply(models,
                function(x){
                  x[[1]]$mpd$syr:(x[[1]]$mpd$nyr + 1)})
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

  base.yrs <- base.model$mpd$syr:(base.model$mpd$nyr + 1)
  base.sbt <- base.model$mpd$sbt

  cols <- colorRampPalette(c("red", "blue", "green"))(length(model.names))

  if(leg){
    layout(1:2, heights = c(1, 5))
    par(mar = rep(0, 4))
    plot(0, 0, type="n", ann=FALSE, axes=FALSE)
    legend("center",
           model.names,
           bg = "transparent",
           ncol = 6,
           col = c(1, cols),
           lty = c(1, rep(2, length(model.names) - 1)),
           lwd = 2)
    par(mar=c(5,4,0,2))
  }
  plot(base.yrs,
       base.sbt,
       col = 1,
       lwd = 3,
       las = 1,
       lty = 1,
       xlim = xlim,
       ylim = ylim,
       xlab = "",
       ylab = "",
       type = "l",
       ...)
  lapply(1:length(yrs),
         function(x){
           lines(yrs[[x]],
                 sbt[[x]],
                 xlab = "",
                 ylab = "",
                 col = cols[x],
                 las = 1,
                 lwd = 2,
                 lty = 2,
                 xlim = xlim,
                 ylim = ylim,
                 ...)})

  if(show.bo.line){
    abline(h = 0.3 * sbo,
           col = "red",
           lty = 1,
           lwd = 2)
    mtext(expression("0.3SB"[0]),
          4,
          at = 0.3 * sbo,
          col = "red",
          las = 1)
  }

  mtext("Year", 1, line = 3)
  mtext("Biomass (1000 mt)", 2, line = 3)

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
