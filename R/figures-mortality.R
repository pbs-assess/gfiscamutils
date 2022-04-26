#' Plot the natural mortality for MCMC cases for iscam models
#'
#' @param models A list of iscam model objects
#' @param model.names A vector of names to show on the legend
#' @param which.gear The gear index to plot
#' @param ylim The y limits for the plot
#' @param opacity How opaque the credibility envelopes are
#' @param leg Position of the legend. NULL means no legend is shown
#' @param ... Other graphical arguments
#'
#' @details Plot the natural mortality with credibility intervals for the mcmc
#'   case of the models
#'
#' @return Nothing
#' @export
make.natural.mort.mcmc.plot <- function(models,
                                        model.names = NULL,
                                        which.gear = 1,
                                        ylim,
                                        opacity = 75,
                                        leg = NULL,
                                        ...
                                        ){
  par(mar = c(5.1, 5.1, 4.1, 3.1))

  m.quants <- lapply(models,
                     function(x){
                       x$mcmccalcs$nat.mort.quants})
  yrs <- lapply(m.quants,
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
                         m.quants[[x]],
                         xlab = "",
                         ylab = "",
                         col = x,
                         las = 1,
                         xlim = xlim,
                         ylim = ylim,
                         opacity = opacity,
                         first = ifelse(x == 1, TRUE, FALSE),
                         ...)})

  mtext("Year", 1, line = 3)
  mtext("Natural mortality", 2, line = 3)

  if(!is.null(model.names) & !is.null(leg)){
    legend(leg,
           model.names,
           bg = "transparent",
           col = 1:length(models),
           lty = 1,
           lwd = 2)
  }

}
