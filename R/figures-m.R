#' Plot natural mortality for iscam models
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
make.m.plot <- function(models,
                        model.names = NULL,
                        ylim,
                        opacity = 75,
                        offset = 0.1,
                        append.base.txt = NULL,
                        show.bmsy.line = FALSE,
                        show.bo.line = FALSE,
                        ind.letter = NULL,
                        leg = NULL,
                        ...
                        ){

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
  mtext("Natural Mortality (M)", 2, line = 3)

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
