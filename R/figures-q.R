#' Plot the catchability trajectories for iscam models
#'
#' @param models A list of iscam model objects
#' @param model.names A vector of names to show on the legend
#' @param ylim The y limits for the plot
#' @param opacity How opaque the credibility envelopes are
#' @param ind.letter A letter to show on the plot (for panel plots)
#' @param leg Position of the legend. NULL means no legend is shown
#' @param ... Other graphical arguments
#'
#' @return Nothing
#' @export
make.q.plot <- function(models,
                        model.names = NULL,
                        ylim,
                        opacity = 75,
                        ind.letter = NULL,
                        leg = NULL,
                        ...
                        ){

  par(mar = c(5.1, 5.1, 4.1, 3.1))

  end.yr <- models[[1]]$dat$end.yr

  q1.len <- length(1951:1987)
  q2.len <- length(1988:end.yr)
  yrs <- 1951:end.yr
  xlim <- c(1951, end.yr)
  rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
  }
  q.quants <- lapply(models,
                     function(x){
                       q <- x$mcmccalcs$q.quants
                       q1.rep <- rep.col(q[,1], q1.len)
                       q2.rep <- rep.col(q[,2], q2.len)
                       j <- cbind(q1.rep, q2.rep)
                     })

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

  lapply(1:length(q.quants),
         function(x){
           draw.envelope(yrs,
                         q.quants[[x]],
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
  mtext("Survey q", 2, line = 3)

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
