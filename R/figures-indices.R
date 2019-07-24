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
