#' An extended version of [coda::autocorr.plot()]
#'
#' @details
#' This code was copied exactly from [coda::autocorr.plot()] and modified to pass
#' arguments to [base::plot()] and to remove the `ask` argument.
#'
#' @param x See [coda::autocorr.plot()]
#' @param lag_max [coda::autocorr.plot()]
#' @param auto_layout [coda::autocorr.plot()]
#' @param ... Arguments passed to [base::plot()]
#'
#' @return Nothing
#' @export
autocorr_plot <- function(x,
                          lag_max,
                          auto_layout = TRUE,
                          ...){

  if(auto_layout){
    oldpar <- par(mfrow = coda:::set.mfrow(Nchains = coda:::nchain(x),
                                           Nparms = coda:::nvar(x)))
  }
  if(!coda:::is.mcmc.list(x)){
    x <- coda:::mcmc.list(coda:::as.mcmc(x))
  }

  for(i in 1:coda:::nchain(x)){
    if(missing(lag_max)){
      xacf <- acf(coda:::as.ts.mcmc(x[[i]]),
          plot = FALSE)
    }else{
      xacf <- acf(coda:::as.ts.mcmc(x[[i]]),
                  lag.max = lag_max,
                  plot = FALSE)
    }
    for(j in 1:coda:::nvar(x)){
      plot(xacf$lag[, j, j],
           xacf$acf[, j, j],
           type = "h",
           ylab = "Autocorrelation",
           xlab = "Lag",
           ylim = c(-1, 1),
           ...)
    }
  }
  invisible(x)
}
