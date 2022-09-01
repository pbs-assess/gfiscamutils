#' Apply burnin and thinning to the MCMC posteriors
#'
#' @param mcmc_dat A data frame of the MCMC posteriors
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#'
#' @return an mcmc window object (CODA package)
#' @importFrom coda mcmc
#' @export
mcmc_thin <- function(mcmc_dat,
                      burnin,
                      thin){

  if(is.vector(mcmc_dat)){
    mcmc_obj <- mcmc(mcmc_dat)
    mcmc_window <- window(mcmc_obj,
                          start = burnin + 1,
                          thin = thin)
    return(mcmc_window)
  }
  nm <- names(mcmc_dat)
  mcmc_obj <- apply(mcmc_dat, 2, mcmc)
  mcmc_window <- NULL
  for(col in 1:ncol(mcmc_obj)){
    tmp <- window(mcmc_obj[, col],
                  start = burnin + 1,
                  thin = thin)
    mcmc_window <- cbind(mcmc_window, tmp)
  }
  mcmc_window <- as.data.frame(mcmc_window)
  names(mcmc_window) <- nm

  mcmc_window
}
