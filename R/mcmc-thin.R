#' Apply burnin and thinning to an MCMC posteriors data frame
#'
#' @param mcmc_dat A data frame of the MCMC posteriors
#' @param burnin The number of samples to burn away from the beginning of
#' the `mcmc_dat` data frame
#' @param thin The thinning, or extract every Nth row from `mcmc_dat` AFTER
#' `burnin` has been removed from it
#'
#' @return The modified `mcmc_dat` data frame
#' @export
mcmc_thin <- function(mcmc_dat,
                      burnin,
                      thin){

  if(!"data.frame" %in% class(mcmc_dat)){
    stop("`mcmc_dat` is not a data frame", call. = FALSE)
  }

  if(nrow(mcmc_dat) <= burnin){
    stop("`burnin` is too large. `mcmc_dat` has ", nrow(mcmc_dat), " rows ",
         "and `burnin` is ", burnin, call. = FALSE)
  }

  as_tibble(mcmc_dat) %>%
    slice((burnin + 1):nrow(.)) %>%
    slice(seq(1, nrow(.), thin))
}
