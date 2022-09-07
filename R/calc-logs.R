#' Calculate logs for several parameters using MCMC
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#' @param log.params A vector of regular expressions to determine which
#'   parameters to apply the log function to
#'
#' @details The column names will be prepended with log_ for the parameters which
#'   had the log function applied
#'
#' @return a data frame (mc with the log columns appended)
#' @export
calc_logs <- function(mc,
                      log.params = c("^ro$",
                                     "^m$",
                                     "^m_sex1$",
                                     "^m_sex2$",
                                     "^rbar$",
                                     "^rinit$",
                                     "^q_gear[1-9]+$")){

  nm <- colnames(mc)

  inds <- map(log.params, ~{
    grep(.x, nm)
  }) %>%
    `[`(lengths(.) > 0) |>
    unlist()

  colnames(mc)[inds] <- paste0("log_", colnames(mc)[inds])
  mc[,inds] <- apply(mc[,inds],
                     2,
                     log)
  mc
}
