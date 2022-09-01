#' Calculate logs for several parameters using MPD
#'
#' @param mpd A list of MPD outputs
#' @param log_params Patterns to match for parameter names that need to be logged
#' @details The new list elements will have the same names but have 'log_' appended
#'
#' @return  a list (mpd with some new elements appended, the log-applied elements)
#' @export
calc_mpd_logs <- function(mpd,
                          log_params = c("^ro$",
                                         "^m$",
                                         "^rbar$",
                                         "^rinit$",
                                         "^q$")){

  inds <- map(log_params, ~{
    grp <- grep(.x, names(mpd))
    if(!length(grp)){
      return(NULL)
    }
    grp
  })
  inds[sapply(inds, is.null)] <- NULL
  if(!length(inds)){
    return(mpd)
  }
  inds <- inds %>% map_dbl(~{.x})
  log_names <- paste0("log_", names(mpd)[inds])
  vals <- map(mpd[inds], log)
  names(vals) <- log_names
  c(mpd, vals)
}
