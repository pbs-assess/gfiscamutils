#' Simplify MCMC parameter names for this package code
#'
#' @param dat  A data frame of posteriors as seen in the MCMC output csv files
#'
#' @return The original data frame with the area and group prefixes removed from the column names
#' @export
simplify_names <- function(dat){

  pnames <- names(dat)
  pnames <- gsub("m_sex([[:digit:]]+)", "m\\1", pnames)
  pnames <- gsub("msy_fleet([[:digit:]]+)", "msy\\1", pnames)
  pnames <- gsub("fmsy_fleet([[:digit:]]+)", "fmsy\\1", pnames)
  pnames <- gsub("umsy_fleet([[:digit:]]+)", "umsy\\1", pnames)
  pnames <- gsub("q_gear([[:digit:]]+)",  "q\\1", pnames)
  pnames <- gsub("SSB", "ssb", pnames)
  pnames <- gsub("sel_sd50_female_gear([[:digit:]]+)", "selsd\\1_female", pnames)
  pnames <- gsub("sel_sd50_male_gear([[:digit:]]+)", "selsd\\1_male", pnames)
  pnames <- gsub("sel_age50_female_gear([[:digit:]]+)", "selage\\1_female", pnames)
  pnames <- gsub("sel_age50_male_gear([[:digit:]]+)", "selage\\1_male", pnames)

  names(dat) <- pnames
  # Remove objective function value
  dat[, names(dat) != "f"]
}

#' Strip static parameters from MCMC output data
#'
#' @param model An iscam model object
#' @param dat A data frame of the MCMC parameter estimates
#'
#' @details Strip out the static (non-estimated) parameters from the mcmc output data
#'   for the given scenario. We only need to see estimated parameters on the
#'   diagnostic plots. If there are no static parameters, NULL will be returned
#'
#' @return The parameters data frame for the model given with static parameters removed
#' @export
strip_static_params <- function(model, dat){
  # Strip out the static (non-estimated) parameters from the mcmc output data
  #  for the given scenario. We only need to see estimated parameters on the
  #  diagnostic plots. If there are no static parameters, NULL will be returned

  # Check the control file to see which parameters were static
  inp <- as.data.frame(model$ctl$params)
  static <- inp[inp$phz <= 0, ]
  snames <- rownames(static)

  # Now remove those from the mcmc data
  pnames <- names(dat)
  # remove the log_ stuff from the input parameter names
  snames <- gsub("log_", "", snames)

  # Remove static selectivity params
  sel_params <- as.data.frame(model$ctl$sel)
  est_phase <- sel_params["estphase", ]
  static_sel <- est_phase < 1
  sel_post_names <- names(dat)[grep("sel_age50.*", names(dat))]
  gear_nums <- map_dbl(sel_post_names, ~{
    as.numeric(gsub(".*([0-9]+)", "\\1", .x))
  })
  static_sel_1 <- gear_nums %in% which(static_sel)
  if(sum(static_sel)){
    snames <- c(snames, sel_post_names[static_sel_1])
  }

  sel_sd_post_names <- names(dat)[grep("sel_sd50.*", names(dat))]
  gear_nums <- map_dbl(sel_sd_post_names, ~{
    as.numeric(gsub(".*([0-9]+)", "\\1", .x))
  })
  static_sel_2 <- gear_nums %in% which(static_sel)
  if(sum(static_sel)){
    snames <- c(snames, sel_sd_post_names[static_sel_2])
  }

  dat[, !names(dat) %in% snames]
}
