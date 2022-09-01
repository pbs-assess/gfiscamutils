#' Construct an iscam model object from its input and output files
#'
#' @param model_dir The directory the model is in
#' @param mcmc_subdir subdirectory in which mcmc results for models are stored.
#'  Can be the empty string in which case they will be in the model's root directory
#' @param ... arguments to pass to [calc_mcmc()]
#'
#' @details Load all the iscam files for output and input, and return the model object
#'   If MCMC directory is present, load that and perform calculations for mcmc
#'  parameters.
#'
#' @return An iscam model object
#' @importFrom purrr map_dbl
#' @export
load_iscam_files <- function(model_dir,
                             mcmc_subdir = "mcmc",
                             ...){

  model <- list()
  model$path <- model_dir
  if(!dir.exists(model_dir)){
    stop("The directory ", model_dir, " does not exist")
  }

  # Get the names of the input files
  inp_files <- fetch_file_names(model_dir, iscam.starter.file)
  model$dat.file <- inp_files[[1]]
  model$ctl.file <- inp_files[[2]]
  model$proj.file <- inp_files[[3]]

  # Load the input files
  model$dat <- read_data_file(model$dat.file)
  model$ctl <- read_control_file(model$ctl.file,
                                 model$dat$num.gears,
                                 model$dat$num.age.gears)
  model$proj <- read_projection_file(model$proj.file)
  model$par <- read_par_file(file.path(model_dir, par.file))
  # Load MPD results
  model$mpd <- read_report_file(file.path(model_dir, rep.file))
  # Unflatten A_hat so there are nice dataframes of estimated
  #  numbers-at-age for each gear
  model$mpd$a_obs <- extract_age_output(model, type = "obs")
  model$mpd$a_hat <- extract_age_output(model, type = "est")
  model$mpd$a_nu <- extract_age_output(model, type = "resid")

  # Add sigma and tau
  sigtau <- calc_sig_tau(model$mpd$rho, model$mpd$vartheta)
  model$mpd$tau <- sigtau[[1]]
  model$mpd$sigma <- sigtau[[2]]

  # Some of the parameters need to be logged
  model$mpd <- calc_mpd_logs(model$mpd)

  # Some parameters need to be split by gears into a list of vectors
  vbt_mpd <- as.data.frame(model$mpd$vbt)
  names(vbt_mpd) <- c("gear", "group", "year", "biomass")
  # Split data frame into list of biomass vectors, one for each gear
  vbt_mpd <- split(vbt_mpd, vbt_mpd$gear)
  model$vbt$mpd$vbt <- map(seq_along(vbt_mpd), ~{
    vbt_mpd[[.x]]$biomass
  })

  # Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  # Set the mcmc path. This doesn't mean it exists.
  model$mcmcpath <- file.path(model_dir, mcmc_subdir)

  # If the directory 'mcmc' exists, load the mcmc output
  if(dir.exists(model$mcmcpath)){
    message("MCMC output found in ", model$mcmcpath, ". Loading...")
    model$mcmc <- read_mcmc(model, ...)
    model$mcmccalcs <- calc_mcmc(model, ...)
    model$mcmc$params <- simplify_names(model$mcmc$params)
    model$mcmc$params <- fix_m(model$mcmc$params)
    model$mcmc$params_est <- get_estimated_params(model$mcmc$params)
    model$mcmc$params_est_log <- calc_logs(model$mcmc$params_est)
  }
  class(model) <- mdl_cls
  model
}
