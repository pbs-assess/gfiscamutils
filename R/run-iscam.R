#' Run an iscam model through the [base::shell()] command
#'
#' @param model_dir The directory in which the iscam model lies
#' @param ... Passed on to other functions
#' @param mcmc_mode Logical. TRUE means enter the `mcmc_dir` and run an
#' MCMC with the remaining arguments
#' @param mcmc_dir Directory to run the MCMC in
#' @param mcmc_chain_length MCMC chain length
#' @param mcsave MCMC save every `mcsave`th posterior
#' @param mcscale Scale step size for the first `mcscale` evaluations
#' @param maxfn Maximum number of function evaluations
#' @param crit Gradient convergence criterion
#' @param nox Logical. If TRUE, do not show gradient info to the screen
#'
#' @return [base::invisible()]
#' @export
run_iscam <- function(model_dir,
                      mcmc_mode = FALSE,
                      mcmc_dir = "mcmc",
                      mcmc_chain_length = 500,
                      mcsave = 1,
                      mcscale = 0,
                      maxfn = 2000,
                      crit = 0.0001,
                      nox = TRUE,
                      ...){
  if(mcmc_mode){
    model_dir <- file.path(model_dir, mcmc_dir)
    cmd <- paste0("cd ", model_dir, " & ",
                  iscam.exe.file,
                  " -mcmc ", mcmc_chain_length,
                  " -mcsave ", mcsave,
                  " -mcscale ", mcscale,
                  " -maxfn ", maxfn,
                  " -crit ", format(crit, scientific = FALSE),
                  ifelse(nox, " -nox", ""))
    shell(cmd, intern = TRUE, wait = FALSE)
    cmd <- paste0("cd ", model_dir, " & ",
                  iscam.exe.file,
                  " -mceval")
    shell(cmd, intern = TRUE, wait = FALSE)
  }else{
    cmd <- paste0("cd ", model_dir, " & ", iscam.exe.file,
                  " -maxfn ", maxfn,
                  " -crit ", format(crit, scientific = FALSE),
                  ifelse(nox, " -nox", ""))
    shell(cmd, intern = TRUE, wait = FALSE)
  }
  invisible()
}

#' Run multiple iscam models in parallel
#'
#' @param model_dirs A vector of model directories
#' @param ... Arguments passed to [run_iscam()]
#'
#' @return Nothing
#' @importFrom furrr future_map future_options
#' @importFrom future plan
#' @export
run_multiple_iscam <- function(model_dirs, ...){
  plan("multisession")
  future_map(model_dirs, ~run_iscam(...),
  .options = future_options(packages = c("gfiscamutils")),
  ...)
  plan()
}
