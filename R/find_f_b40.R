#' Find the Reference Removal Rate (RRR) `bo_refpt` F_b0 which is the F value
#' which  will bring the Spawning biomass to `bo_refpt` B0 in one year within
#' the tolerance of `tol` thousand tonnes.
#'
#' @details
#' This is performed by an automated search for the catch level required to
#' get the F value for the removal rate reference point. The initial catch is
#' provided and should be based on the value in the decision table which
#' is close to 0.5 probability that the biomass next year will drop
#' below `bo_refpt` of B0. The initial value is placed as a catch TAC value
#' in the projection file and the model mceval is run, then the biomass is
#' compared with the 0.4B0 value and the tolerance is checked. If within the
#' tolerance, `tol`, the F value is extracted and that is the RRR. If it is
#' not within tolerance `tol`, the new catch becomes the current evaluated
#' catch plus the difference between the estimated biomass and the B0
#' reference point biomass. This routine is continued until within the
#' tolerance `tol` or the maximum number of iterations has taken place
#' (`max_iter`).
#'
#' The `bash` shell is used to run the ISCAM models. You must have `bash`
#' available on your system. For Windows, you need to install WSL2
#' (Windows Subsystem for Linux) and install ISCAM on it to use this.
#'
#' Install ISCAM: navigate to /usr/bin and clone the gfiscam GitHub
#' repository. Type `make -j 8 dist`. It should be at:
#' `/usr/bin/gfiscam/build/dist/bin/iscam`. This full path is necessary
#' because the `$PATH` is not transferred through the `system()` call.
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param bo_refpt A fractional value of B0 to use as the reference point to
#' approach in the calculation
#' @param init_catch Initial catch value to run the model with
#' @param root_dir The root hard drive name, must be either 'c', 'C',
#' 'd', or 'D'
#' @param tol The catch tolerance in thousands of tonnes, default 0.5
#' @param max_iter The maximum number of iterations to perform. If this is
#' hit, a warning will be issued and the F value returned will be NA
#' @param burnin The number of posterior samples to remove from the beginning
#' of the list of posteriors
#' @param proj_yrs The number of years to project
#'
#' @return A list of two items, the F and U values. Each of these are the length
#' of the number of fleets
#' @export
find_f_b40 <- function(model,
                       bo_refpt = 0.4,
                       init_catch = 1,
                       root_dir = "d",
                       tol = 0.5,
                       max_iter = 10,
                       burnin = 1000,
                       proj_yrs = 50){

  curr_dir <- getwd()
  on.exit(setwd(curr_dir))

  # Replace root dir with the one supplied. Works on both Linux and Windows
  # formats (c:/xxx or /mnt/c/xxx)
  sbo <- model$mcmc$params$sbo
  mcmc_path <- gsub("(/*)[C|c|D|d]([/|:])", paste0("\\1", root_dir, "\\2"), model$mcmcpath)

  # Change dirs from linux to windows if necessary
  os <- get_os()

  if(os == "windows"){
    if(grepl("^/mnt", mcmc_path)){
      pth <- gsub("^/mnt/", "", mcmc_path)
      pth <- gsub("^([C|c|D|d])/", "\\1:/", pth)
    }
  }else if(os == "linux" || os == "osx"){
    if(grepl("^[C|c|D|d]:", mcmc_path)){
      pth <- gsub("^([C|c|D|d]):", "/mnt/\\1", .x)
    }
  }else{
    stop("Not implemented for operating system `", os, "`", call. = FALSE)
  }

  fns <- dir(pth, full.names = TRUE)
  # Remove the csv files as they will be produced by the iscam -mceval calls below
  fns <- fns[-grep("csv$", fns)]

  tmp_dir <- tempdir()
  file.copy(fns, tmp_dir)

  # Set up initial PFC file value before binary catch reduction algorithm
  # which will narrow in on F_Bo_40% removal rate
  pfc <- model$proj
  pfc$num.projyrs <- proj_yrs
  pfc$num.tac <- 1
  pfc$tac.vec <- init_catch
  proj_file <- file.path(tmp_dir, basename(model$proj.file))
  write_projection_file(proj_file, proj_lst = pfc, overwrite = TRUE)
  bash_lines <- c("#!/bin/bash",
                  "`which iscam` -mceval")
  writeLines(bash_lines, file.path(tmp_dir, "iscam_eval.sh"))
  setwd(tmp_dir)
  system("chmod +x iscam_eval.sh", intern = TRUE)
  system("sed -i -e 's/\r$//' iscam_eval.sh", intern = TRUE)
  iter <- 0
  curr_catch <- init_catch
  repeat{
    message("Iteration ", iter + 1, " of F_B0_", bo_refpt, " search, catch set to ", curr_catch)
    command <- "bash iscam_eval.sh"
    system(command, intern = TRUE)
    # Read and burnin
    proj <- read.csv(file.path(tmp_dir, mcmc.proj.file))
    proj <- proj |> slice((burnin + 1):nrow(proj))
    unlink(mcmc.proj.file, force = TRUE)

    col_in_proj <- paste0("B", base_model$dat$end.yr + 50)
    diff_from_bo_refpt <- median(as.numeric(pull(proj, col_in_proj))) -
      bo_refpt * median(as.numeric(sbo))
    if(iter >= max_iter){
      warning("Maximum number of iterations reached (", max_iter, "), ",
              "Difference between median of column `", col_in_proj, "` and ",
              bo_refpt, "B0 is: ", diff_from_bo_refpt)
      break;
    }
    if(abs(diff_from_bo_refpt) <= tol){
      message("Found a catch where difference between median of column `",
              col_in_proj, "` and 0.4B0 (", diff_from_bo_refpt, ") is within ",
              "the tolerance of ", tol, ". Model F value for that year is the ",
              "F_B0_", bo_refpt, " removal rate, or the F required to get to ",
              bo_refpt, "B0 in one year.")
      break;
    }
    message("The difference between proj biomass and ", bo_refpt, "B0 is ", diff_from_bo_refpt)
    # Make catch larger or smaller to zone in to the 0.4B0 value
    pfc$tac.vec <- curr_catch <- curr_catch + diff_from_bo_refpt
    write_projection_file(proj_file, proj_lst = pfc, overwrite = TRUE)
    iter <- iter + 1
  }
  f_output <- read.csv(file.path(tmp_dir, mcmc.fishing.mort.file)) |>
    as_tibble()
  f_out <- f_output |>
    slice((burnin + 1):nrow(f_output)) |>
    select(grep(model$dat$end.yr, names(f_output))) |>
    apply(MARGIN = 2, median)
  out <- list()
  out$f <- f_out
  out$u <- 1.0 - exp(-f_out)
  out$catch <- curr_catch
  names(out$f) <- map_chr(seq_along(out$f), ~{paste0("f_fleet", .x)})
  names(out$u) <- map_chr(seq_along(out$u), ~{paste0("u_fleet", .x)})

  out
}
