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
#' @export
load_iscam_files <- function(model_dir,
                             mcmc_subdir = "mcmc",
                             ...){

  model <- list()
  model$path <- model_dir
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

#' Perform some quantile calculations on the MCMC posteriors
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param burnin The number of MCMC records to remove for burnin period
#' for projections if `load_proj == TRUE`
#' @param thin Remove every nth record for thinning of MCMC output
#' for projections if `load_proj == TRUE`
#' @param probs The probabilities to use for `quantile()` calculations
#' @param load_proj Load the projections from the MCMC and do the calculations
#' @param index_scale Number to multiply the index values by so they match the
#' `survey_index` values
#' @param ... arguments to pass to [calc_proj_probs()] to construct the
#' decision tables
#'
#' @return A list of each parameter for which quantiles were calculated
#' @export
calc_mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      probs = c(0.025, 0.5, 0.975),
                      index_scale = 1e6,
                      load_proj = TRUE,
                      ...){

  if(is.null(model$mcmc)){
    stop("The mcmc list was null. Check read_mcmc() function.")
  }
  if(length(probs) != 3){
    stop("`probs` must be a vector of three numeric values")
  }
  if(class(probs) != "numeric"){
    stop("`probs` must be a numeric vector")
  }
  if(any(probs < 0 || probs > 1)){
    stop("`probs` values must all be between 0 and 1")
  }
  if(probs[1] >= probs[2] || probs[2] >= probs[3]){
    stop("`probs` values must be incresing and no two can be equal")
  }
  if(probs[2] != 0.5){
    warning("The second value for `probs` is not 0.5 (the median)")
  }

  # Create output list
  out <- list()

  # Parameters
  mc <- model$mcmc
  mpd <- model$mpd
  out$params <- mc$params
  if(is.null(out$params)){
    stop("There is no MCMC output for the model in ", model$mcmcpath)
  }
  out$params <- strip_static_params(model, out$params)
  nm <- names(out$params)
  # Remove non-parameters
  out$params <- out$params[, -grep("fmsy|umsy|msy_|SSB|f", nm)]
  # Calculate sigma and tau and add to p_dat
  if("rho" %in% names(out$params) && "vartheta" %in% names(out$params)){
    sigtau <- calc_sig_tau(out$params$rho, out$params$vartheta)
    out$params$tau <- sigtau[[1]]
    out$params$sigma <- sigtau[[2]]
  }

  out$params_log <- calc_logs(out$params)
  out$params_quants <- apply(out$params, 2, quantile, prob = probs)
  out$params_quants_log <- apply(out$params_quants, c(1, 2), log)

  quantify <- function(mc_d, mpd_d = NULL){
    # Apply the quantile function to all columns in a data frame and bind
    # the MPD value
    j <- apply(mc_d, 2, quantile, prob = probs)
    if(!is.null(mpd_d)){
      j <- rbind(j, mpd_d)
      rownames(j)[length(probs) + 1] <- "MPD"
    }
    j
  }
  # Spawning biomass
  out$sbt <- mc$sbt
  out$sbt_quants <- quantify(out$sbt, mpd$sbt)

  # Depletion
  out$depl <- apply(out$sbt, 2, function(x){x / out$params$sbo})
  out$depl_quants <- quantify(out$depl, mpd$sbt / mpd$sbo)

  # Natural mortality (possible list)
  out$m_female <- mc$m[[1]]
  out$m_female_quants <- apply(out$m_female, 2, quantile, prob = probs)
  out$m_female_quants <- rbind(out$m_female_quants, mpd$m[1])
  rownames(out$m_female_quants)[4] <- "MPD"
  if(length(mc$m) == 2){
    out$m_male <- mc$m[[2]]
    out$m_male_quants <- apply(out$m_male, 2, quantile, prob = probs)
    out$m_male_quants <- rbind(out$m_male_quants, mpd$m[2])
    rownames(out$m_male_quants)[4] <- "MPD"
  }

  # Recruitment
  out$rt <- mc$rt
  out$rt_quants <- quantify(out$rt, mpd$rt)

  ## Recruitment deviations
  out$rdev <- mc$rdev
  out$rdev_quants <- apply(out$rdev, 2, quantile, prob = probs)

  ## Q for the survey indices
  out$q <- out$params %>% select(matches("^q_gear[[:digit:]]+$"))
  num_inds <- ncol(out$q)
  if(num_inds != length(model$dat$index_abbrevs)){
    stop("The number of `q` outputs from the model does not match the names ",
         "entered in the data file. See model$dat$index_abbrevs")
  }
  colnames(out$q) <- model$dat$index_abbrevs
  out$q_quants <- quantify(out$q, exp(model$mpd$log_q))

  build_quant_list <- function(mc_dat, mpd_dat){
    # Run quantiles on each data frame in a list of dataframes and append
    #  the MPD values as well. Returns a list of dataframes
    mc_dat_q <- imap(mc_dat, ~{
      # Remove columns with all NAs
      .x <- .x[, colSums(is.na(.x)) != nrow(.x)]
      q <- apply(.x, 2, quantile, prob = probs, na.rm = TRUE)
      # Suppress warning that the MPD is longer than the MCMC, this happens when
      # there are NAs in the final years in the MCMC
      suppressWarnings(q <- rbind(q, mpd_dat[.y, ]))
      rownames(q)[length(probs) + 1] <- "MPD"
      q
    })
  }

  # Reshape MPD vulnerable biomass (list of data frames by gear)
  # vbt output is special for some reason
  vbt_mpd <- as.data.frame(mpd$vbt)
  names(vbt_mpd) <- c("gear", "group", "year", "biomass")
  # Split data frame into list of data frames, one for each gear
  vbt_mpd <- split(vbt_mpd, vbt_mpd$gear)
  vbt_mpd <- map(vbt_mpd, ~{
    .x$biomass
  })
  vbt_mpd <- do.call(rbind, vbt_mpd)
  out$vbt <- mc$vbt
  out$vbt_quants <- build_quant_list(mc$vbt, vbt_mpd)

  # Fishing mortalities by gear (list of data frames)
  out$ft <- mc$ft
  out$ft_quants <- build_quant_list(mc$ft, mpd$ft)
  out$ut <- mc$ut
  out$ut_quants <- build_quant_list(mc$ut, mpd$ut)

  # Index fits
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  if(!is.null(mc$it)){
    out$it_quants <- imap(mc$it, ~{
      imap(.x, ~{
        quantile(.x * index_scale, probs = probs, na.rm = TRUE) %>%
          t() %>%
          as.data.frame() %>%
          as_tibble() %>%
          mutate(year = .y)
      }) %>%
        bind_rows() %>%
        mutate(survey_abbrev = .y)
    }) %>%
      bind_rows() %>%
      rename(lowerci = !!sym(prob_cols[1]),
             biomass = !!sym(prob_cols[2]),
             upperci = !!sym(prob_cols[3])) %>%
      select(survey_abbrev, year, biomass, lowerci, upperci) %>%
      mutate(year = as.numeric(year))
  }
  # Index residuals
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  if(!is.null(mc$epsilon)){
    names(mc$epsilon) <- names(mc$it)
    out$epsilon_quants <- imap(mc$epsilon, ~{
      imap(.x, ~{
        quantile(.x, probs = probs, na.rm = TRUE) %>%
          t() %>%
          as.data.frame() %>%
          as_tibble() %>%
          mutate(year = .y)
      }) %>%
        bind_rows() %>%
        mutate(survey_abbrev = .y)
    }) %>%
      bind_rows() %>%
      rename(lowerci = !!sym(prob_cols[1]),
             biomass = !!sym(prob_cols[2]),
             upperci = !!sym(prob_cols[3])) %>%
      select(survey_abbrev, year, biomass, lowerci, upperci) %>%
      mutate(year = as.numeric(year))
  }

  if(!is.null(mc$agefits)){
    out$agefit_quants <- calc_age_quants(mc$agefits, probs)
  }
  if(!is.null(mc$ageresids)){
    out$ageresids_quants <- calc_age_quants(mc$ageresids, probs)
  }

  if(load_proj){
    # Burn in and calculate quantiles for each TAC level
    out$proj <- mc$proj %>%
      split(~TAC) %>%
      map(~{mcmc_thin(.x, burnin = burnin, thin = thin)})
    out$proj_quants <- out$proj %>%
      map(quantify)

    # Replace the final year of the sbt with the values obtained from the TAC 0 projection
    out$sbt[, ncol(out$sbt)] <- out$proj$`0`[, 2]
    out$sbt_quants <- quantify(out$sbt, mpd$sbt)
  }

  out
}

#' Extract and calculate probabilities from the iscam projection model
#'
#' @param model An iscam model object
#' @param burnin The number of samples to burn away from the beginning of the MCMC
#' @param thin The thinning to apply to the MCMC posterior samples
#' @param fixed_cutoffs A vector of catch cutoffs to use in decision tables
#' @param ... Extra arguments
#'
#' @details Extract and calculate probabilities from the projection model.
#'   Used for decision tables in the document (see make.decision.table())
#'   in tables-decisions.r
#'
#' @return A data frame which has its names formatted for latex
#' @export
calc_proj_probs <- function(model,
                            burnin,
                            thin,
                            fixed_cutoffs,
                            ... ){

  mc <- model$mcmc
  proj <- mc$proj
  tac <- sort(unique(proj$TAC))
  p <- model$proj$ctl.options
  s_yr <- p[rownames(p) == "syrmeanm", 1]
  e_yr <- p[rownames(p) == "nyrmeanm", 1] + 2
  e_yr_1 <- e_yr - 1
  e_yr_2 <- e_yr - 2
  fc <- fixed_cutoffs
  proj_dat <- data.frame()
  for(t in 1:length(tac)){
    d <- proj[proj$TAC == tac[t], ]
    d <- mcmc_thin(d, burnin, thin)
    n.row <- nrow(d)
    k <- c(tac[t] * 1000,
           length(which(d[,paste0("B", e_yr_1)] < d$X03B0)) / n.row,
           median(d[,paste0("B", e_yr_1)] / d$X03B0),
           length(which(d[,paste0("B", e_yr_1)] < (0.6 * d$B0))) / n.row)
    #length(which(d[,paste0("B", e_yr_1)] < d$X09B0)) / n.row,
    #median(d[,paste0("B", e_yr_1)] / d$X09B0))

    if(t == 1){
      col_names <- c(latex.mlc(c(e_yr_1,
                                 "TAC (t)")),
                     latex.mlc(c(paste0("P(SB_{",
                                        e_yr_1,
                                        "}<"),
                                 "0.3SB_0)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("Med(SB_{",
                                        e_yr_1,
                                        "}/"),
                                 "0.3SB_0)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("P(SB_{",
                                        e_yr_1,
                                        "}<"),
                                 "0.6SB_0)"),
                               math.bold = TRUE))
    }
    if(which.model == 2){
      k <- c(k,
             length(which(d[,paste0("B", e_yr_1)] < fc[which.stock])) / n.row,
             median(d[,paste0("B", e_yr_1)] / fc[which.stock]))
      if(t == 1){
        col_names <- c(col_names,
                       latex.mlc(c(paste0("P(SB_{",
                                          e_yr_1,
                                          "} <"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE),
                       latex.mlc(c(paste0("Med(SB_{",
                                          e_yr_1,
                                          "} /"),
                                   paste0(f(fc[which.stock] * 1000),
                                          "~t)")),
                                 math.bold = TRUE))
      }
    }

    k <- c(k,
           length(which(d$UT > 0.2)) / n.row,
           length(which(d$UT > 0.1)) / n.row,
           # length(which(d$UT > 0.05)) / n.row,
           # length(which(d$UT > 0.03)) / n.row,
           # length(which(d$UT > 0.07)) / n.row,
           #length(which(d$UT > 0.08)) / n.row,
           # length(which(d$UT > 0.09)) / n.row,
           median(d$UT))
    if(t == 1){
      col_names <- c(col_names,
                     latex.mlc(c(paste0("P(U_{",
                                        e_yr_1,
                                        "}>"),
                                 "20\\%)"),
                               math.bold = TRUE),
                     latex.mlc(c(paste0("P(U_{",
                                        e_yr_1,
                                        "}>"),
                                 "10\\%)"),
                               math.bold = TRUE),
                     #  latex.mlc(c(paste0("P(U_{",
                     #                   e_yr_1,
                     #                  "}>"),
                     #         "5\\%)"),
                     #   math.bold = TRUE),
                     # latex.mlc(c(paste0("P(U_{",
                     #                    e_yr_1,
                     #                  "}>"),
                     #        "3\\%)"),
                     #  math.bold = TRUE),
                     #latex.mlc(c(paste0("P(U_{",
                     #                e_yr_1,
                     #              "}>"),
                     #   "7\\%)"),
                     # math.bold = TRUE),
                     # latex.mlc(c(paste0("P(U_{",
                     #                 e_yr_1,
                     #                "}>"),
                     #        "8\\%)"),
                     #     math.bold = TRUE),
                     #latex.mlc(c(paste0("P(U_{",
                     #        e_yr_1,
                     #          "}>"),
                     #  "9\\%)"),
                     #  math.bold = TRUE),
                     latex.math.bold(paste0("Med(U_{",
                                            e_yr_1,
                                            "})")))
    }
    proj_dat <- rbind(proj_dat, k)
  }
  colnames(proj_dat) <- col_names

  proj_dat
}

#' Delete all files with a specific extension recursively. By default, .rds files
#'
#' @param path The directory to start in.
#' @param ext The extension of files to include for deletion
#' @export
#' @examples
#' \dontrun{
#' delete_files_ext(file.path(drs$models_dir, "001-bridge-models"))
#' }
delete_files_ext <- function(path = NULL, ext = "rds"){
  stopifnot(!is.null(path))
  ext_full <- paste0(".", ext, "$")
  ans <- readline(paste0("Warning - you are about to delete all ", ext, " files recursively in the\n",
                         path, "\n",
                         "directory. You cannot undo this operation. Are you sure? (y/n) <ENTER> "))
  if(ans == "n"){
    message("No files were deleted.")
    return(invisible())
  }
  dirs <- file.path(list.dirs(path, recursive = TRUE))
  map(dirs, ~{
    unlink(file.path(.x, grep(ext_full, dir(.x), value = TRUE)))
  })
  message("Deleted all ", ext, " files.")
  invisible()
}

#' Read the iscam starter file to get the iscam input file names
#'
#' @param path Full path to the file
#' @param filename Filename
#'
#' @return A list with three names:
#'   1. Data file name
#'   2. Control file name
#'   3. Projection file name
#'
#' @export
fetch_file_names <- function(path, filename){

  fn <- file.path(path, filename)
  if(!file.exists(fn)){
    stop("The file ", fn, " does not exist. It must be present and it must have three lines: ",
         " data file name, control file name, and projection file name.", call. = FALSE)
  }
  d <- readLines(fn, warn = FALSE)
  ## Remove comments
  d <- gsub("#.*", "", d)
  ## Remove trailing whitespace
  d <- gsub(" +$", "", d)
  list(file.path(path, d[1]),
       file.path(path, d[2]),
       file.path(path, d[3]))
}

#' Read in the iscam report (.rep) file
#'
#' @param fn Filename
#'
#' @return A list representing everything in the report file
#' @export
#'
#' @details Read in the iscam report (.rep) file:
#'   File structure:
#'   It is assumed that each text label will be on its own line,
#'   followed by one or more lines of data.
#'   If the label is followed by a single value or line of data,
#'   a vector will be created to hold the data.
#'   If the label is followed by multiple lines of data,
#'   a matrix will be created to hold the data. The matrix might be
#'   ragged so a check is done ahead of time to ensure correct
#'   matrix dimensions.
#'
#'   If a label has another label following it but no data,
#'   that label is thrown away and not included in the returned list.
#'
#'   A label must start with an alphabetic character followed by
#'   any number of alphanumeric characters (includes underscore and .)
read_report_file <- function(fn){

  if(!file.exists(fn)){
    warning("Report file ", basename(fn)," not found in ", dirname(fn), ". Setting MPD output to NA.")
    return(NA)
  }

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  # A vector of the object names
  objs  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat, value = TRUE)
  nobj <- length(objs)
  ret  <- list()
  indname <- 0

  for(obj in seq_len(nobj)){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat, "[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(x){c(x, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  ret
}

#' Read in the iscam data file
#'
#' @param file Filename (full path)
#' @param verbose Say more
#'
#' @return A list representing the contents of the iscam data file
#' @importFrom stringr str_split
#' @export
read_data_file <- function(file = NULL,
                           verbose = FALSE){

  if(!file.exists(file)){
    warning("Data file ", basename(file)," not found in ", dirname(file), ". Is your ",
            iscam.starter.file, " set up correctly? Setting data to NA.")
    return(NA)
  }
  data <- readLines(file, warn=FALSE)
  tmp <- list()
  ind <- 0

  # Remove any empty lines
  data <- data[data != ""]

  # remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  # Get the element number for the "Gears" names if present
  dat <- grep("^#.*Gears:.+", data)
  tmp$has_gear_names <- FALSE
  if(length(dat > 0)){
    gear_names_str <- gsub("^#.*Gears:(.+)", "\\1", data[dat])
    gear_names <- strsplit(gear_names_str, ",")[[1]]
    tmp$gear_names <- gsub("^[[:blank:]]+", "", gear_names)
    tmp$has_gear_names <- TRUE
  }

  # Get the element number for the "GearAbbrevs" names if present
  dat <- grep("^#.*GearAbbrevs:.+", data)
  tmp$has_gear_abbrevs <- FALSE
  if(length(dat > 0)){
    gear_abbrevs_str <- gsub("^#.*GearAbbrevs:(.+)", "\\1", data[dat])
    gear_abbrevs <- strsplit(gear_abbrevs_str, ",")[[1]]
    tmp$gear_abbrevs <- gsub("^[[:blank:]]+", "", gear_abbrevs)
    tmp$has_gear_abbrevs <- TRUE
  }

  # Get the element number for the "FleetGears" names if present
  dat <- grep("^#.*FleetGears:.+",data)
  tmp$has_fleet_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    fleet_gear_names_str <- gsub("^#.*FleetGears:(.+)", "\\1", data[dat])
    fleet_gear_names <- strsplit(fleet_gear_names_str, ",")[[1]]
    tmp$fleet_gear_names <- gsub("^[[:blank:]]+", "", fleet_gear_names)
    tmp$has_fleet_gear_names <- TRUE
  }

  # Get the element number for the "FleetAbbrevs" names if present
  dat <- grep("^#.*FleetAbbrevs:.+", data)
  tmp$has_fleet_abbrevs <- FALSE
  if(length(dat > 0)){
    # The gear abbreviations were in the file. These are the survey_abbrev column in the survey index data
    fleet_abbrevs_str <- gsub("^#.*FleetAbbrevs:(.+)", "\\1", data[dat])
    fleet_abbrevs <- strsplit(fleet_abbrevs_str, ",")[[1]]
    tmp$fleet_abbrevs <- gsub("^[[:blank:]]+", "", fleet_abbrevs)
    tmp$has_fleet_abbrevs <- TRUE
  }

  # Get the element number for the "IndexGears" names if present
  dat <- grep("^#.*IndexGears:.+",data)
  tmp$has_index_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    index_gear_names_str <- gsub("^#.*IndexGears:(.+)", "\\1", data[dat])
    index_gear_names <- strsplit(index_gear_names_str, ",")[[1]]
    tmp$index_gear_names <- gsub("^[[:blank:]]+", "", index_gear_names)
    tmp$has_index_gear_names <- TRUE
  }

  # Get the element number for the "IndexAbbrevs" names if present
  dat <- grep("^#.*IndexAbbrevs:.+", data)
  tmp$has_index_abbrevs <- FALSE
  if(length(dat > 0)){
    # The gear abbreviations were in the file. These are the survey_abbrev column in the survey index data
    index_abbrevs_str <- gsub("^#.*IndexAbbrevs:(.+)", "\\1", data[dat])
    index_abbrevs <- strsplit(index_abbrevs_str, ",")[[1]]
    tmp$index_abbrevs <- gsub("^[[:blank:]]+", "", index_abbrevs)
    tmp$has_index_abbrevs <- TRUE
  }

  # Get the element number for the "AgeGears" names if present (gears with age comp data)
  dat <- grep("^#.*AgeGears:.+",data)
  tmp$has_age_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    age_gear_names_str <- gsub("^#.*AgeGears:(.+)", "\\1", data[dat])
    age_gear_names <- strsplit(age_gear_names_str, ",")[[1]]
    tmp$age_gear_names <- gsub("^[[:blank:]]+", "", age_gear_names)
    tmp$has_age_gear_names <- TRUE
  }

  # Get the element number for the "AgeAbbrevs" names if present (gears with age comp data)
  dat <- grep("^#.*AgeAbbrevs:.+",data)
  tmp$has_age_gear_names <- FALSE
  if(length(dat >0)){
    # The gear names were in the file
    age_gear_abbrevs_str <- gsub("^#.*AgeAbbrevs:(.+)", "\\1", data[dat])
    age_gear_abbrevs <- strsplit(age_gear_abbrevs_str, ",")[[1]]
    tmp$age_gear_abbrevs <- gsub("^[[:blank:]]+", "", age_gear_abbrevs)
    tmp$has_age_gear_abbrevs <- TRUE
  }

  ## Get the element number for the "CatchUnits" if present
  dat <- grep("^#.*CatchUnits:.+", data)
  if(length(dat > 0)){
    catch.units.str <- gsub("^#.*CatchUnits:(.+)", "\\1", data[dat])
    tmp$catch.units <- gsub("^[[:blank:]]+", "", catch.units.str)
  }

  ## Get the element number for the "IndexUnits" if present
  dat <- grep("^#.*IndexUnits:.+", data)
  if(length(dat > 0)){
    index.units.str <- gsub("^#.*IndexUnits:(.+)", "\\1", data[dat])
    tmp$index.units <- gsub("^[[:blank:]]+", "", index.units.str)
  }

  ## Save the number of specimens per year (comment at end of each age comp
  ##  line), eg. #135 means 135 specimens contributed to the age proportions for
  ##  that year
  age.n <- vector()
  ## Match age comp lines which have N's as comments
  tmp$has.age.comp.n <- FALSE
  pattern <- "^[[:digit:]]{4}[[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]][[:space:]]+[[:digit:]].*#([[:digit:]]+).*"
  dat <- data[grep(pattern, data)]
  if(length(dat) > 0){
    for(n in 1:length(dat)){
      age.n[n] <- sub(pattern, "\\1", dat[n])
    }
  }
  ## age.n is now a vector of values of N for the age comp data.
  ## The individual gears have not yet been parsed out, this will
  ##  happen later when the age comps are read in.

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the DAT file changes format
  tmp$num.areas <- as.numeric(dat[ind <- ind + 1])
  tmp$num.groups <- as.numeric(dat[ind <- ind + 1])
  tmp$num.sex <- as.numeric(dat[ind <- ind + 1])
  tmp$start.yr <- as.numeric(dat[ind <- ind + 1])
  tmp$end.yr <- as.numeric(dat[ind <- ind + 1])
  tmp$start.age <- as.numeric(dat[ind <- ind + 1])
  tmp$end.age <- as.numeric(dat[ind <- ind + 1])
  tmp$num.gears <- as.numeric(dat[ind <- ind + 1])
  tmp$prop.female <- as.numeric(dat[ind <- ind + 1])

  ## Gear allocation
  tmp$gear.alloc  <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  if(!tmp$has_gear_names){
    tmp$gear_names <- 1:length(tmp$gear.alloc)
  }

  ## Age-schedule and population parameters
  tmp$linf      <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$k         <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$to        <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$lw.alpha  <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$lw.beta   <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$age.at.50.mat <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$sd.at.50.mat  <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  tmp$use.mat   <- as.numeric(dat[ind <- ind + 1])
  tmp$mat.vec   <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+|,")[[1]])

  ## Catch data
  tmp$num.catch.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$catch         <- matrix(NA, nrow = tmp$num.catch.obs, ncol = 7)

  for(row in 1:tmp$num.catch.obs){
    tmp$catch[row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  }
  colnames(tmp$catch) <- c("year", "gear", "area", "group", "sex", "type", "value")

  ## Abundance indices are a ragged object and are stored as a list of matrices
  tmp$num.indices     <- as.numeric(dat[ind <- ind + 1])
  tmp$num.index.obs   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$survey.type <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
  ##nrows <- sum(tmp$nitnobs)
  tmp$indices <- list()
  for(index in 1:tmp$num.indices){
    nrows <- tmp$num.index.obs[index]
    ncols <- 8
    tmp$indices[[index]] <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$indices[[index]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$indices[[index]]) <- c("iyr","it","gear","area","group","sex","wt","timing")
  }
  ## Age composition data are a ragged object and are stored as a list of matrices
  tmp$num.age.gears <- as.numeric(dat[ind <- ind + 1])
  ##if(!tmp$hasAgeGearNames){
  ##  tmp$ageGearNames <- 1:length(tmp$nagears)
  ##}

  tmp$num.age.gears.vec       <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.start.age <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$num.age.gears.end.age   <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$eff                     <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$age.comp.flag           <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  tmp$dm_num_samp             <- as.numeric(dat[ind <- ind + 1])
  tmp$dm_use_single_num_samp  <- as.logical(dat[ind <- ind + 1])
  tmp$age.comps <- NULL
  ## One list element for each gear (tmp$nagears)
  ## Check to see if there are age comp data
  if(tmp$num.age.gears.vec[1] > 0){
   tmp$age.comps <- list()
   for(gear in 1:tmp$num.age.gears){
     nrows <- tmp$num.age.gears.vec[gear]
     ## 5 of the 6 here is for the header columns
     ncols <- tmp$num.age.gears.end.age[gear] - tmp$num.age.gears.start.age[gear] + 7
     tmp$age.comps[[gear]] <- matrix(NA, nrow = nrows, ncol = ncols)

     for(row in 1:nrows){
       tmp$age.comps[[gear]][row,] <- as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
     }
     colnames(tmp$age.comps[[gear]]) <- c("year",
                                          "sample_size",
                                          "gear",
                                          "area",
                                          "group",
                                          "sex",
                                          tmp$num.age.gears.start.age[gear]:tmp$num.age.gears.end.age[gear])
   }
  }
  ## Build a list of age comp gear N's
  tmp$age.gears.n <- list()
  start <- 1
  for(ng in 1:length(tmp$num.age.gears.vec)){
    end <- start + tmp$num.age.gears.vec[ng] - 1
    tmp$age.gears.n[[ng]] <- age.n[start:end]
    start <- end + 1
  }
  ## Empirical weight-at-age data
  tmp$num.weight.tab <- as.numeric(dat[ind <- ind + 1])
  tmp$num.weight.obs <- as.numeric(dat[ind <- ind + 1])
  tmp$waa <- NULL

  if(tmp$num.weight.obs > 0){
    ## Parse the weight-at-age data
    nrows       <- tmp$num.weight.obs
    ncols       <- tmp$end.age - tmp$start.age + 6
    tmp$weight.at.age <- matrix(NA, nrow = nrows, ncol = ncols)
    for(row in 1:nrows){
      tmp$weight.at.age[row,] <-
        as.numeric(strsplit(dat[ind <- ind + 1], "[[:blank:]]+")[[1]])
    }
    colnames(tmp$weight.at.age) <- c("year",
                                     "gear",
                                     "area",
                                     "group",
                                     "sex",
                                     tmp$start.age:tmp$end.age)
  }

  ## Annual Mean Weight data
  ## Catch data
  tmp$num.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.obs <- as.numeric(dat[ind <- ind + 1])
  if(tmp$num.mean.weight.obs >0){
    tmp$mean.weight.data  <- matrix(NA, nrow = sum(tmp$num.mean.weight.obs), ncol = 7)
    for(row in 1:sum(tmp$num.mean.weight.obs)){
      tmp$mean.weight.data[row,] <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    }
    colnames(tmp$mean.weight.data) <- c("year",
                                        "meanwt",
                                        "gear",
                                        "area",
                                        "group",
                                        "sex",
                                        "timing")
  }
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the iscam control file
#'
#' @param file Filename
#' @param num.gears The total number of gears in the datafile
#' @param num.age.gears The number of gears with age composition information
#'   in the datafile
#' @param verbose Say more
#'
#' @return A list representing the contents on the iscam control file
#' @export
read_control_file <- function(file = NULL,
                              num.gears = NULL,
                              num.age.gears = NULL,
                              verbose = FALSE){

  if(is.null(num.gears)){
    stop("You must supply the total number of gears (num.gears).")
  }
  if(is.null(num.age.gears)){
    stop("You must supply the number of gears with age composition (num.age.gears).")
  }

  if(!file.exists(file)){
    warning("Control file ", basename(file)," not found in ", dirname(file), ". Is your ",
            iscam.starter.file, " set up correctly? Setting data to NA.")
    return(NA)
  }
  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## Remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## Remove the lines that start with #.
  dat <- data[-dat]

  ## Save the parameter names, since they are comments and will be deleted in
  ##  subsequent steps.
  ## To get the npar, remove any comments and preceeding and trailing
  ##  whitespace for it.
  dat1 <- gsub("#.*", "", dat[1])
  dat1 <- gsub("^[[:blank:]]+", "", dat1)
  dat1 <- gsub("[[:blank:]]+$", "", dat1)
  n.par <- as.numeric(dat1)
  param.names <- vector()
  ## Lazy matching with # so that the first instance matches, not any other
  pattern <- "^.*?# *([[:alnum:]]+_*[[:alnum:]]*_*[[:alnum:]]*).*"
  for(param.name in 1:n.par){
    ## Each parameter line in dat which starts at index 2,
    ##  retrieve the parameter name for that line
    param.names[param.name] <- sub(pattern, "\\1", dat[param.name + 1])
  }
  ## Now that parameter names are stored, parse the file.
  ##  remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the CTL file and needs to
  ## be updated whenever the CTL file changes format.
  tmp <- list()
  ind <- 0
  tmp$num.params <- as.numeric(dat[ind <- ind + 1])
  tmp$params <- matrix(NA, nrow = tmp$num.params, ncol = 7)
  for(param in 1:tmp$num.params){
    tmp$params[param,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  colnames(tmp$params) <- c("ival","lb","ub","phz","prior","p1","p2")
  ## param.names is retreived at the beginning of this function
  rownames(tmp$params) <- param.names

  ## Age and size composition control parameters and likelihood types
  nrows <- 9
  ncols <- num.age.gears
  tmp$age.size <- matrix(NA, nrow = nrows, ncol = ncols)

  for(row in 1:nrows){
    tmp$age.size[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$age.size) <- c("gearind",
                              "likelihoodtype",
                              "minprop",
                              "comprenorm",
                              "logphiphase",
                              "logagetau2phase",
                              "phi1phase",
                              "phi2phase",
                              "degfreephase")

  ## Selectivity parameters for all gears
  nrows <- 12
  ncols <- num.gears
  tmp$sel <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$sel[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here
  rownames(tmp$sel) <- c("iseltype",
                         "agelen50log_f",
                         "std50log_f",
                         "agelen50log_m",
                         "std50log_m",
                         "nagenodes",
                         "nyearnodes",
                         "estphase",
                         "penwt2nddiff",
                         "penwtdome",
                         "penwttvs",
                         "nselblocks")

  ## Start year for time blocks, one for each gear
  max.block <- max(tmp$sel[12,])
  tmp$start.yr.time.block <- matrix(nrow = num.gears, ncol = max.block)
  for(ng in 1:num.gears){
    ## Pad the vector with NA's to make it the right size if it isn't
    ##  maxblocks size.
    tmp.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
    if(length(tmp.vec) < max.block){
      for(i in (length(tmp.vec) + 1):max.block){
        tmp.vec[i] <- NA
      }
    }
    tmp$start.yr.time.block[ng,] <- tmp.vec
  }

  ## Priors for survey Q, one column for each survey
  tmp$num.indices <- as.numeric(dat[ind <- ind + 1])
  nrows <- 3
  ncols <- tmp$num.indices
  tmp$surv.q <- matrix(NA, nrow = nrows, ncol = ncols)
  for(row in 1:nrows){
    tmp$surv.q[row,] <-
      as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$surv.q) <- c("priortype",
                            "priormeanlog",
                            "priorsd")

  ## Controls for fitting to mean weight data
  tmp$fit.mean.weight <- as.numeric(dat[ind <- ind + 1])
  tmp$num.mean.weight.cv <- as.numeric(dat[ind <- ind + 1])
  n.vals <- tmp$num.mean.weight.cv
  tmp$weight.sig <-  vector(length = n.vals)
  for(val in 1:n.vals){
    tmp$weight.sig[val] <- as.numeric(dat[ind <- ind + 1])
  }

  ## Miscellaneous controls
  n.rows <- 20
  tmp$misc <- matrix(NA, nrow = n.rows, ncol = 1)
  for(row in 1:n.rows){
    tmp$misc[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  for it here.
  rownames(tmp$misc) <- c("verbose",
                          "rectype",
                          "sdobscatchfirstphase",
                          "sdobscatchlastphase",
                          "unfishedfirstyear",
                          "maternaleffects",
                          "meanF",
                          "sdmeanFfirstphase",
                          "sdmeanFlastphase",
                          "mdevphase",
                          "sdmdev",
                          "mnumestnodes",
                          "fracZpriorspawn",
                          "agecompliketype",
                          "IFDdist",
                          "fitToMeanWeight",
                          "calculateMSY",
                          "runSlowMSY",
                          "slowMSYPrecision",
                          "slowMSYMaxF")
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the contents of the iscam projection file
#'
#' @param file Filename
#'
#' @return A list representing the contents of the iscam projection file
#' @export
read_projection_file <- function(file = NULL){

  if(!file.exists(file)){
    warning("Projection file ", basename(file)," not found in ", dirname(file), ". Is your ",
            iscam.starter.file, " set up correctly? Setting data to NA.")
    return(NA)
  }

  data <- readLines(file, warn = FALSE)

  ## Remove any empty lines
  data <- data[data != ""]

  ## remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  ## Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  ## remove the lines that start with #.
  dat <- data[-dat]

  ## remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  ## remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  ## Now we have a nice bunch of string elements which are the inputs for iscam.
  ## Here we parse them into a list structure.
  ## This is dependent on the current format of the DAT file and needs to
  ##  be updated whenever the proj file changes format.
  tmp <- list()
  ind <- 0

  ## Get the TAC values
  tmp$num.tac  <- as.numeric(dat[ind <- ind + 1])
  for(tac in 1:tmp$num.tac){
    ## Read in the tacs, one, per line
    tmp$tac.vec[tac] <- as.numeric(dat[ind <- ind + 1])
  }

  ## If the tac vector is on one line
  ##tmp$tac.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  ## Get the control options vector
  tmp$num.ctl.options <- as.numeric(dat[ind <- ind + 1])
  n.rows <- tmp$num.ctl.options
  n.cols <- 1
  tmp$ctl.options  <- matrix (NA, nrow = n.rows, ncol = n.cols)
  for(row in 1:n.rows){
    tmp$ctl.options[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  ## Rownames here are hardwired, so if you add a new row you must add a name
  ##  or it here.
  option.names <- c("syrmeanm",
                    "nyrmeanm",
                    "syrmeanfecwtageproj",
                    "nyrmeanfecwtageproj",
                    "syrmeanrecproj",
                    "nyrmeanrecproj",
                    "shortcntrlpts",
                    "longcntrlpts",
                    "bmin")
  rownames(tmp$ctl.options) <- option.names[1:tmp$num.ctl.options]
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}

#' Read in the contents of the iscam par file
#'
#' @param file Filename
#'
#' @return A list representing the contents of the iscam par file
#' @export
read_par_file <- function(file = NULL){

  if(!file.exists(file)){
    warning("Par file ", basename(file)," not found in ", dirname(file), ". Setting data to NA.")
    return(NA)
  }

  data <- readLines(file, warn = FALSE)
  tmp <- list()

  conv_check <- as.numeric(unlist(regmatches(data[1], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", data[1]))))

  # Remove the first line from the par data since we already parsed it and saved the values
  data <- data[-1]

  # Commented lines signify that the following lines (until another commented line)
  # are parameter estimates for the parameter named in the comment
  param_nm_inds <- grep("^#", data)
  param_start_inds <- param_nm_inds + 1
  param_end_inds <- c(param_nm_inds[-1] - 1, length(data))
  tmp <- map(seq_along(param_nm_inds), ~{
    data[param_start_inds[.x]:param_end_inds[.x]]
  })
  names(tmp) <- gsub("# +", "", data[param_nm_inds])

  tmp$num_params <- format(conv_check[1], digits = 0, scientific = FALSE)
  tmp$obj_fun_val <- format(conv_check[2], digits = 6, scientific = FALSE)
  tmp$max_gradient <- format(conv_check[3], digits = 8, scientific = FALSE)
  tmp
}

#' Read in the MCMC results from an iscam model, reformat them, and
#' remove some records as defined by `burnin` and `thin`
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param burnin The number of MCMC records to remove for burnin period
#' @param thin Remove every nth record for thinning of MCMC output
#' @param load_proj Load projection output
#' @param ... Extra arguments
#'
#' @return A list of data frames representing the MCMC output of the
#' iscam model, or NULL if there was a problem or there were no MCMC
#' output files
#' @importFrom stringr str_extract
#' @importFrom purrr imap
#' @export
read_mcmc <- function(model,
                      burnin = 1000,
                      thin = 1,
                      load_proj = TRUE,
                      ...){

  mcmc_dir <- model$mcmcpath

  if(is.null(mcmc_dir)){
    stop("`model` does not have `mcmcpath` set (model$mcmcpath is NULL)")
  }

  list_by <- function(d, by = "fleet"){
    mc <- mcmc_thin(d, burnin, thin)
    ngear <- model$dat$num.gears
    tmp <- map(seq_len(ngear), ~{
      mc %>% select(contains(paste0(by, .x)))
    }) %>%
      map(~{if(ncol(.x)) .x else NULL})
    tmp <- tmp %>%
      map(~{
        if(all(is.na(.x))) NULL else .x
      })
    tmp[sapply(tmp, is.null)] <- NULL
    # Make column names years only
    tmp <- tmp %>% map(~{
      names(.x) <- str_extract(names(.x), "[0-9]+$")
      .x
    })
    tmp
  }

  # List of files to load in, with associated type of data:
  # "default"     - thinning/burnin is done
  # "single"      - column names are converted to years, output is a data frame
  # "list"        - the data frame is broken into a list by the third item in the list
  # "projections" - no processing is done, it is done in calc_mcmc() instead
  #   which is sent to `list_by()` to extract by column names
  # "special"     - outputs which are matrices by year, gear, and sex such as
  #   age fits or age residuals. There is a special format for these files. See
  #   the mcmc_output() function in iscam source (iscam.tpl file) for the output format
  fn_lst <- list(list(mcmc.file, "default"),
                 list(mcmc.biomass.file, "single"),
                 list(mcmc.recr.file, "single"),
                 list(mcmc.recr.devs.file, "single"),
                 list(mcmc.fishing.mort.file, "list", "fleet"),
                 list(mcmc.natural.mort.file, "list", "sex"),
                 list(mcmc.fishing.mort.u.file, "list", "fleet"),
                 list(mcmc.vuln.biomass.file, "list", "fleet"),
                 list(mcmc.index.fits.file, "list", "gear"),
                 list(mcmc.index.resids.file, "list", "gear"),
                 list(mcmc.age.fits.file, "special", "gear"),
                 list(mcmc.age.resids.file, "special", "gear"),
                 list(mcmc.proj.file, "projections"))

  # Names given to the return list elements. Must be same length as `fn_lst`
  nms <- c("params", "sbt", "rt", "rdev", "ft",
           "m", "ut", "vbt", "it", "epsilon",
           "agefits", "ageresids", "proj")

  if(length(nms) != length(fn_lst)){
    stop("Length of `fn_lst` must be the same as the length of `nms`")
  }

  imap(fn_lst, ~{
    d <- NULL
    fn <- file.path(mcmc_dir, .x[1])
    if(file.exists(fn)){
      if(.x[[2]] == "special"){
        d <- load_agefit(fn,
                         gear_names = model$dat$age_gear_names,
                         burnin = burnin,
                         thin = thin)
      }else if(.x[[2]] == "default"){
        d <- read.csv(fn)
        d <- mcmc_thin(d, burnin, thin)
      }else if(.x[[2]] == "single"){
        d <- read.csv(fn)
        names(d) <- str_extract(names(d), "[0-9]+$")
        d <- mcmc_thin(d, burnin, thin)
      }else if(.x[[2]] == "list"){
        if(length(.x) < 3){
          stop("sublist ", .y, " does not have a third element which is ",
               "required when using 'list' for the second item")
        }
        d <- read.csv(fn)
        d <- list_by(d, by = .x[[3]])
        # Set column names for indices to actual years instead of numbers 1:ncol
        if(nms[.y] == "it" || nms[.y] == "epsilon"){
          # Set names and years for index fits
          d <- imap(d, ~{
            as_tibble(.x) %>%
              `names<-`(model$dat$indices[[.y]][, "iyr"])
          }) %>%
            `names<-`(model$dat$index_abbrevs)
        }
      }else if(.x[[2]] == "projections"){
        # Do no processing
        d <- read.csv(fn)
      }else{
        stop("Sublist ", .y, " of fn_lst has an unimplmented value for its second element (", .x[[2]], ")")
      }
    }else{
      warning("File ", fn, " does not exist, check iSCAM output")
    }
    d
  }) %>% `names<-`(nms)
}

#' Extract the given data frame into a list of matrices by iscam 'group'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by group. The group number is located in the names of the columns of the
#'   data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
#'   or more digits representing the group number and prefix is the string
#'   given as an argument to the function.
#'
#' @return A list of matrices, one element per group
#' @export
extract_group_matrices <- function(data = NULL,
                                   prefix = NULL){

  if(is.null(data) || is.null(prefix)){
    stop("You must give two arguments (data & prefix).")
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique_groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique_groups))
  # This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique_groups)){
    # Get all the column names (group_names) for this group by making a specific
    #  pattern for it
    group_pattern <- paste0(prefix, group, "_[[:digit:]]+")
    group_names <- names[grep(group_pattern, names)]
    # Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_([[:digit:]]+)")
    group_names <- sub(pattern, "\\1", group_names)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[, grep(group_pattern, names)]
    colnames(dat) <- group_names
    tmp[[group]] <- dat
  }
  tmp
}

#' Extract the given data frame into a list of matrices by iscam 'area'
#' and 'sex'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by area-sex and gear. The area-sex number is located in the names of the
#'   columns of the data frame in this format:
#'   "prefix[areasexnum]_gear[gearnum]_year" where [areasexnum] and [gearnum]
#'   are one or more digits and prefix is the string given as an argument
#'   to the function.
#'
#' @return a list (area-sex) of lists (gears) of matrices, one element
#'  per group
#' @export
extract_area_sex_matrices <- function(data = NULL,
                                      prefix = NULL){

  if(is.null(data) || is.null(prefix)){
    stop("You must give two arguments (data & prefix).")
  }

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_gear[[:digit:]]+_[[:digit:]]+")
  groups <- sub(pattern, "\\1", names)
  unique_groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique_groups))
  # This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique_groups)){
    # Get all the column names (group_names) for this group by making a
    #  specific pattern for it
    group_pattern <- paste0(prefix, group, "_gear[[:digit:]]+_[[:digit:]]+")
    group_names <- names[grep(group_pattern, names)]
    # Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_gear([[:digit:]]+_[[:digit:]]+)")
    group_names <- sub(pattern, "\\1", group_names)
    # At this point, group_names' elements look like this: 1_1963
    # The first value is the gear, and the second, the year.
    # Get the unique gears for this area-sex group
    pattern <- "([[:digit:]]+)_[[:digit:]]+"
    gears <- sub(pattern, "\\1", group_names)
    unique.gears <- unique(as.numeric(gears))
    tmp2 <- vector("list", length = length(unique.gears))
    for(gear in 1:length(unique.gears)){
      gear.pattern <- paste0(prefix, group,"_gear", gear, "_[[:digit:]]+")
      # Now, the data must be extracted
      # Get the column numbers that this group are included in
      dat <- data[,grep(gear.pattern, names)]
      tmp2[[gear]] <- dat
    }
    tmp[[group]] <- tmp2
  }
  tmp
}

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
    tmp <- window(mcmc_obj[,col],
                  start = burnin + 1,
                  thin = thin)
    mcmc_window <- cbind(mcmc_window, tmp)
  }
  mcmc_window <- as.data.frame(mcmc_window)
  names(mcmc_window) <- nm

  mcmc_window
}

#' Extract an age structure MPD object from iSCAM output (.rep file)
#'
#' @param model An iscam model object
#' @param type One of 'obs', 'est', 'resid'
#'
#' @return A list of data frames, one for each gear
#' @export
extract_age_output <- function(model,
                               type = "obs"){
  if(class(model) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != mdl_cls){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(!type %in% c("obs", "est", "resid")){
    stop("'type' must be one of 'obs', 'est', or 'resid'",
         call. = FALSE)
  }
  data_type <- switch(type,
                      "obs" = "d3_A",
                      "est" = "A_hat",
                      "resid" = "A_nu")
  mpd <- model$mpd
  if(is.na(mpd[1])){
    return(NA)
  }
  sage <- mpd$n_A_sage[1]
  nage <- mpd$n_A_nage[1]

  a_names <- grep(data_type, names(mpd), value = TRUE)

  j <- map(a_names, ~{
    x <- mpd[[.x]] %>% as_tibble
    if(type %in% c("est", "resid")){
      names(x) <- c("year", "gear", "area", "group", "sex", sage:nage)
    }else if(type == "obs"){
      names(x) <- c("year", "sample_size", "gear", "area", "group", "sex", sage:nage)
    }
    x
  })
  j
}

#' Fetch a data frame of the estimated MCMC parameters only
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details If all values in a given column are different, it is assumed that
#'   the parameter was estimated.
#'
#' @return A data frame of estimated parameters
#' @export
get_estimated_params <- function(mc){

  posts <- apply(mc,
                 2,
                 function(x){
                   if(length(unique(x)) > 1)
                     return(x)
                 })
  # Remove NULL list elements (fixed parameters)
  posts_lst <- posts[!sapply(posts, is.null)]
  do.call(cbind, posts_lst)
}

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
  grp <- lapply(log.params,
                function(x){
                  grep(x, nm)})
  inds.lst <- grp[sapply(grp,
                         function(x){
                           length(x) > 0})]
  inds <- unique(do.call(c, inds.lst))
  colnames(mc)[inds] <- paste0("log_", colnames(mc)[inds])
  mc[,inds] <- apply(mc[,inds],
                     2,
                     log)
  mc
}

#' Change the column name 'm1' to 'm' unless both 'm1' and 'm2' exist
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details Fo column names of data frame mc:
#'   If m1 and m2 both exist, no change
#'   If only m1 exists, change the name to m
#'
#' @return A data frame the same as mc but with modifications (see details)
#' @export
fix_m <- function(mc){

  grp <- grep("m[12]", colnames(mc))
  if(length(grp) == 1){
    colnames(mc)[grp] <- "m"
  }
  mc
}

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
