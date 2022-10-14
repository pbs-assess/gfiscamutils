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
#' @importFrom dplyr rename bind_rows
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
    mc_d <- mc_d |>
      as_tibble() |>
      map_df(~{as.numeric(.x)})
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
      #.x <- .x[, colSums(is.na(.x)) != nrow(.x)]
      # Replace NAs with zeros
      .x[is.na(.x)] <- 0
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
        mutate(survey_name = .y)
    }) %>%
      bind_rows() %>%
      rename(lowerci = !!sym(prob_cols[1]),
             biomass = !!sym(prob_cols[2]),
             upperci = !!sym(prob_cols[3])) %>%
      select(survey_name, year, biomass, lowerci, upperci) %>%
      mutate(year = as.numeric(year))
  }
  # Index residuals
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  if(!is.null(mc$epsilon)){
    names(mc$epsilon) <- names(mc$it)
    out$epsilon_quants <- imap(mc$epsilon, ~{
      imap(.x, ~{
        quantile(.x, probs = probs, na.rm = TRUE) |>
          t() |>
          as.data.frame() |>
          as_tibble() |>
          mutate(year = .y) |>
          select(year, everything()) |>
          `names<-`(c("year", "lowerci", "biomass", "upperci"))
      }) |>
        bind_rows() |>
        mutate(survey_name = .y)
    }) |>
      bind_rows() |>
      select(survey_name, year, everything()) %>%
      mutate(year = as.numeric(year))
  }

  if(!is.null(mc$std_epsilon)){
    names(mc$std_epsilon) <- names(mc$it)
    out$std_epsilon_quants <- imap(mc$std_epsilon, ~{
      imap(.x, ~{
        quantile(.x, probs = probs, na.rm = TRUE) |>
          t() |>
          as.data.frame() |>
          as_tibble() |>
          mutate(year = .y) |>
          select(year, everything()) |>
          `names<-`(c("year", "lowerci", "biomass", "upperci"))
      }) |>
        bind_rows() |>
        mutate(survey_name = .y)
    }) |>
      bind_rows() |>
      select(survey_name, year, everything()) %>%
      mutate(year = as.numeric(year))
  }

  if(!is.null(mc$agefits)){
    # Can't ttrust these functions...
    # out$agefit_quants <- calc_longer_quants(mc$agefits,
    #                                         type = "age",
    #                                         probs)
  }
  if(!is.null(mc$ageresids)){
    # out$ageresids_quants <- calc_longer_quants(mc$ageresids,
    #                                            type = "age",
    #                                            probs)
  }
  if(!is.null(mc$selest)){
    # out$selest_quants <- calc_longer_quants(mc$selest,
    #                                         type = "sel",
    #                                         probs)
  }

  if(load_proj){
    # Burn in and calculate quantiles for each TAC level
    mc$proj <- distinct(mc$proj) |>
      filter(TAC != "TAC")

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
