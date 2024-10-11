#' Perform some quantile calculations on the MCMC posteriors
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param burnin The number of MCMC records to remove for burnin period
#' @param thin Remove every nth record for thinning of MCMC output
#' @param probs The probabilities to use for `quantile()` calculations
#' @param load_proj Do the calculations for the projections
#' @param index_scale Number to multiply the index values by so they match the
#' `survey_index` values
#' @param ... Arguments to be absorbed by other functions
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
  if(!inherits(probs, "numeric")){
    stop("`probs` must be a numeric vector")
  }
  if(any(probs < 0 | probs > 1)){
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
    stop("There is no MCMC output for the model in ", model$mcmcpath,
         " Did you remember to run iscam -mceval?")
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
    j <- apply(mc_d, 2, quantile, prob = probs, na.rm = TRUE)
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

  # Stock-recruit parameters
  so <- out$params$so
  beta <- out$params$beta
  phie <- out$params$phie
  if(length(beta) != length(so) || length(beta) != length(phie)){
    stop("Number of burned-in posteriors for so, beta, and phie for stock ",
         "recruit calculation arre not the same. Check ", mcmc.file, " file",
         call. = FALSE)
  }
  # Do stock-recruit calculation on each posterior
  out$sr <- list()
  bio <- seq(50, 200, length.out = 200)
  for(i in seq_along(so)){
    out$sr[[i]] <- vec2df((so[i] * bio) / (1.0 + beta[i] * bio) * exp(-0.5 * model$mpd$tau * model$mpd$tau))
    names(out$sr[[i]]) <- as.character(1:ncol(out$sr[[i]]))
  }
  out$sr <- out$sr |>
    map_df(~{.x})

  sr_mpd <- (mpd$so * mpd$rt * mpd$phie) / (1.0 + mpd$beta * mpd$rt * mpd$phie)
  out$sr_quants <- quantify(out$sr, sr_mpd)
  bio <- vec2df(bio, nms = seq_along(bio))
  row_nms <- rownames(out$sr_quants)
  out$sr_quants <- out$sr_quants |>
    as_tibble() |>
    bind_rows(bio) |>
    as.matrix()
  rownames(out$sr_quants) <- c(row_nms, "sbt")

  # Recruitment deviations
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
      if(is.data.frame(mpd_dat) || is.matrix(mpd_dat)){
        q <- rbind(q, mpd_dat[.y, ])
      }else{
        q <- rbind(q, mpd_dat)
      }
      rownames(q)[length(probs) + 1] <- "MPD"
      q
    })
  }

  # Catch
  out$ct <- mc$ct
  out$ct_quants <- build_quant_list(mc$ct, mpd$ct)

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
    out$selest_quants <- calc_longer_quants(mc$selest,
                                           type = "sel",
                                           probs)
  }

  if(load_proj){

    # Burn in and calculate quantiles for each TAC level
    mc$proj <- distinct(mc$proj) |>
      filter(TAC != "TAC")

    out$proj <- mc$proj |>
      rename(catch = TAC) |>
      mutate_all(~as.numeric(.x)) |>
      split(~catch) |>
      map(~{mcmc_thin(.x, burnin = burnin, thin = thin)})

    # Extract biomass values only
    out$proj_sbt <- out$proj |>
      map(~{
        nms <- names(.x)
        wch_bio <- grep("^B20[0-9]{2}$", nms, value = T)
        select(.x, c(catch, all_of(wch_bio)))
      }) |>
      bind_rows()

    # Calculate depletion values only
    sbo <- out$params$sbo

    out$proj_depl <- out$proj_sbt |>
      group_by(catch) |>
      mutate_at(vars(-catch), ~{
        length(sbo) <- length(.x)
        as.numeric(.x) / sbo}) |>
      ungroup()

    names(out$proj_sbt) <- gsub("B", "", names(out$proj_sbt))
    names(out$proj_depl) <- gsub("B", "", names(out$proj_depl))

    perc <- round(probs * 100, 1)
    perc[2] <- ifelse(perc[2] == 50.0, "50", perc[2])
    perc <- paste0(perc, "%")

    # Pass in proj_depl or proj_sbt from above to proj_df
    calc_proj_quants <- function(proj_df){
      proj_df |>
        split(~catch) |>
        imap(~{
          x <- .x |> imap(~{
            if(.y != "catch"){
              quantile(.x, probs, na.rm = TRUE)
            }
          })
          x[lengths(x) == 0] <- NULL
          yrs <- names(x)
          x <- x |> bind_rows() |>
            mutate(year = yrs,
                   catch = .y) |>
            select(catch, year, everything())
        }) |>
        bind_rows() |>
        mutate(catch = as.numeric(catch),
               year = as.numeric(year))
    }

    out$proj_sbt_quants <- calc_proj_quants(out$proj_sbt)
    out$proj_depl_quants <- calc_proj_quants(out$proj_depl)

    # Replace the final year of the sbt with the values obtained from the
    # TAC 0 projection
    if(nrow(out$proj$`0`) < nrow(out$sbt[, ncol(out$sbt)])){
      diff <- nrow(out$sbt) - nrow(out$proj$`0`)
      warning("The number of posteriors in the zero catch scenario is ",
              nrow(out$proj$`0`), " when it should be ",
              nrow(out$sbt), ". The last ", diff, " posteriors were ",
              "replicated to allow the RDS file to be built, ",
              "but you should run the mcmc to a longer chain length\n")
      rws <- (nrow(out$proj$`0`) + 1):(nrow(out$proj$`0`) + diff)
      extra_rows <- out$sbt |> slice(rws)
      out$proj$`0` <- out$proj$`0` |> bind_rows(extra_rows)
    }
    out$sbt[, ncol(out$sbt)] <- out$proj$`0`[, 2]
    out$sbt_quants <- quantify(out$sbt, mpd$sbt)
  }

  out
}
