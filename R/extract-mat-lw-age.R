#' Export maturity, length/weight, and age-at-50% parameters by
#' creating a file with the output parameters which can be easily
#' pasted into an iSCAM data file
#'
#' @param surv_samples Output from [gfdata::get_survey_samples()]
#' @param surv_abbrevs The survey abbreviations to filter for.
#' If `NULL`, no filtering will occur
#' @param areas The areas to filter for. If `NULL`, no filtering
#' will occur
#' @param maturity_code The maturity code as found in the GFBio database
#' @param maturity_convention_code The maturity convention code as
#' found in the GFBio database
#' @param ageing_method_codes A numeric vector of ageing method codes to filter
#' on. Defaults to `NULL`, which brings in all valid ageing codes
#' See [gfdata::get_age_methods()].
#' @param usability_codes An optional vector of usability codes.
#' All usability codes not in this vector will be omitted.
#' Set to `NULL` to include all samples
#' @param digits The number of digits each number should have in the
#' output
#' @param write_file Logical. If `TRUE`, write the output to the file,
#' if `FALSE`, return the output as an object
#' @param fn The file name
#' @param ... Absorb unintended parameters passed through ... in
#' calling functions
#'
#' @return If `write_file` is `FALSE`, a table of the parameter estimates
#' @importFrom gfplot fit_mat_ogive fit_length_weight
#' @export
export_mat_lw_age <- function(surv_samples,
                              surv_abbrevs = c("SYN QCS",
                                               "SYN WCVI",
                                               "SYN HS",
                                               "SYN WCHG"),
                              # areas here are 3CD and 5ABCDE in order
                              areas = c("03",
                                        "04",
                                        "05",
                                        "06",
                                        "07",
                                        "08",
                                        "09"),
                              maturity_code = 3,
                              maturity_convention_code = 4,
                              ageing_method_codes = NULL,
                              usability_codes = c(0, 1, 2, 6),
                              digits = 7,
                              write_file = TRUE,
                              fn = paste0("vonb-lw-age50-", Sys.Date(), ".txt"),
                              ...){

  options(pillar.sigfig = 7)

  if(!is.null(surv_abbrevs)){
    surv_samples <- surv_samples |>
      filter(survey_abbrev %in% surv_abbrevs)
  }
  if(!is.null(areas)){
    surv_samples <- surv_samples |>
      filter(major_stat_area_code %in% areas)
  }
  if(!is.null(usability_codes)){
    surv_samples <- surv_samples |>
      filter(usability_code %in% usability_codes)
  }
  if(!is.null(ageing_method_codes)){
    surv_samples <- surv_samples |>
      filter(ageing_method %in% ageing_method_codes)
  }

  surv_samples <- surv_samples[!duplicated(surv_samples$specimen_id), , drop = FALSE] # critical!

  # Von B fits
  vb_f <- fit_vb(surv_samples, sex = "female")
  fem_pars <- c(vb_f$pars$linf, vb_f$pars$k, vb_f$pars$t0)
  vb_m <- fit_vb(surv_samples, sex = "male")
  mal_pars <- c(vb_m$pars$linf, vb_m$pars$k, vb_m$pars$t0)

  # Length/weight fits
  lw_f <- fit_length_weight(surv_samples, sex = "female")
  fem_pars <- c(fem_pars, exp(lw_f$pars$log_a), lw_f$pars$b)
  lw_m <- fit_length_weight(surv_samples, sex = "male")
  mal_pars <- c(mal_pars, exp(lw_m$pars$log_a), lw_m$pars$b)

  # a50 is only here to compare with the a50_non_log output using browser()
  a50 <- fit_mat_ogive(surv_samples, type = "age")

  # Age-at-50% fits
  if(!is.null(maturity_convention_code) && !is.null(maturity_code)){
    surv_samples <- surv_samples |>
      filter(!is.na(age),
             !is.na(maturity_code),
             sex %in% 1:2,
             maturity_convention_code != 9) |>
      #filter(maturity_convention_code %in% !!maturity_convention_code) |>
      select(age, sex, maturity_code)

    age_prop_mature <- map(1:2, function(sx){
      surv_samples |>
        filter(sex == sx) |>
        group_by(age) |>
        mutate(is_mature = ifelse(maturity_code < !!maturity_code, FALSE, TRUE)) |>
        summarize(prop_mature = sum(is_mature) / n()) |>
        ungroup()
    })

    mat_model <- function(par, age, prop_mature){
      prop_m <- 1 / (1 + exp(- par[2] * (age - par[1])))
      sum((prop_m - prop_mature) ^ 2)
    }

    a50_non_log <- map2(age_prop_mature, 1:2, ~{
      conv <- optim(par = c(5, 3),
                    fn = mat_model,
                    method = "L-BFGS-B",
                    lower = 0,
                    upper = Inf,
                    age = age_prop_mature[[.y]]$age,
                    prop_mature = age_prop_mature[[.y]]$prop_mature)
      if(conv$convergence != 0){
        stop("Age-at-50% model failed to converge for ",
             ifelse(.y == 1, "males", "females"),
             ". Message from optim was: ", conv$message,
             call. = FALSE)
      }
      conv
    })

    mal_pars <- c(mal_pars, a50_non_log[[1]]$par[1], a50_non_log[[1]]$par[2])
    fem_pars <- c(fem_pars, a50_non_log[[2]]$par[1], a50_non_log[[2]]$par[2])
  }

  mal_pars <- enframe(mal_pars, name = NULL)
  names(mal_pars) <- "male"
  fem_pars <- enframe(fem_pars, name = NULL)
  names(fem_pars) <- "female"
  out <- round(bind_cols(fem_pars, mal_pars), digits)

  # Add comments at end of lines
  comments <- c("# -asymptotic length (linf)",
                "# -brody growth coefficient (k)",
                "# -theoretical age at zero length (tt0)",
                "# -scalar in length-weight allometry (alpha)",
                "# -power parameter in length-weight allometry (beta)",
                "# -age at 50% maturity",
                "# -std at 50% maturity")
  comments <- enframe(comments, name = NULL)
  names(comments) <- "comments"
  out <- bind_cols(out, comments)

  if(write_file){
    nongit_dir <- file.path(dirname(here()), "arrowtooth-nongit")
    dir.create(file.path(nongit_dir, "data-output"), showWarnings = FALSE)
    fn <- file.path(nongit_dir,
                    file.path("data-output",
                              fn))
    write.table(out,
                fn,
                quote = FALSE,
                row.names = FALSE)
    message("Created file ", fn)
  }else{
    # Get number of specimens for maturity model
    # num_spec <- surv_samples |>
    #   filter(!is.na(age),
    #          !is.na(maturity_code),
    #          sex %in% 1:2) |>
    #   filter(maturity_convention_code %in% !!maturity_convention_code) |>
    #   select(age, sex, maturity_code) |>
    #   group_by(sex) |>
    #   summarize(n()) |>
    #   ungroup() |>
    #   mutate(sex = ifelse(sex == 1, "male", "female")) |>
    #   rename(num_specimens = `n()`)

    out
  }
}
