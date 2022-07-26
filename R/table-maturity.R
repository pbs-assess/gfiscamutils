#' Table of the Age-at-50% maturity and shape parameter values
#' (sd50) for input into iSCAM
#'
#' @details
#' iSCAM takes these values as inputs in the DAT file and creates a vector
#' of maturities-at-age by calling `plogis(a50, sd50)` where `a50` and `sd50`
#' are the outputs of this function, `sd50` is actually the shape parameter,
#' not a true standard deviation of values.
#'
#' The maturity code table for maturity_convention_code 4 (flatfish):
#' MATURITY_CODE,SPECIMEN_SEX_CODE,MATURITY_NAME,MATURITY_DESC
#' 1, 1, IMMATURE, "TESTES VERY SMALL, STRING-LIKE AND SOMEWHAT TRANSLUCENT OR PINKISH IN COLOUR"
#' 1, 2, IMMATURE, "OVARIES VERY SMALL, TRANSLUCENT OR PINKISH  AND SOMEWHAT GELATINOUS IN TEXTURE"
#' 2, 1, MATURING, "TESTES ENLARGING, A DISTINCT BULGE EVIDENT BUT STILL TRANSLUCENT OR PINKISH IN COLOUR"
#' 2, 2, MATURING, "OVARIES RELATIVELY SMALL, PINKISH-YELLOW OR CREAM IN COLOUR, GRANULAR IN TEXTURE.  NO DISTINCT EGGS VISIBLE"
#' 3, 1, DEVELOPING, "TESTES ENLARGING, BROWN-WHITE OR WHITE IN COLOUR, FIRM IN TEXTURE"
#' 3, 2, DEVELOPING, "OVARIES LARGE, CREAM OR YELLOW IN COLOUR CONTAINING OPAQUE EGGS THAT CAN BE DISTINGUISED BY DIRECT OBSERVATION.  SEX MAY BE DETERMINED EXTERNALLY"
#' 4, 1, RIPE, "TESTES LARGE, WHITE AND EASILY BROKEN. NO SPERM EVIDENT"
#' 4, 2, GRAVID, "OVARIES CONTAINING PARTLY OR WHOLLY TRANSLUCENT EGGS.  SEX EASILY DETERMINED EXTERNALLY"
#' 5, 1, SPAWNING, "TESTES LARGE, WHITE AND SPERM EVIDENT"
#' 5, 2, RIPE, "OVARIES CONTAINING ENTIRELY TRANSLUCENT, MATURE OVA.  EGGS LOOSE AND WILL RUN FROM OVIDUCTS UNDER SLIGHT PRESSURE"
#' 6, 1, SPENT, "TESTES FLACCID, SHRUNKEN AND YELLOW-BROWN IN COLOUR. SPERM DUCTS ENLARGED AND A SMALL AMOUNT OF SPRM MAY BE PRESENT"
#' 6, 2, SPENT, "OVARIES LARGE, FLACCID AND PURPLE IN COLOUR; A FEW TRANSLUCENT EGGS MAY BE LEFT.  OVARIAN MEMBRANE VERY VASCULAR (BLOODSHOT) AND SAC-LIKE"
#' 7, 1, RESTING, "TESTES FIRM, SMALL AND YELLOW-BROWN IN COLOUR.  SPERM DUCTS SMALL"
#' 7, 2, RESTING, "OVARIES CONTRACTED AND FIRM, PINKISH GREY TO CREAM-YELLOW IN COLOUR AND MAY APPEAR GRANULAR IN TEXTURE BUT NO DISTINCT EGGS ARE VISIBLE"
#'
#' @param surv_samples A `survey_samples` list as output by
#' [gfdata::get_survey_samples()]
#' @param surv_series A vector of integers representing the survey ids to use
#' 1:4 = c("SYN QCS", "OTHER HS MSA", "SYN HS", "SYN WCVI")
#' @param maturity_code cutoff value for maturity code. See table in `details`
#' section. This value and above is considered mature and below this value
#' is considered immature
#' @param maturity_convention_code Type of maturity scale to use. Flatfish is
#' code 4 (default)
#' @param usability_codes The codes to use as described in the 'usability'
#' table in `gfbiosql`. Run this to see all the codes:
#' [gfdata::runsql("gfbiosql", "select * from usability")]
#' @param start_year First year to include. If `NULL`, start year of data will
#' be used
#' @param end_year Last year to include
#'
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param return_df Logical. If `TRUE`, return the [data.frame] and not
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return Either a [data.frame] or a [csasdown::csas_table()], depending on
#' the value of `return_df`
#' @export
table_maturity <- function(surv_samples = NULL,
                           surv_series = 1:4,
                           maturity_code = 5,
                           maturity_convention_code = 4,
                           usability_codes = NULL,
                           start_year = NULL,
                           end_year = NULL,
                           col_widths = NULL,
                           return_df = FALSE,
                           ...){

  if(is.null(surv_samples)){
    stop("`surv_samples` must not be `NULL`", call. = FALSE)
  }

  tab <- surv_samples
  if(!is.null(surv_series)){
    tab <- tab |>
      filter(survey_series_id %in% surv_series)
  }
  yrs <- sort(unique(surv_samples$year))

  if(is.null(start_year) || !start_year %in% yrs){
    start_year <- yrs[1]
  }
  if(is.null(end_year) || !end_year %in% yrs){
    end_year <- yrs[length(yrs)]
  }
  if(!is.null(usability_codes)){
    tab <- tab |>
      filter(usability_code %in% usability_codes)
  }

  tab <- tab |>
    filter(year %in% start_year:end_year) |>
    filter(!is.na(age),
           !is.na(maturity_code),
           sex %in% 1:2) |>
    filter(maturity_convention_code %in% !!maturity_convention_code) |>
    select(age, sex, maturity_code)

  # Get num specimens
  num_spec <- tab |>
    group_by(sex) |>
    summarize(n()) |>
    ungroup() |>
    select(-sex) |>
    rename(num_specimens = `n()`)

  age_prop_mature <- map(1:2, ~{
    tab |>
      filter(sex == .x) |>
      group_by(age) |>
      mutate(is_mature = ifelse(maturity_code < !!maturity_code, FALSE, TRUE)) |>
      summarize(prop_mature = sum(is_mature) / n()) |>
      ungroup()
  })

  mat_model <- function(par, age, prop_mature){
    prop_m <- 1 / (1 + exp(- ((age - par[1]) / par[2])))
    sum((prop_m - prop_mature) ^ 2)
  }

  # For testing single sex table making
  #age_prop_mature <- age_prop_mature[-1]

  kk <- imap(age_prop_mature, ~{
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

  # Determine if the output contains one sex (female) or two
  nsex <- length(kk)

  out_df <- tibble("Sex" = `if`(nsex == 1, "Female", c("Male", "Female")))
  mat_vals_df <- map_df(kk, ~{
    vec2df(.x$par, c("A50", "SD50"))
  })
  out_df <- out_df |>
    bind_cols(mat_vals_df) |>
    bind_cols(num_spec)

  if(return_df){
    return(out_df)
  }

  names(out_df) <- c("Sex", "$A_{50}$", "$SD_{50}$", "Number of specimens")
  out <- csas_table(out_df,
                    format = "latex",
                    align = rep("r", ncol(out_df)),
                    col_names_align = rep("r", ncol(out_df)),
                    ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = col_widths)
  }

  out
}
