#' Create a table of MPD parameter comparisons
#'
#' @description
#' Make a table of parameter estimate comparisons for MPD models.
#' Also show B0, Fmsy. Bmsy, and msy
#'
#' @param models A list of iSCAM models as output from [load_iscam_files()]
#' @param digits Number of digits to show
#' @param french If `TRUE` translate to French
#' @param model_col_widths Widths for columns, except the Parameter column
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return An [csasdown::csas_table()]
#' @importFrom stringr str_detect
#' @importFrom kableExtra column_spec linebreak row_spec
#' @importFrom purrr map_int
#' @export
table_param_est_mpd <- function(models,
                                digits = 3,
                                french = FALSE,
                                model_col_widths = "5em",
                                ...){

  ctrls <- map(models, ~{
    .x$ctl
  })
  # Assume all models have the same primary parameters
  params <- ctrls[[1]]$params
  param_names <- rownames(params)

  mpds <- map(models, ~{
    .x$mpd
  })
  # Get catchability names so that we can add the extra rows for models with fewer indices,
  # so that binding the data frames together by column later works
  q_names <- map(models, ~{
    .x$dat$index_abbrevs
  }) %>%
    flatten %>%
    map_chr(~{.x}) %>%
    unique()

  # Maximum number of fleets. All models with less will have blank rows added for the missing gears
  num_fleets <- map_int(models, ~{
    length(.x$mpd$msy)
  }) %>%
    max
  fleet_nums <- seq_len(num_fleets)[-1]

  # Extract parameter estimates
  param_names[param_names == "h"] <- "steepness"

  param_ests <- map2(mpds, seq_along(mpds), ~{
    log_m_ind <- which(names(.x) == "log_m")
    # Test for single M, uncomment this
    #.x[log_m_ind][[1]] <- .x[log_m_ind][[1]][1]
    .x$log_m_female = .x[log_m_ind][[1]][1]
    if(length(.x[log_m_ind][[1]]) == 2){
      .x$log_m_male = .x[log_m_ind][[1]][2]
    }else{
      .x$log_m_male = NA_real_
    }
    param_inds <- match(param_names, names(.x))
    param_inds <- param_inds[!is.na(param_inds)]
    x <- .x[param_inds] %>% as.data.frame() %>% t() %>% as_tibble(rownames = "param")
    x <- x %>% select(1:2) %>% `names<-`(c("param", "value"))

    # Remove logs
    log_inds <- which(grepl("^log_", x$param))
    x$value[log_inds] <- exp(x$value[log_inds])
    x$param[log_inds] <- gsub("log_", "", x$param[log_inds])

    # Add B0, SB0
    y <- tibble(param = c("bo", "sbo", "bmsy"),
                value = c(.x$bo, .x$sbo, .x$bmsy))
    x <- x %>% bind_rows(y)

    # Add MSY-based reference points
    y <- tibble(param = c("msy", "fmsy"),
                value = c(.x$msy[1], .x$fmsy[1]))
    # The bookdown doc build will fail here if you have an exit(1); in your
    # iSCAM code somewhere and ran the model because the report file failed to be completed
    x <- x %>% bind_rows(y)
    # Multiple fishing fleets some blank
    if(length(fleet_nums)){
      for(i in fleet_nums){
        new_fleet_msy <- y %>%
          mutate(value = case_when(param == "msy" ~ .x$msy[i],
                                   param == "fmsy" ~ .x$fmsy[i],
                                   TRUE ~ NA_real_),
                 param = case_when(param == "msy" ~ paste0("msy", i),
                                   param == "fmsy" ~ paste0("fmsy", i),
                                   TRUE ~ ""))
        x <- x %>% bind_rows(new_fleet_msy)
      }
    }
    # Add q estimates
    catchability <- .x$q
    names(catchability) <- models[[.y]]$dat$index_abbrevs
    inds <- match(names(catchability), q_names)
    tmp <- rep(NA, length(q_names))
    tmp[inds] <- catchability
    names(tmp) <- q_names
    catchability <- tmp %>%
      as_tibble(rownames = "param") %>%
      mutate(param = paste0("$q_{", param, "}$"))
    x <- x %>% bind_rows(catchability)
    # Only keep param names in first data frame so when bound together they don't repeat

    if(.y != 1){
      x <- x %>% select(value)
    }
    x
  }) %>% bind_cols()
  names(param_ests)[-1] <- names(models)

  param_ests <- param_ests %>%
    mutate_at(vars(-param), function(x){format(round(x, digits), digits = 3, nsmall = 3)}) %>%
    mutate_at(vars(-param), function(x){ifelse(grepl(" +NA", x), "--", x)})

  # Rename the parameters with their latex values (where possible)
  fleet1 <- models[[1]]$dat$gear_abbrevs[1]
  fleet2 <- models[[1]]$dat$gear_abbrevs[2]
  param_latex_col <- param_ests %>% select(param)
  param_latex_col <- param_latex_col %>%
    mutate(param = case_when(param == "ro" ~ "$R_0$",
                             param == "steepness" ~ "$h$",
                             param == "m_male" ~ "$M_{male}$",
                             param == "m_female" ~ "$M_{female}$",
                             param == "rbar" ~ "$\\overline{R}$",
                             param == "rinit" ~ "$\\overline{R}_{\\mli{init}}$",
                             param == "rho" ~ "$\\rho$",
                             param == "vartheta" ~ "$\\vartheta$",
                             param == "tau" ~ "$\\tau$",
                             param == "sigma" ~ "$\\sigma$",
                             param == "bo" ~ "$B_0$",
                             param == "sbo" ~ "$SB_0$",
                             param == "msy" ~ paste0("$MSY_{", fleet1, "}$"),
                             param == "fmsy" ~ paste0("$F_{MSY_{", fleet1, "}}$"),
                             param == "msy2" ~ paste0("$MSY_{", fleet2, "}$"),
                             param == "fmsy2" ~ paste0("$F_{MSY_{", fleet2, "}}$"),
                             param == "bmsy" ~ "$B_{MSY}$",
                             str_detect(param, "^q - ") ~ gsub("q - ((\\w+\\s*)+)$", "$q_{\\1}$", param),
                             TRUE ~ param))

  param_ests <- param_ests %>%
    select(-param) %>%
    bind_cols(param_latex_col) %>%
    rename(Parameter = param) %>%
    select(Parameter, everything())

  tab <- csas_table(param_ests,
                    col.names = names(param_ests),
                    align = c("l", rep("r", ncol(param_ests) - 1)),
                    ...)

  # Add group separation lines
  num_pars <- models[[1]]$ctl$num.params
  tab <- tab %>%
    row_spec(num_pars, hline_after = TRUE) %>%
    row_spec(num_pars + 5 + 2 * (num_fleets - 1), hline_after = TRUE)

  if(!is.null(model_col_widths)){
    tab <- tab %>%
      column_spec(2:ncol(param_ests), width = model_col_widths)
  }
  tab
}

