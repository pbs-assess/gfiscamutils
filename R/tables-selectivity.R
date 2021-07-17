#' Make a table of selectivity parameter estimate comparisons for MPD models
#'
#' @rdname param_est_mpd_table
#'
#' @return an [csasdown::csas_table()]
#' @importFrom dplyr everything case_when
#' @importFrom csasdown csas_table
#' @export
sel_param_est_mpd_table <- function(models,
                                    digits = 3,
                                    french = FALSE,
                                    model_col_widths = NULL,
                                    ...){

  gear_names_all <- map(models, ~{
    .x$dat$gear_abbrevs
  }) %>%
    flatten %>%
    map_chr(~{.x}) %>%
    unique()
  sels <- map2(models, names(models), ~{
    gear_names <- .x$dat$gear_abbrevs
    sel_set <- .x$ctl$sel %>% t() %>% as_tibble()
    x <- .x$mpd$sel_par %>% as_tibble() %>%
      mutate(gear = gear_names) %>%
      select(gear, everything()) %>%
      mutate(Type = sel_set$iseltype,
             Fixed = sel_set$estphase,
             Fixed = ifelse(Type == 6 & Fixed == -row_number(), "Yes",
                            ifelse(Fixed < 0, paste0("Mirror ", gear_names_all[abs(sel_set$estphase)]),
                                   ifelse(Fixed > 0, "", Fixed))),
             Type = case_when(Type == 1 ~ "Logistic (1)",
                              Type == 2 ~ "Selectivity coefficients (2)",
                              Type == 3 ~ "Cubic spline with age-nodes (3)",
                              Type == 4 ~ "Time-varying cubic spline with age-nodes (4)",
                              Type == 5 ~ "Time-varying bicubic spline with age & year nodes (5)",
                              Type == 6 ~ "Logistic (6)",
                              Type == 7 ~ "Logistic function of body weight (7)",
                              TRUE ~ "NA"))

    if(!all(gear_names_all %in% gear_names)){
      missing_gears <- gear_names_all[!gear_names_all %in% gear_names]
      for(i in missing_gears){
        new_row <- x[1,]
        new_row$gear <- i
        new_row <- new_row %>% mutate_at(vars(-gear), ~{NA})
        x <- x %>% bind_rows(new_row)
      }
    }
    x <- x %>% mutate(model = .y) %>%
      select(model, everything())
    # Remove model names for all but first row
    x <- arrange(x, match(gear, gear_names_all))
    x <- x %>% mutate(model = ifelse(row_number() == 1, model, ""))
  }) %>%
    bind_rows() %>%
    select(-V1, -V2) %>%
    rename(`Param 1` = V3,
           `Param 2` = V4,
           Model = model,
           Gear = gear)

  sels <- sels %>%
    mutate_at(vars(`Param 1`, `Param 2`), function(x){format(round(x, digits), digits = 3, nsmall = 3)}) %>%
    mutate_at(vars(`Param 1`, `Param 2`), function(x){ifelse(grepl(" +NA", x), "--", x)}) %>%
    mutate(Type = ifelse(is.na(Type), "--", Type)) %>%
    mutate(Fixed = ifelse(is.na(Fixed), "--", Fixed))

  tab <- csas_table(sels,
                    col.names = names(sels),
                    ...)

  group_sep <- length(gear_names_all)

  # Add group separation lines
  for(i in 1:nrow(sels)){
    if(i %% group_sep == 0){
      tab <- tab %>%
        row_spec(i, hline_after = TRUE)
    }
  }

  if(!is.null(model_col_widths)){
    tab <- tab %>%
      column_spec(2:ncol(sels), width = model_col_widths)
  }
  tab
}
