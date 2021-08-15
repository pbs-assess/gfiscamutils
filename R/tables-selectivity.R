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

  gear_names_lst <- map(models, ~{
    .x$dat$gear_abbrevs
  })

  sels <- map2(models, seq_along(models), ~{
    gear_names <- .x$dat$gear_abbrevs
    sel_set <- .x$ctl$sel %>% t() %>% as_tibble()
    x <- .x$mpd$sel_par
    if(is.null(x)){
      x <- .x$mpd$sel_par_f
    }

    selex <- x %>% as_tibble()
    if(.x$dat$num.sex == 2){
      selex_m <- .x$mpd$sel_par_m %>% as_tibble() %>%
        mutate(V2 = 2)
      selex <- selex %>% bind_rows(selex_m)
    }
    names(selex) <- c("Gear", "Sex", "Female_P1", "Female_P2")
    selex_lst <- selex %>% group_by(Sex) %>% group_split() %>% as.list()
    if(length(selex_lst) == 2){
      selex_lst[[2]] <- selex_lst[[2]] %>% transmute(Male_P1 = Female_P1,
                                                     Male_P2 = Female_P2)
      selex <- selex_lst %>% bind_cols()
    }else{
      selex <- selex_lst[[1]] %>%
      mutate(Male_P1 = NA,
             Male_P2 = NA)
    }

    x <- selex %>% as_tibble() %>%
      mutate(Gear = gear_names) %>%
      mutate(Type = sel_set$iseltype,
             Fixed = sel_set$estphase,
             Fixed = ifelse(Type == 6 & Fixed == -row_number(), "Yes",
                            ifelse(Fixed < 0, paste0("Mirror ", gear_names_lst[[.y]][abs(sel_set$estphase)]),
                                   ifelse(Fixed > 0, "", Fixed))),
             Type = case_when(Type == 1 ~ "Logistic (1)",
                              Type == 2 ~ "Selectivity coefficients (2)",
                              Type == 3 ~ "Cubic spline with age-nodes (3)",
                              Type == 4 ~ "Time-varying cubic spline with age-nodes (4)",
                              Type == 5 ~ "Time-varying bicubic spline with age & year nodes (5)",
                              Type == 6 ~ "Logistic (6)",
                              Type == 7 ~ "Logistic function of body weight (7)",
                              TRUE ~ "NA"))

    if(!all(gear_names_lst[[.y]] %in% gear_names)){
      missing_gears <- gear_names_lst[[.y]][!gear_names_lst[[.y]] %in% gear_names]
      for(i in missing_gears){
        new_row <- x[1,]
        new_row$gear <- i
        new_row <- new_row %>% mutate_at(vars(-gear), ~{NA})
        x <- x %>% bind_rows(new_row)
      }
    }

    x <- x %>% mutate(Model = names(models)[.y]) %>%
      select(Model, everything())
    # Remove model names for all but first row
    x <- arrange(x, match(Gear, gear_names_lst[[.y]]))
    x <- x %>% mutate(Model = ifelse(row_number() == 1, Model, ""))
  }) %>%
    bind_rows() %>%
    rename(`Female Age50` = Female_P1,
           `Female SD50` = Female_P2,
           `Male Age50` = Male_P1,
           `Male SD50` = Male_P2)

  sels <- sels %>%
    mutate_at(vars(`Female Age50`,
                   `Female SD50`,
                   `Male Age50`,
                   `Male SD50`),
              function(x){format(round(x, digits), digits = 3, nsmall = 3)}) %>%
    mutate_at(vars(`Female Age50`,
                   `Female SD50`,
                   `Male Age50`,
                   `Male SD50`),
              function(x){ifelse(grepl(" *NA", x), "--", x)}) %>%
    mutate(Type = ifelse(is.na(Type), "--", Type)) %>%
    mutate(Fixed = ifelse(is.na(Fixed), "--", Fixed))

  tab <- csas_table(sels,
                    col.names = names(sels),
                    ...)

  # Add group separation lines
  gear_names_lst_cumu <- cumsum(map_int(gear_names_lst, ~{length(.x)}))
  map(gear_names_lst_cumu, ~{
    tab <<- tab %>%
      row_spec(.x, hline_after = TRUE)
  })

  if(!is.null(model_col_widths)){
    tab <- tab %>%
      column_spec(2:ncol(sels), width = model_col_widths)
  }
  tab
}
