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
    selex <- x %>%
      as_tibble() %>%
      mutate(Sex = "Female")
    if(.x$dat$num.sex == 2){
      selex_m <- .x$mpd$sel_par_m %>%
        as_tibble() %>%
        mutate(Sex = "Male")
      selex <- selex %>% bind_rows(selex_m)
    }
    names(selex) <- c("gear", "year_ind", "p1", "p2", "sex")

    selex_lst <- selex %>%
      group_by(sex) %>%
      group_split() %>%
      as.list()
    selex_lst[[1]] <- selex_lst[[1]] %>%
      rename(Female_P1 = p1, Female_P2 = p2)
    if(length(selex_lst) == 2){
      selex_lst[[2]] <- selex_lst[[2]] %>%
        transmute(Male_P1 = p1, Male_P2 = p2)
      selex <- selex_lst %>% bind_cols()
    }
    selex <- selex %>% select(-sex)
    # Check for TV selectivity
    selex_lst <- selex %>% group_by(gear) %>% group_split()
    model <- .x
    selex <- map2(selex_lst, seq_along(selex_lst), ~{
      if(nrow(.x) == 1){
        out <- .x %>% mutate(year_ind = "All")
      }else{
        # TV selectivity, need to replace year indices with year ranges
        years <- c(model$ctl$start.yr.time.block[.x$gear[1], ], model$dat$end.yr)
        offset_years <- years[-1] - 1
        offset_years[length(offset_years)] <- offset_years[length(offset_years)] + 1
        blocked_years <- paste0(years[-length(years)], "-", offset_years)
        out <- .x <- .x %>%
          mutate(year_ind = blocked_years[year_ind])
      }
      out <- out %>%
        rename(Years = year_ind) %>%
        mutate(Years = factor(Years)) %>%
        mutate(Gear = gear_names[.y]) %>%
        select(-gear) %>%
        mutate(Type = sel_set$iseltype[.y],
               Fixed = sel_set$estphase[.y],
               Fixed = ifelse(Type == 6, "Yes", "--"),
               Type = case_when(Type == 1 ~ "Logistic (1)",
                                Type == 2 ~ "Selectivity coefficients (2)",
                                Type == 3 ~ "Cubic spline with age-nodes (3)",
                                Type == 4 ~ "Time-varying cubic spline with age-nodes (4)",
                                Type == 5 ~ "Time-varying bicubic spline with age & year nodes (5)",
                                Type == 6 ~ "Logistic (6)",
                                Type == 7 ~ "Logistic function of body weight (7)",
                                TRUE ~ "NA")) %>%
        select(Gear, Years, everything())

      out
    }) %>% map_dfr(~{.x})

    if(!all(gear_names_lst[[.y]] %in% gear_names)){
      missing_gears <- gear_names_lst[[.y]][!gear_names_lst[[.y]] %in% gear_names]
      for(i in missing_gears){
        new_row <- selex[1,]
        new_row$gear <- i
        new_row <- new_row %>% mutate_at(vars(-gear), ~{NA})
        selex <- selex %>% bind_rows(new_row)
      }
    }

    selex <- selex %>% mutate(Model = names(models)[.y]) %>%
      select(Model, everything())
    # Remove model names for all but first row
    selex <- arrange(selex, match(Gear, gear_names_lst[[.y]]))
    selex <- selex %>% mutate(Model = ifelse(row_number() == 1, Model, ""))
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
  # Remove last line as it is the bottom of the table anyway
  gear_names_lst_cumu <- gear_names_lst_cumu[-length(gear_names_lst_cumu)]
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
