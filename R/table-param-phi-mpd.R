#' Create a table comparing phi parameter
#'
#' @description
#' Make a table of comparisons of estimates of the phi parameter for the
#' Dirichlet Multinomial for MPD models.
#'
#' @param model An iSCAM model as output from [load_iscam_files()]
#' @param format See argument in [knitr::kable()]
#' @param yrs A vector of years to include in the table. If `NULL`, all
#' will be included
#' @param digits Number of digits to show
#' @param col_widths Widths for columns, except the Parameter column
#' @param ret_df If `TRUE`, return a data.frame instead of the
#' [csasdown::csas_table()]
#' @param by_sex If `TRUE`, the table will have two rows per year, one for
#' each sex. If `FALSE`, the female value will be used
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return An [csasdown::csas_table()]
#' @importFrom kableExtra add_header_above
#' @export
table_param_phi_mpd <- function(model,
                                format = "latex",
                                yrs = NULL,
                                digits = 2,
                                col_widths = "5em",
                                ret_df = FALSE,
                                by_sex = FALSE,
                                ...){

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
  }

  gear_names <- model$dat$age_gear_abbrevs
  # Split matrix into a list of the rows
  lp <- model$mpd$log_phi %>% split(seq(nrow(.))) %>% `names<-`(gear_names)
  samp_size <- model$mpd$dm_sample_sizes %>% split(seq(nrow(.))) %>% `names<-`(gear_names)
  age_comps <- model$dat$age.comps
  lp <- lp %>% map(~{.x[!is.na(.x)]})
  samp_size <- samp_size %>% map(~{.x[!is.na(.x)]})
  # Strip year and sex cols from the age comps data input matrices
  yr_sex <- map(age_comps, ~{
    as_tibble(.x) %>% select(year, sex)
  })
  # Make a list of data frames containing the input sample sizes, log_phi, and Neff estimates by gear
  lp_samp <- map(seq_along(yr_sex), ~{
    lp[[.x]] <- rep(lp[[.x]], length(samp_size[[.x]]))
    lp[[.x]] <- as_tibble(lp[[.x]])
    samp_size[[.x]] <- as_tibble(samp_size[[.x]])
    out <- bind_cols(yr_sex[[.x]],
                     samp_size[[.x]],
                     (samp_size[[.x]] + samp_size[[.x]] * exp(lp[[.x]])) / (samp_size[[.x]] + exp(lp[[.x]])),
                     lp[[.x]])
    names(out) <- c("Year",
                    "Sex",
                    "n",
                    "neff",
                    "log_phi")

    out <- out |>
      mutate(Sex = ifelse(Sex == 0, "Female", ifelse(Sex == 1, "Female", "Male"))) |>
      mutate_at(vars(4:5), ~{format(round(., digits), digits = digits, nsmall = digits)}) %>%
      mutate_at(vars(3), ~{format(., nsmall = 0)})
    if(!is.null(yrs)){
      out <- out |>
        filter(Year %in% yrs)
    }
    out
  })
  # Join all the data frames in the list into one data frame
  xx <- lp_samp[[1]]
  for(df in lp_samp[seq_along(lp_samp)[-1]]){
    xx <- full_join(xx, df, by = c("Year", "Sex"))
  }
  xx <- xx[order(c(xx$Year, xx$Sex)),] %>%
    filter(!is.na(Year))

  out <- xx %>%
    mutate_at(vars(-c(Year, Sex)), ~replace(., is.na(.), "--"))

  # Construct extra group headers for gears
  col_names <- c("Year", "Sex")
  header_above <- c(" " = 1)
  for(i in seq_along(gear_names)){
    if(format == "html"){
      col_names <- c(col_names, "\\(N\\)", "\\(N_{eff}\\)", "\\(log(\\phi)\\)")
    }else{
      col_names <- c(col_names, "$N$", "$N_{eff}$", "$log(\\phi)$")
    }
    header_above <- c(header_above, setNames(3, gear_names[i]))
  }
  names(out) <- col_names

  if(!by_sex){
    tmp_nms <- names(out)
    tmp_nms <- tmp_nms[tmp_nms != "Sex"]
    names(out) <- as.character(1:ncol(out))
    out <- out |>
      filter(`2` == "Female") |>
      select(-`2`)|>
      mutate(`1` = as.character(`1`))
    names(out) <- tmp_nms
  }

  if(ret_df){
    return(out)
  }

  if(format == "html"){
    tab <- out |>
      kable(...) |>
      add_header_above(header_above)
  }else{
    tab <- csas_table(out,
                      format = format,
                      col_names = names(out),
                      align = c("l", rep("r", ncol(out) - 1)),
                      col_names_align = rep("r", ncol(out)),
                      ...) |>
      add_header_above(header_above)

    if(!is.null(col_widths)){
      tab <- tab %>%
        column_spec(2:ncol(out), width = col_widths)
    }
  }

  tab
}

