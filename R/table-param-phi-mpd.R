#' Create a table comparing phi parameter
#'
#' @description
#' Make a table of comparisons of estimates of the phi parameter for the
#' Dirichlet Multinomial for MPD models.
#'
#' @param model An iSCAM model as output from [load_iscam_files()]
#' @param digits Number of digits to show
#' @param french If `TRUE` translate to French
#' @param col_widths Widths for columns, except the Parameter column
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return An [csasdown::csas_table()]
#' @importFrom kableExtra add_header_above
#' @export
table_param_phi_mpd <- function(model,
                                digits = 2,
                                french = FALSE,
                                col_widths = "5em",
                                ...){

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
    out %>%
      mutate(Sex = ifelse(Sex == 0, "Female", ifelse(Sex == 1, "Female", "Male")))%>%
      mutate_at(vars(4:5), ~{format(round(., digits), digits = digits, nsmall = digits)}) %>%
      mutate_at(vars(3), ~{format(., digits = 0, nsmall = 0)})
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
  header_above <- c(" " = 2)
  for(i in seq_along(gear_names)){
    col_names <- c(col_names, "$N$", "$N_{eff}$", "$log(\\phi)$")
    header_above <- c(header_above, setNames(3, gear_names[i]))
  }
  names(out) <- col_names

  tab <- csas_table(out,
                    col.names = names(out),
                    align = c("l", rep("r", ncol(out) - 1)),
                    ...) %>%
    add_header_above(header_above)

  if(!is.null(col_widths)){
    tab <- tab %>%
      column_spec(2:ncol(out), width = col_widths)
  }
  tab
}

