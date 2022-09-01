#' Load CSV files in the 'longer' data format. These files will have a
#' column for gear, posterior number, and year and many rows. MCMC
#' burnin and thinning is applied to each unique group
#'
#' @details
#' Made to load the age fit, age residuals, or selectivity
#' estimates files.
#'
#' @param fn The CSV filename
#' @param gear_names A vector of gear names to use in the resulting data frame.
#' If `NULL`, the gear numbers in the file will be used
#' @param burnin The number of MCMC records to remove for burnin period
#' @param thin Remove every nth record for thinning of MCMC output
#' @param end_yr The year to set as the end year for the final year block in
#' selectivity. If there is only one block, this will be the end year for it.
#' This is only used if `type` is "sel"
#'
#' @return The list of output data frames representing the age fits, age
#' residuals, or selectivity parameter estimates by gear and year
#' @importFrom readr read_csv
#' @importFrom dplyr cur_group_id filter group_by ungroup slice select
#' @importFrom rlang sym
#' @importFrom lubridate year
#' @export
load_longer <- function(fn,
                        type = c("age", "sel"),
                        gear_names = NULL,
                        burnin = 1000,
                        thin = 1,
                        end_yr = year(Sys.Date()) - 1){

  type <- match.arg(type)

  if(!file.exists(fn)){
    stop("The file `", fn, "` does not exist", call. = FALSE)
  }

  d <- read_csv(fn)

  if(type == "age"){
    year_colname <- "year"
  }else{
    year_colname <- "start_year"
  }
  year <- sym(year_colname)

  if(!year_colname %in% names(d)){
    stop("Column `", year_colname, "` not found in the file `", fn, "`", call. = FALSE)
  }

  df <- d |>
    group_by(gear, sex, !!year) |>
    slice(seq_len(burnin)) |>
    slice(seq(1, burnin, thin)) |>
    ungroup()

  if(!is.null(end_yr)){
    df <- df |>
      filter(!!year <= end_yr)
  }

  if(!is.null(gear_names)){
    if(length(gear_names) != length(unique(df$gear))){
      stop("Length of `gear_names` is not the same as the number of unique ",
           "gears found in the file `", fn, "`", call. = FALSE)
    }
    df <- df |>
      group_by(gear) |>
      mutate(gear_ind = cur_group_id()) |>
      mutate(gear = gear_names[gear_ind]) |>
      ungroup() |>
      select(-gear_ind)
  }

  df
}
