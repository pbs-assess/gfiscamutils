#' Create a table of catch, by Landings and Discards
#'
#' @param catch_df A data frame as output by [gfplot::tidy_catch()]
#' @param start_yr The year to start the table or `NULL` to include all data
#' @param by_area Logical. If TRUE, make a table by area (grouped as 3CD and
#' 5ABCDE)
#' @param gear_col_widths Width to make the columns for the gear types
#' @param scale_factor Value to divide all the catches by to make the table
#' in tonnes
#' @param ret_df If `TRUE` return a data frame, if `FALSE`, return an
#' [csasdown::csas_table()]
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_catch <- function(catch_df,
                        start_yr = NULL,
                        by_area = FALSE,
                        gear_col_widths = "5em",
                        scale_factor = 1e3,
                        ret_df = FALSE,
                        ...){

  if(length(unique(catch_df$species_common_name)) > 1){
    stop("There is more than one species in `catch_df`",
         call. = FALSE)
  }

  catch_df <- catch_df |>
    select(-species_common_name)

  if(by_area){
    return(table_catch_area(catch_df,
                            start_yr,
                            gear_col_widths,
                            scale_factor,
                            ...))
  }

  catch_df <- catch_df |>
    group_by(year, gear) |>
    summarize(value = sum(value)) |>
    pivot_wider(names_from = "gear", values_from = "value") |>
    ungroup()

  if(is.null(start_yr)){
    start_yr <- min(catch_df$year)
  }else{
    if(start_yr < min(catch_df$year) || start_yr > max(catch_df$year)){
      stop("`start_yr` is not within the range of years in the `catch_df` ",
           " data frame (", paste(range(catch_df$year), collapse = "-"), ")",
           call. = FALSE)
    }
  }

  # Sum all non-discarded to make landings sum column
  years <- catch_df |>
    select(year)
  discarded <- catch_df |>
    select(Discarded)
  landed <- catch_df |>
    select(-c(year, Discarded)) |>
    rowSums(na.rm = TRUE) |>
    enframe(name = NULL)

  # Re-build the catch_df data frame
  tab <- bind_cols(years, landed)
  tab <- bind_cols(tab, discarded)

  tab <- tab |>
    filter(year >= start_yr) |>
    rename(Year = year,
           Landings = value) |>
    mutate(Landings = Landings / scale_factor,
           Discarded = Discarded / scale_factor)

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  if(ret_df){
    return(tab)
  }

  names(tab) <- tr(names(tab))

  out <- csas_table(tab,
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(!is.null(gear_col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = gear_col_widths)
  }

  out
}
