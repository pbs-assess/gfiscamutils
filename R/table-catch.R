#' Create a table of catch by gear type
#'
#' @param catch_df A data frame as output by [gfplot::tidy_catch()]
#' @param start_yr The year to start the table or `NULL` to include all data
#' @param digits Number of decimal places for the values in the table
#' @param gear_rm A vector of gear types to remove from the table. Look at the
#' function code to see the list of available gears (`available_gears`)
#' @param gear_col_widths Width to make the columns for the gear types
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_catch <- function(catch_df,
                        start_yr = NULL,
                        digits = 2,
                        gear_rm = available_gears,
                        gear_col_widths = "5em",
                        ...){

  available_gears <- c("Bottom trawl",
                       "Discarded",
                       "Unknown/trawl",
                       "Midwater",
                       "trawl",
                       "Hook and line",
                       "LONGLINE",
                       "Trap")

  if(!all(gear_rm %in% available_gears)){
    stop("Not all `gear_rm` are allowed. See the function argument list for choices",
         call. = FALSE)
  }

  if(length(unique(catch_df$area)) != 1){
    stop("More than one area found in the `catch_df` data frame",
         call. = FALSE)
  }

  if(length(unique(catch_df$species_common_name)) != 1){
    stop("More than one species_common_name found in the `catch_df` data frame",
         call. = FALSE)
  }

  if(is.null(start_yr)){
    start_yr <- min(catch_df$year)
  }else{
    if(start_yr < min(catch_df$year) || start_yr > max(catch_df$year)){
      stop("`start_yr` is not within the range of years in the `catch_df` ",
           " data frame (", paste(range(catch_df$year), collapse = "-"), ")",
           call. = FALSE)
    }
  }

  tab <- catch_df %>%
    select(-area, -species_common_name) %>%
    filter(!gear %in% gear_rm) %>%
    filter(year >= start_yr) %>%
    pivot_wider(names_from = "gear", values_from = "value") %>%
    rename(Year = year)

  if(fr()){
    names(tab) <- en2fr(names(tab))
  }

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(!is.null(gear_col_widths)){
    out <- out %>%
      column_spec(2:ncol(tab), width = gear_col_widths)
  }

  out
}
