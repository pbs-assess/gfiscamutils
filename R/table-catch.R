#' Create a table of catch by gear type
#'
#' @param catch_df A data frame as output by [gfplot::tidy_catch()]
#' @param start_yr The year to start the table or `NULL` to include all data
#' @param avail_gears If `TRUE`, return a vector of the names of gears you can
#' provide to `gear_rm`. If `FALSE`, return the [csasdown::csas_table()]
#' @param gear_rm A vector of gear types to remove from the table. Call this
#' function with `avail_gears` set to `TRUE` for a listing of available gears
#' @param gear_col_widths Width to make the columns for the gear types
#' @param by_gear If `TRUE` the table will contain a column for each gear
#' except for those found in `gear_rm`. If `FALSE` only a total column will be
#' shown, unless `by_area` is `TRUE`, then a total column for each area will
#' be shown
#' @param by_area If `TRUE`, the table will contain a column for each area
#' except for those found in `area_rm`. If `FALSE` only a total column will be
#' shown, unless `by_gear` is `TRUE`
#' @param scale_factor Value to divide all the catches by to make the table
#' in tonnes
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_catch <- function(catch_df,
                        start_yr = NULL,
                        avail_gears = FALSE,
                        gear_rm = available_gears,
                        gear_col_widths = "5em",
                        scale_factor = 1e3,
                        ...){

  available_gears <- c("Bottom trawl",
                       "Discarded",
                       "Unknown/trawl",
                       "Midwater",
                       "trawl",
                       "Hook and line",
                       "LONGLINE",
                       "Trap")

  if(avail_gears){
    return(available_gears)
  }

  if(!all(gear_rm %in% available_gears)){
    stop("Not all `gear_rm` are in the list of available gears. Call this ",
         "function with `avail_gears` set to `TRUE` for a listing of ",
         "available gears",
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

  # Add 'Total Landings' column
  total <- tab %>%
    select(-Year, -Discarded) %>%
    mutate(total = rowSums(.)) %>%
    select(total) %>%
    rename(`Total Landings` = total)
  tab <- tab %>%
    bind_cols(total)

  # Convert values and swap last two cols
  tab <- tab %>%
    mutate_at(vars(-Year), ~{. / scale_factor})
  ord <- 1:ncol(tab)
  tmp_ord <- ord
  ord[length(ord) - 1] <- ord[length(ord)]
  ord[length(ord)] <- tmp_ord[length(ord) - 1]
  tab <- tab[, ord]

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  if(fr()){
    names(tab) <- en2fr(names(tab))
  }

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
