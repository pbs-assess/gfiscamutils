#' Create a table of catch by gear type
#'
#' @param catch_df A data frame as output by [gfplot::tidy_catch()]
#' @param start_yr The year to start the table or `NULL` to include all data
#' @param area If `NULL`, sum together all catch by year for all areas found
#' in `catch_df`. Otherwise, a single area name (eg. 3CD or 5ABCDE). If the
#' area name is not in `catch_df$area`, an error will be thrown
#' @param gear_col_widths Width to make the columns for the gear types
#' @param scale_factor Value to divide all the catches by to make the table
#' in tonnes
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_catch <- function(catch_df,
                        start_yr = NULL,
                        area = NULL,
                        gear_col_widths = "5em",
                        scale_factor = 1e3,
                        ...){

  if(length(unique(catch_df$species_common_name)) > 1){
    stop("There is more than one species in `catch_df`", call. = FALSE)
  }
  catch_df <- catch_df |>  select(-species_common_name)


  if(is.null(area)){
    catch_df <- catch_df |>
      group_by(year, gear) |>
      summarize(value = sum(value))
  }else{
    if(length(area) > 1){
      stop("`area` must either be `NULL` or a single area name. You have ",
           "supplied more than one name.",
           call. = FALSE)
    }
    if(!area %in% catch_df$area){
      stop("`area` = '", area, "' was not found in `catch_df`",
           call. = FALSE)
    }
    catch_df <- catch_df |>
      filter(area == !!area)
  }

  catch_df <- catch_df |>
    pivot_wider(names_from = "gear", values_from = "value")

  if(is.null(start_yr)){
    start_yr <- min(catch_df$year)
  }else{
    if(start_yr < min(catch_df$year) || start_yr > max(catch_df$year)){
      stop("`start_yr` is not within the range of years in the `catch_df` ",
           " data frame (", paste(range(catch_df$year), collapse = "-"), ")",
           call. = FALSE)
    }
  }

  browser()
  tab <- catch_df |>
    filter(year >= start_yr) |>
    rename(Year = year)
browser()
  # Add 'Total Landings' column
  total <- tab |>
    select(-Year, -Discarded) |>
    mutate(total = rowSums(.)) |>
    select(total) |>
    rename(`Total Landings` = total)
  tab <- tab |>
    bind_cols(total)

  # Convert values and swap last two cols
  tab <- tab |>
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
    out <- out |>
      column_spec(2:ncol(tab), width = gear_col_widths)
  }

  out
}
