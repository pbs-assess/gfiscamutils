#' Create a table of catch, by Landings and Discards
#'
#' @param catch_df A data frame as output by [gfplot::tidy_catch()]
#' @param start_yr The year to start the table or `NULL` to include all data
#' @param by_area Logical. If TRUE, make a table by area (grouped as 3CD and
#' 5ABCDE)
#' @param gear_col_widths Width to make the columns for the gear types
#' @param scale_factor Value to divide all the catches by to make the table
#' in tonnes
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_catch <- function(catch_df,
                        start_yr = NULL,
                        by_area = FALSE,
                        gear_col_widths = "5em",
                        scale_factor = 1e3,
                        ...){

  if(length(unique(catch_df$species_common_name)) > 1){
    stop("There is more than one species in `catch_df`", call. = FALSE)
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

  names(tab) <- en2fr(names(tab))

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

#' Return a table by area, where the areas are defined by the unique values
#' in the `area` column in table `catch_df`
#'
#' @keywords internal
#' @inheritParams table_catch
table_catch_area <- function(catch_df,
                             start_yr = NULL,
                             gear_col_widths = "5em",
                             scale_factor = 1e3,
                             ...){

  catch_df <- catch_df |>
    group_by(year, gear, area) |>
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

  year_area <- catch_df |>
    select(year, area)
  discarded <- catch_df |>
    select(Discarded)
  landed <- catch_df |>
    select(-c(year, area, Discarded)) |>
    rowSums(na.rm = TRUE) |>
    enframe(name = NULL)

  # Re-build the catch_df data frame
  tab <- bind_cols(year_area, landed)
  tab <- bind_cols(tab, discarded)

  tab <- tab |>
    filter(year >= start_yr) |>
    rename(Year = year,
           Landings = value,
           Area = area) |>
    mutate(Landings = Landings / scale_factor,
           Discarded = Discarded / scale_factor)

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  areas <- unique(tab$Area)
  tab <- tab |>
    pivot_wider(names_from = Area, values_from = c("Landings", "Discarded"))

  # Order the columns by area
  tmp <- map(areas, ~{tab[, grep(.x, names(tab))]}) |>
    map_dfc(~{.x})
  tab <- cbind(enframe(tab$Year, name = NULL), tmp) |>
    as_tibble() |>
    rename(Year = value)
  names(tab) <- gsub("^(Landings|Discarded)_.*$", "\\1", names(tab))

  header_vec <- c(" " = 1)
  for(i in seq_along(areas)){
    header <- 2
    names(header) <- areas[i]
    header_vec <- c(header_vec, header)
  }

  names(tab) <- en2fr(names(tab))
  area_nm <- length(areas) * 2
  names(area_nm) <- en2fr("Area")
  area_header_vec <- c(" " = 1, area_nm)
  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...) |>
    add_header_above(header = header_vec) |>
    add_header_above(header = area_header_vec)

  if(!is.null(gear_col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = gear_col_widths)
  }

  out
}
