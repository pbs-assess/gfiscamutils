#' Return a table by area, where the areas are defined by the unique values
#' in the `area` column in table `catch_df`
#'
#' @keywords internal
#' @inheritParams table_catch
#' @export
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

  names(tab) <- tr(names(tab))
  area_nm <- length(areas) * 2
  names(area_nm) <- tr("Area")
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

