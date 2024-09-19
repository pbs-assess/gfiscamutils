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

#' Return a table by fleet, where the fleets are determined by the `...`
#' argument as data frames
#'
#' @keywords internal
#' @inheritParams table_catch
#' @param catch_df_lst A list of data frames with catch, one for each fleet
#' which have had [gfplot::tidy_catch()] applied
#' @param fleet_nms A vector of names for the fleets, to appear on the table.
#' Must be the same length as `catch_df_lst`
#' @param show_total_col If `TRUE`, show a column with the total landings
#' and discards for all fleets
#' @param bold_headers If `TRUE`, make all column headers bold
#' @export
table_catch_fleet <- function(catch_df_lst = NULL,
                              fleet_nms = NULL,
                              start_yr = NULL,
                              gear_col_widths = "5em",
                              scale_factor = 1e3,
                              ret_df = FALSE,
                              show_total_col = TRUE,
                              bold_headers = TRUE,
                              ...){

  if(is.null(catch_df_lst)){
    stop("`catch_df_lst` must not be `NULL`",
         call. = FALSE)
  }

  if(!"list" %in% class(catch_df_lst)){
    stop("`catch_df_lst` must be a list",
         call. = FALSE)
  }

  if(is.null(fleet_nms)){
    stop("`fleet_nms` must not be `NULL`",
         call. = FALSE)
  }

  if(length(catch_df_lst) != length(fleet_nms)){
    stop("`catch_df_lst` nust be the same length as `fleet_nms`",
         call. = FALSE)
  }

  if(is.null(start_yr)){
    start_yr <- min(catch_df_lst[[1]]$year)
  }else{
    if(start_yr < min(catch_df_lst[[1]]$year) ||
       start_yr > max(catch_df_lst[[1]]$year)){
      stop("`start_yr` is not within the range of years in the `catch_df_lst[[1]]` ",
           " data frame (", paste(range(catch_df_lst[[1]]$year), collapse = "-"), ")",
           call. = FALSE)
    }
  }

  # Capitalize all words in gear names for table headers
  fleet_nms <- strsplit(fleet_nms, " ") |>
    map_chr(\(flt){
        paste0(toupper(substring(flt, 1, 1)),
               substring(flt, 2),
               collapse = " ")
    })

  catch_df <- map2_dfr(catch_df_lst,
                       fleet_nms,
                       function(catch_df, fleet_nm){
                         catch_df <- catch_df |>
                           group_by(year, gear) |>
                           summarize(value = sum(value)) |>
                           pivot_wider(names_from = "gear",
                                       values_from = "value") |>
                           ungroup() |>
                           mutate(fleet = fleet_nm)
                       })

  year_flt <- catch_df |>
    select(year, fleet)
  discarded <- catch_df |>
    select(Discarded)
  landed <- catch_df |>
    select(-c(year, fleet, Discarded)) |>
    rowSums(na.rm = TRUE) |>
    enframe(name = NULL)

  # Re-build the catch_df data frame
  tab <- bind_cols(year_flt, landed)
  tab <- bind_cols(tab, discarded)

  tab <- tab |>
    filter(year >= start_yr) |>
    rename(Year = year,
           Landings = value,
           Fleet = fleet) |>
    mutate(Landings = Landings / scale_factor,
           Discarded = Discarded / scale_factor)

  tab <- tab[order(tab$Year), ]

  fleets <- unique(tab$Fleet)
  tab <- tab |>
    pivot_wider(names_from = Fleet, values_from = c("Landings", "Discarded"))

  # Order the columns by fleet
  tmp <- map(fleets, ~{
    x <- tab[, grep(.x, names(tab))]
    if(show_total_col){
      x <- x |>
        mutate(total = rowSums(x))
    }
    x
    }) |>
    map_dfc(~{.x})
  if(show_total_col){
    # Add total of totals
    tot <- tmp[, grep("total", names(tmp))] |>
      rowSums() |>
      enframe(name = NULL)
    tmp <- tmp |>
      bind_cols(tot)
  }

  tab <- cbind(enframe(tab$Year,
                       value = "Year",
                       name = NULL), tmp) |>
    as_tibble()

  names(tab) <- gsub("^(Landings|Discarded)_.*$", "\\1", names(tab))
  names(tab) <- gsub("^total.*$", "Total", names(tab))
  names(tab) <- gsub("^value$", "Total Catch", names(tab))

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- 0

  if(ret_df){
    return(tab)
  }

  header_vec <- c(" " = 1)
  for(i in seq_along(fleets)){
    header <- ifelse(show_total_col, 3, 2)
    names(header) <- fleets[i]
    header_vec <- c(header_vec, header)
  }

  names(tab) <- tr(names(tab))
  fleet_nm <- length(fleets) * ifelse(show_total_col, 3, 2)
  names(fleet_nm) <- tr("Fleet")
  fleet_header_vec <- c(" ", fleet_nm)

  if(show_total_col){
    header_vec <- c(header_vec, " " = 1)
  }

  # Make bold headers
  if(bold_headers){
    names(tab) <- paste0("\\textbf{", names(tab), "}")
    names(header_vec) <- ifelse(names(header_vec) == " ",
                                " ",
                                paste0("\\\\textbf{", names(header_vec), "}"))
  }

  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...) |>
    add_header_above(header = header_vec, escape = FALSE) #|>
    #add_header_above(header = fleet_header_vec)

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

