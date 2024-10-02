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
#' @param format See `format` parameter in [knitr::kable()]
#' @export
table_catch_fleets <- function(catch_df_lst = NULL,
                               fleet_nms = NULL,
                               start_yr = NULL,
                               gear_col_widths = "5em",
                               scale_factor = 1e3,
                               ret_df = FALSE,
                               show_total_col = TRUE,
                               bold_headers = TRUE,
                               format = "latex",
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
  names(tab) <- gsub("^Discarded", "Discards", names(tab))
  names(tab) <- gsub("^total.*$", "Total", names(tab))
  names(tab) <- gsub("^value$", "Total Catch", names(tab))

  # Change to French if necessary
  names(tab) <- tr(names(tab))

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
    if(format == "latex"){
      names(tab) <- paste0("\\textbf{", names(tab), "}")
      names(header_vec) <- ifelse(names(header_vec) == " ",
                                  " ",
                                  paste0("\\\\textbf{", names(header_vec), "}"))
    }else if(format == "html"){
    }else{
      stop("`format` must be one of 'latex' or 'html")
    }
  }

  out <- csas_table(tab,
                    format = format,
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...) |>
    add_header_above(header = header_vec, escape = FALSE)

  if(!is.null(gear_col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = gear_col_widths)
  }

  out
}

