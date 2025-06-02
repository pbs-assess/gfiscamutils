#' Create a table of index data values
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param digits Number of decimal points to show in table
#' @param ... Arguments passed to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()] object
#' @export
table_index_values <- function(model,
                               col_widths = "5em",
                               digits = 2,
                               ...){


  indices <- model$dat$indices
  index_nms <- model$dat$index_gear_names
  if(length(indices) != length(index_nms)){
    stop("The model does not have the same number of indices in the dat ",
         "file as index names in the dat file", call. = FALSE)
  }

  # Remove useless columns
  tmp <- imap(indices, ~{.x |>
      as_tibble() |>
      select(iyr, it, wt) |>
      mutate(wt = round(1 / wt, digits)) |>
      rename(Year = iyr,
             Index = it,
             CV = wt)
    })

  #tab <- purrr::reduce(tmp, dplyr::full_join, by = "Year")
  tmp_join <- tmp[[1]]
  tmp <- tmp[-1]
  for(i in seq_along(tmp)){
    tmp_join <- full_join(tmp_join, tmp[[i]], by = "Year")
  }
  tab <- tmp_join |> arrange(Year)

  # Replace NA's with dashes in whole table
  tab <- tab |>
    mutate_all(~{ifelse(is.na(.), "--", .)})

  # Rename all columns to Index or CV (except Year)
  names(tab) <- gsub("(\\.\\w)+$", "", names(tab))

  header_vec <- c(" " = 1)
  for(i in seq_along(index_nms)){
    header <- 2
    names(header) <- index_nms[i]
    names(header) <- gsub(" ", "\\\n", names(header))
    header_vec <- c(header_vec, header)
  }

  tmp_nms_inds <- which(names(tab) != "CV")
  names(tab)[tmp_nms_inds] <- tr(names(tab)[tmp_nms_inds])

  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...) |>
    add_header_above(header = header_vec, ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = col_widths)
  }

  out
}
