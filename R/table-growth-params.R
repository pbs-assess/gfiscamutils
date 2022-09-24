#' Create table of the growth parameters for input into the ISCAM model
#'
#' @details
#' iSCAM takes these values as inputs in the DAT file and creates a vector
#' of maturities-at-age by calling `plogis(a50, sd50)` where `a50` and `sd50`
#' are the outputs of this function, `sd50` is actually the shape parameter,
#' not a true standard deviation of values.
#'
#' @param params_fn A filename (RDS file) to read in, containing the data frame
#' output by `export_mat_lw_age(survey_samples_syn, write_file = FALSE)`
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param ret_df Logical. If `TRUE`, return the [data.frame] and not
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return Either a [data.frame] or a [csasdown::csas_table()], depending on
#' the value of `return_df`
#' @importFrom purrr map_dfr
#' @export
table_growth_params <- function(params_fn = file.path(dirname(here()),
                                                      "arrowtooth-nongit/data/growth.rds"),
                                col_widths = NULL,
                                ret_df = FALSE,
                                digits = 2,
                                ...){

  params <- readRDS(params_fn) |>
    select(comments, everything()) |>
    rename(Parameter = comments,
           Female = female,
           Male = male)
  params$Parameter <- firstup(gsub("^# -(.*)$", "\\1", params$Parameter))
  params$Parameter <- gsub("linf", "$l_{\\\\infty}$", params$Parameter)
  params$Parameter <- gsub("\\(k\\)", "\\($k$\\)", params$Parameter)
  params$Parameter <- gsub("tt0", "$tt_0$", params$Parameter)
  params$Parameter <- gsub("alpha", "$\\\\alpha$", params$Parameter)
  params$Parameter <- gsub("beta", "$\\\\beta$", params$Parameter)
  params$Parameter <- gsub("%", "$\\\\%$", params$Parameter)

  j <- pmap(params, ~{
    mtch <- grepl("alpha", ..1)
    if(mtch){
      return(c(f(..2, 7), f(..3, 7)))
    }
    c(f(..2, digits), f(..3, digits))
  }) |>
    setNames(1:nrow(params)) |>
    map_dfr(~{.x}) |>
    t() |>
    as_tibble()

  params[, 2:3] <- j

  if(ret_df){
    return(params)
  }

  names(params) <- c(tr("Parameter"),
                     tr("Female"),
                     tr("Male"))

  out <- csas_table(params,
                    format = "latex",
                    align = c("l", rep("r", ncol(params) - 1)),
                    col_names_align = rep("r", ncol(params)),
                    ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = col_widths)
  }

  out
}
