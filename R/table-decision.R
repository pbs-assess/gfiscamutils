#' Create a decision table with a single reference point or probabilities
#' of biomass declining or increasing
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param catch_vals The catch values to include in the table. If `NULL`,
#' all catch values present in the projections will be used
#' @param ret_df Logical. If `TRUE` return a data frame with the values,
#' instead of the [csasdown::csas_table()] formatted table
#' @param format One of "html" or "latex"
#' @param bold_header If `TRUE`, make the headers bold. This only works
#' if the `format` is 'html'. If it is 'latex', you must paste latex bold
#' macros around the column headers manually
#' @param ... Arguments to be passed to [get_proj_biomass_raw()],
#' [calc_probs_biomass()], and [csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_decision <- function(model,
                           catch_vals = NULL,
                           ret_df = FALSE,
                           format = c("latex", "html"),
                           bold_header = FALSE,
                           ...){

  format <- match.arg(format)

  x <- get_proj_biomass_raw(model, ...)

  if(!is.null(catch_vals[1])){
    x <- x |>
      filter(catch %in% catch_vals)
  }

  tab <- calc_probs_biomass(x, format = format, ...)

  names(tab)[1] <- tr("Catch (kt)")

  if(ret_df){
    return(tab)
  }

  out <- csas_table(tab,
                    format = format,
                    bold_header = bold_header,
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol("tab")),
                    ...)

  out

}
