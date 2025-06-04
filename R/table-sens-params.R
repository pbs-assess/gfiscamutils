#' Create a table summarizing changes for sensitivity models
#'
#' @param sens_desc_vec A vector of sensitivity descriptions
#' @param sens_change_vec A vector of sensitivity changes
#' @param ret_df Logical. If `TRUE`, return a data frame instead of the table
#' @param col_widths A vector of column widths. See [csasdown::csas_table()]
#' @param ... Arguments passed to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_sens_param_changes <- function(sens_desc_vec,
                                     sens_change_vec,
                                     ret_df = FALSE,
                                     col_widths = NULL,
                                     bold_header = TRUE,
                                     ...){

  if(length(sens_desc_vec) != length(sens_change_vec)){
    stop("Lengths of the input vectors must be the same",
         call. = FALSE)
  }

  desc <- enframe(unlist(sens_desc_vec), name = NULL) |>
    `names<-`(tr("Description"))
  changes <- enframe(unlist(sens_change_vec), name = NULL) |>
    `names<-`(tr("Changes"))
  tab <- bind_cols(desc, changes)

  if(ret_df){
    return(tab)
  }

  out <- csas_table(tab,
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    bold_header = bold_header,
                    align = rep("l", ncol(tab)),
                    col_names_align = rep("l", ncol(tab)),
                    ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = col_widths)
  }

  out
}
