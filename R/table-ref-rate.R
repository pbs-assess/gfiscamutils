#' Make a table containing the reference rate and values adssociated with it
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param lst A list of the reference rate and associated parameters as
#' output by [find_f_b40()]
#' @param format One of "latex" or "html"
#' @param digits Number of decimal places to show in the table
#' @param bold_headers If `TRUE`, make all column headers bold
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_ref_rate <- function(model,
                           lst,
                           format = "latex",
                           digits = 3,
                           bold_headers = TRUE,
                           ...){

  fleet_nms <- model$dat$fleet_gear_names

  catch_nm <- tr("Catch (kt)")
  catch_sym <- sym(catch_nm)
  f_col <- ifelse(fr(),
                  "$F_{0{,}4B_0}$",
                  "$F_{0.4B_0}$")
  f_col_sym <- sym(f_col)
  u_col <- ifelse(fr(),
                  "$U_{0{,}4B_0}$",
                  "$U_{0.4B_0}$")
  u_col_sym <- sym(u_col)
  catch_df <- vec2df(c("", "", f(lst$catch, digits)),
                     nms = c(f_col,
                             u_col,
                             catch_nm))

  fleet_sym <- sym(tr("Fleet"))

  d <- tibble(!!f_col_sym := f(lst$f, digits),
              !!u_col_sym := f(lst$u, digits),
              !!catch_sym := f(lst$catch * model$dat$gear.alloc[1:2], digits)) |>
    bind_rows(catch_df) |>
    mutate(Fleet = c(fleet_nms, tr("Total"))) |>
    select(Fleet, everything()) |>
    rename(!!fleet_sym := Fleet) |>
    map_df(~{gsub(" +NA", "", .x)})

  if(bold_headers){
    names(d) <- gsub("^\\$", "$\\\\mathbf{", names(d))
    names(d) <- gsub("\\$$", "}$", names(d))
    inds_not_math <- !grepl("^\\$", names(d))
    nms_not_math <- names(d)[inds_not_math]
    nms_not_math <- paste0("\\textbf{", nms_not_math, "}")
    names(d)[inds_not_math] <- nms_not_math
  }

  csas_table(d,
             format = format,
             bold_headers = FALSE,
             ...) |>
    row_spec(2, hline_after = TRUE)
}
