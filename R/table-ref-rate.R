#' Make a table containing the reference rate and values adssociated with it
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param lst A list of the reference rate and associated parameters as
#' output by [find_f_b40()]
#' @param format One of "latex" or "html"
#' @param digits Number of decimal places to show in the table
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_ref_rate <- function(model,
                           lst,
                           format = "latex",
                           digits = 3,
                           ...){

  fleet_nms <- model$dat$fleet_gear_names

  catch_nm <- tr("Catch (kt)")
  catch_sym <- sym(catch_nm)
  catch_df <- vec2df(c("", "", f(lst$catch, digits)),
                     nms = c("$F_{0.4B_0}$",
                             "$U_{0.4B_0}$",
                             catch_nm))

  fleet_sym <- sym(tr("Fleet"))

  d <- tibble(`$F_{0.4B_0}$` = f(lst$f, digits),
              `$U_{0.4B_0}$` = f(lst$u, digits),
              !!catch_sym := f(lst$catch * model$dat$gear.alloc[1:2], digits)) |>
    bind_rows(catch_df) |>
    mutate(Fleet = c(fleet_nms, tr("Total"))) |>
    select(Fleet, everything()) |>
    rename(!!fleet_sym := Fleet) |>
    map_df(~{gsub(" +NA", "", .x)})

  csas_table(d,
             format = format,
             bold_header = FALSE,
             ...) |>
    row_spec(2, hline_after = TRUE)
}
