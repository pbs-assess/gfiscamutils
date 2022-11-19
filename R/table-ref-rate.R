#' Make a table containing the reference rate and values adssociated with it
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param lst A list of the reference rate and associated parameters as
#' output by [find_f_b40()]
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
  catch_df <- vec2df(c("", "", lst$catch),
                     nms = c("$F_{0.4B_0}$",
                             "$U_{0.4B_0}$",
                             "Catch (kt)")) |>
    mutate_all(as.numeric)

  d <- tibble(`$F_{0.4B_0}$` = lst$f,
              `$U_{0.4B_0}$` = lst$u,
              `Catch (kt)` = lst$catch * model$dat$gear.alloc[1:2]) |>
    bind_rows(catch_df) |>
    mutate_all(~{f(.x, digits)}) |>
    mutate(Fleet = c(fleet_nms, "Total")) |>
    select(Fleet, everything()) |>
    map_df(~{gsub(" +NA", "", .x)})

  csas_table(d,
             format = format,
             bold_header = FALSE,
             ...) |>
    row_spec(2, hline_after = TRUE)
}
