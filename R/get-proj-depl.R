#' Extract the projection biomass or depletion columns of raw output
#'
#' @param model An ISCAM model object (class [mdl_cls])
#' @param calc_depl If `TRUE` calculate relative biomass (depletion) if
#' `ret_biomass_cols` is `TRUE`  and change the output columns to those
#' relative biomasses instead of absolute biomass
#' @param ... Absorb arguments intended for other functions
#'
#' @return a data frame with the (number of posteriors * the number of catch
#' levels) rows and columns for each year of biomass projected with the years
#' as column names
#'
#' @export
get_proj_biomass_raw <- function(model,
                                 calc_depl = TRUE,
                                 ...){

  num_proj_yrs <- model$proj$num.projyrs
  ctl_options <- model$proj$ctl.options |>
    as_tibble(rownames = "variable") |>
    rename(value = V1)
  start_yr <- ctl_options |>
    filter(variable == "syrmeanm") |>
    pull(value)
  end_yr <- ctl_options |>
    filter(variable == "nyrmeanm") |>
    pull(value)

  proj <- model$mcmccalcs$proj |> map_dfr(~{.x})
  col_regex <- "^B(2[0-9]{3})$"
  ct_ind <- grep("^catch$", names(proj))
  b_inds <- grep(col_regex, names(proj))

  if(!length(ct_ind)){
    stop("There are no columns with the name `catch` in the ",
         "`model$mcmccalcs$proj` output")
  }
  if(!length(b_inds)){
    stop("There are no columns of the format B2024 in the ",
         "`model$mcmccalcs$proj` output")
  }
  d <- proj |>
    select(ct_ind, b_inds)
  # Remove preceding 'B' from year columns (ignores catch col)
  names(d) <- gsub(col_regex, "\\1", names(d))

  if(calc_depl){
    sbo <- model$mcmccalcs$params$sbo
    d <- d |>
      group_by(catch) |>
      mutate(across(.cols = everything(),
                    .fns = ~{.x / sbo})) |>
      ungroup()
  }

  d
}
