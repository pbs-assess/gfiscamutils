#' Create table of the growth parameters which were input into the ISCAM model
#'
#' @details
#' iSCAM takes these values as inputs in the DAT file and creates a vector
#' of maturities-at-age by calling `plogis(a50, sd50)` where `a50` and `sd50`
#' are the outputs of this function, `sd50` is actually the shape parameter,
#' not a true standard deviation of values.
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param ret_df Logical. If `TRUE`, return the [data.frame] and not
#' @param digits The number of decimal places to report on all except the
#' `alpha` parameter
#' @param alpha_digits The number of decimal places to report on the `alpha`
#' parameter
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return Either a [data.frame] or a [csasdown::csas_table()], depending on
#' the value of `return_df`
#'
#' @export
table_growth_params <- function(model,
                                col_widths = NULL,
                                ret_df = FALSE,
                                digits = 2,
                                alpha_digits = 7,
                                ...){

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
  }

  param_names <- enframe(c(ifelse(fr(),
                                  "Longueur asymptotique ($l_{inf}$)",
                                  "Asymptotic length ($l_{inf}$)"),
                           ifelse(fr(),
                                  "Coefficient de croissance de Brody ($k$)",
                                  "Brody growth coefficient ($k$)"),
                           ifelse(fr(),
                                  "Âge théorique à la longueur zéro ($t_0$)",
                                  "Theoretical age at zero length ($t_0$)"),
                           ifelse(fr(),
                                  "Scalaire dans l'allométrie longueur-poids ($\\alpha$)",
                                  "Scalar in length-weight allometry ($\\alpha$)"),
                           ifelse(fr(),
                                  "Paramètre de puissance dans l'allométrie longueur-poids ($\\beta$)",
                                  "Power parameter in length-weight allometry ($\\beta$)"),
                           ifelse(fr(),
                                  "Âge à 50 ans ($\\dot{a}$)",
                                  "Age at 50\\% maturity ($\\dot{a}$)"),
                           ifelse(fr(),
                                  "SD à 50\\% de maturité ($\\dot{\\gamma}$)",
                                  "SD at 50\\% maturity ($\\dot{\\gamma}$)")),
                         name = NULL) |>
    rename(Parameter = value)

  d <- model$dat
  params <- bind_cols(param_names,
                      map_dfr(list(a = vec2df(d$linf) |> setNames(c("Female", "Male")),
                                   b = vec2df(d$k) |> setNames(c("Female", "Male")),
                                   c = vec2df(d$to) |> setNames(c("Female", "Male")),
                                   d = vec2df(d$lw.alpha) |> setNames(c("Female", "Male")),
                                   e = vec2df(d$lw.beta) |> setNames(c("Female", "Male")),
                                   f = vec2df(d$age.at.50.mat) |> setNames(c("Female", "Male")),
                                   g = vec2df(d$sd.at.50.mat) |> setNames(c("Female", "Male"))),
                              ~{.x}))

  rounded_vals <- pmap(params, ~{
    mtch <- grepl("alpha", ..1)
    if(mtch){
      return(c(f(..2, 7), f(..3, alpha_digits)))
    }
    c(f(..2, digits), f(..3, digits))
  }) |>
    setNames(1:nrow(params)) |>
    map_dfr(~{.x}) |>
    t() |>
    as_tibble()

  params[, 2:3] <- rounded_vals

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
