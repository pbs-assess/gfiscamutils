#' Create table of the growth parameters for input into the ISCAM model
#'
#' @details
#' iSCAM takes these values as inputs in the DAT file and creates a vector
#' of maturities-at-age by calling `plogis(a50, sd50)` where `a50` and `sd50`
#' are the outputs of this function, `sd50` is actually the shape parameter,
#' not a true standard deviation of values.
#'
#' The maturity code table for maturity_convention_code 4 (flatfish):
#' MATURITY_CODE,SPECIMEN_SEX_CODE,MATURITY_NAME,MATURITY_DESC
#' 1, 1, IMMATURE, "TESTES VERY SMALL, STRING-LIKE AND SOMEWHAT TRANSLUCENT OR PINKISH IN COLOUR"
#' 1, 2, IMMATURE, "OVARIES VERY SMALL, TRANSLUCENT OR PINKISH  AND SOMEWHAT GELATINOUS IN TEXTURE"
#' 2, 1, MATURING, "TESTES ENLARGING, A DISTINCT BULGE EVIDENT BUT STILL TRANSLUCENT OR PINKISH IN COLOUR"
#' 2, 2, MATURING, "OVARIES RELATIVELY SMALL, PINKISH-YELLOW OR CREAM IN COLOUR, GRANULAR IN TEXTURE.  NO DISTINCT EGGS VISIBLE"
#' 3, 1, DEVELOPING, "TESTES ENLARGING, BROWN-WHITE OR WHITE IN COLOUR, FIRM IN TEXTURE"
#' 3, 2, DEVELOPING, "OVARIES LARGE, CREAM OR YELLOW IN COLOUR CONTAINING OPAQUE EGGS THAT CAN BE DISTINGUISED BY DIRECT OBSERVATION.  SEX MAY BE DETERMINED EXTERNALLY"
#' 4, 1, RIPE, "TESTES LARGE, WHITE AND EASILY BROKEN. NO SPERM EVIDENT"
#' 4, 2, GRAVID, "OVARIES CONTAINING PARTLY OR WHOLLY TRANSLUCENT EGGS.  SEX EASILY DETERMINED EXTERNALLY"
#' 5, 1, SPAWNING, "TESTES LARGE, WHITE AND SPERM EVIDENT"
#' 5, 2, RIPE, "OVARIES CONTAINING ENTIRELY TRANSLUCENT, MATURE OVA.  EGGS LOOSE AND WILL RUN FROM OVIDUCTS UNDER SLIGHT PRESSURE"
#' 6, 1, SPENT, "TESTES FLACCID, SHRUNKEN AND YELLOW-BROWN IN COLOUR. SPERM DUCTS ENLARGED AND A SMALL AMOUNT OF SPRM MAY BE PRESENT"
#' 6, 2, SPENT, "OVARIES LARGE, FLACCID AND PURPLE IN COLOUR; A FEW TRANSLUCENT EGGS MAY BE LEFT.  OVARIAN MEMBRANE VERY VASCULAR (BLOODSHOT) AND SAC-LIKE"
#' 7, 1, RESTING, "TESTES FIRM, SMALL AND YELLOW-BROWN IN COLOUR.  SPERM DUCTS SMALL"
#' 7, 2, RESTING, "OVARIES CONTRACTED AND FIRM, PINKISH GREY TO CREAM-YELLOW IN COLOUR AND MAY APPEAR GRANULAR IN TEXTURE BUT NO DISTINCT EGGS ARE VISIBLE"
#'
#' @param surv_samples A `survey_samples` list as output by
#' [gfdata::get_survey_samples()]
#' @param col_widths Widths for columns, except the Parameter column
#' the [csasdown::csas_table()]
#' @param ret_df Logical. If `TRUE`, return the [data.frame] and not
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return Either a [data.frame] or a [csasdown::csas_table()], depending on
#' the value of `return_df`
#' @export
table_growth_params <- function(surv_samples = NULL,
                                col_widths = NULL,
                                ret_df = FALSE,
                                digits = 2,
                                ...){

  if(is.null(surv_samples)){
    stop("`surv_samples` must not be `NULL`", call. = FALSE)
  }

  params <- export_mat_lw_age(surv_samples, write_file = FALSE, ...) |>
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
