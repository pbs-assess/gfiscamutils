#' Create a decision table for iSCAM models
#'
#' @description
#' Produce a decision table for the given iscam model
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param format See `format` in [knitr::kable()]
#' @param bo_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.2B0 and 0.4B0 by default
#' @param digits Number of decimal places for the values in the table
#' @param ret_df Logical. If `TRUE` return a data frame with the values,
#' instead of the [knitr::kable()] formatted table
#' @param col_widths Widths of columns, see [kableExtra::column_spec()]
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
#' @importFrom gfutilities f
table_decisions <- function(model,
                            format = c("latex", "html"),
                            bo_refpts = c(0.2, 0.4),
                            digits = 2,
                            ret_df = FALSE,
                            col_widths = NULL,
                            ...){

  format <- match.arg(format)

  if(!mdl_cls %in% class(model)){
    if(mdl_lst_cls %in% class(model) && length(model) == 1){
      # An iSCAM model which happens to be in a list by itself
      model <- model[[1]]
      if(!mdl_cls %in% class(model)){
        stop("`model` has class `gfiscamutils::mdl_lst_cls` class and ",
             "contains one element but that element does not have class ",
             "`gfiscamutils::mdl_cls`.",
             call. = FALSE)
      }
    }else{
      stop("`model` does not have class `gfiscamutils::mdl_cls`.",
           call. = FALSE)
    }
  }

  ctl_options <- model$proj$ctl.options %>%
    as_tibble(rownames = "variable") %>%
    rename(value = V1)
  start_yr <- ctl_options %>%
    filter(variable == "syrmeanm") %>%
    pull(value)
  end_yr <- ctl_options %>%
    filter(variable == "nyrmeanm") %>%
    pull(value)
  end_yr_plus1 <- end_yr + 1

  sbo <- model$mcmccalcs$params$sbo
  sbo_refs <- map(bo_refpts, ~{
    sbo * .x
  }) |>
    setNames(paste0(bo_refpts, "_bo"))

  proj <- model$mcmccalcs$proj
  proj_yrs_inds <- grep("^B20[0-9]+$", names(proj[[1]]))
  proj_yrs_minus_first_inds <- proj_yrs_inds[-1]

  esc_open_prob <- ifelse(format == "html", "\\(P(", "P(")
  esc_bo <- ifelse(format == "html", "B_{0}", "B\\textsubscript{0}")
  esc_open_b <- ifelse(format == "html", "B_{", "B\\textsubscript{")
  esc_close_b <- "}"
  esc_close_prob <- ifelse(format == "html", "\\)", ")")

  tab <- imap(proj, ~{
    # Make all columns numeric
    x_tbl <- .x |>
      as_tibble() |>
      map_df(~{as.numeric(.x)})

    # Calculate depletion for projected years
    x <- x_tbl[proj_yrs_minus_first_inds] |> mutate_all(~{.x / sbo})
    names(x) <- gsub("B", "", names(x))

    i <- x |>
      map_dbl(~{sum(.x < bo_refpts[1]) / length(.x)})
    j <- x |>
      map_dbl(~{sum(.x < bo_refpts[2]) / length(.x)})
    # Calculate probability of decline for projected years

    y <- x_tbl[proj_yrs_inds]
    names(y) <- gsub("B", "", names(y))
    k <- map2(y, seq_along(y), ~{
      if(.y > 1){
        sum(.x < y[[.y - 1]]) / length(.x)
      }else{
        NULL
      }
    })
    k[lengths(k) == 0] <- NULL
    k <- k |> map_dbl(~{.x})

    nms_fancy <- names(i) |>
      map_chr(~{
        paste0(esc_open_prob, esc_open_b, .x, "} < ",
               bo_refpts[1], esc_bo, esc_close_prob)
      })
    names(i) <- nms_fancy

    nms_fancy <- names(j) |>
      map_chr(~{
        paste0(esc_open_prob, esc_open_b, .x, "} < ",
               bo_refpts[2], esc_bo, esc_close_prob)
      })
    names(j) <- nms_fancy

    nms_fancy <- map(as.numeric(names(k)), ~{
        paste0(esc_open_prob, esc_open_b, .x, "} < ",
               esc_open_b, .x - 1, esc_close_b, esc_close_prob)
      })
    names(k) <- nms_fancy

    out <- c(.x$catch[1], i, j, k)
    names(out)[1] <- "Catch (thousand t)"
    out
  }) |>
    map_df(~{.x})

  if(ret_df){
    return(tab)
  }

  out <- csas_table(tab,
                    format = format,
                    bold_header = FALSE,
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol("tab")),
                    ...)

  if(!is.null(col_widths)){
    out <- out |>
      column_spec(1:ncol(tab), width = col_widths)
  }

  out

}
