#' Create a decision table for iSCAM models
#'
#' @description
#' Produce a decision table for the given iscam model
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param format See `format` in [knitr::kable()]
#' @param bo_refpts Vector of proportional values for the reference
#' points
#' @param num_proj_yrs The number of projection years to show in the table.
#' The output may have more but only this many will be shown. If `NULL`,
#' all projection years will be shown
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
                            bo_refpts = c(0.2, 0.35, 0.4),
                            num_proj_yrs = 3,
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

  esc_open_prob <- ifelse(format == "html", "\\(P(", "P(")
  esc_bo <- ifelse(format == "html", "B_{0}", "B\\textsubscript{0}")
  esc_open_b <- ifelse(format == "html", "B_{", "B\\textsubscript{")
  esc_close_b <- "}"
  esc_close_prob <- ifelse(format == "html", "\\)", ")")

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

  # Filter out the columns `catch` and the years we need for the table calcs
  # Make all biomass column names numeric (remove the preceeding 'B')
  # Calculate depletion for all biomass columns
  get_proj <- function(first_yr){
    proj <- model$mcmccalcs$proj |>
      imap(~{
        if(is.null(num_proj_yrs)){
          cols_to_keep <- "^catch$|^B20[0-9]+$"
          cols_to_keep_inds <- grep(cols_to_keep, names(.x))
          if(!length(cols_to_keep_inds)){
            stop("The regular expression '", cols_to_keep, "' failed to match any column names ",
                 "in the data frame for catch level ", .y)
          }
        }else{
          cols_to_keep <- paste0("^catch$|",
                                 paste0("^B",
                                        first_yr:(model$dat$end.yr + num_proj_yrs + 2),
                                        "$",
                                        collapse = "|"))
          cols_to_keep_inds <- grep(cols_to_keep, names(.x))
          if(!length(cols_to_keep_inds)){
            stop("The regular expression '", cols_to_keep, "' failed to match any column names ",
                 "in the data frame for catch level ", .y)
          }
        }
        output <- .x |> select(cols_to_keep_inds)
        names(output) <- gsub("^B", "", names(output))
        # Calculate depletion
        output |> select(-catch) |> mutate_all(~{.x / sbo})
      })
  }

  proj <- get_proj(model$dat$end.yr + 2)
  # Calculate all probability columns for reference points
  ptab <- map(bo_refpts, function(refpt){
    j <- imap(proj, function(proj_tbl, catch_lvl){
      # For each projection table at a given catch level ...
      cols <- proj_tbl |>
        map_dbl(~{
          sum(.x < refpt) / length(.x)
        })
      names(cols) <- names(cols) |>
        map_chr(~{
          paste0(esc_open_prob, esc_open_b, .x, "} < ",
                 refpt, esc_bo, esc_close_prob)
        })
      cols |>
        vec2df(nms = names(cols))
    }) |>
      map_dfr(~{.x})
  }) |>
    map_dfc(~{.x})

  proj <- get_proj(model$dat$end.yr + 1)
  # Calculate probability columns for dropping biomass year-to-year
  j <- map(proj, function(proj_tbl){
    # For each projection table at a given catch level ...
    k <- proj_tbl |>
      map2(seq_along(proj_tbl), ~{
      if(.y > 1){
        sum(.x < proj_tbl[[.y - 1]]) / length(.x)
      }else{
        NULL
      }
    })
    k[lengths(k) == 0] <- NULL
    k <- k |> map_dbl(~{.x})
    names(k) <- map(as.numeric(names(k)), ~{
      paste0(esc_open_prob, esc_open_b, .x, "} < ",
             esc_open_b, .x - 1, esc_close_b, esc_close_prob)
    })
    k
  }) |>
    map_dfr(~{.x})

  tab <- bind_cols(ptab, j) |>
    mutate(catch = as.numeric(names(proj))) |>
    select(catch, everything())
  names(tab)[1] <- tr("Catch (thousand t)")

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
