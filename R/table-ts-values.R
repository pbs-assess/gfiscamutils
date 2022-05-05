#' Create a table of time series estimates for MCMC iSCAM models
#'
#' @description
#' Make a table of medians and/or credible intervals for values such as
#' biomass, recruitment, depletion, and fishing mortality
#'
#' @inheritParams table_param_est_mcmc
#' @param value An output value to produce the table for. Can be
#' 'sbt' (spawning biomass), 'rt' (recruitment), 'ft' (fishing mortality),
#' 'ut' (exploitation rate), or 'depl' (depletion)
#' @param start_yr Year to start the table with. If `NULL`, all years will
#' be included
#'
#' @return A [csasdown::csas_table()]
#' @export
table_ts_values_mcmc <- function(models,
                                 value = c("sbt", "rt", "ft", "ut", "depl"),
                                 type = c("median", "ci"),
                                 start_yr = NULL,
                                 digits = 2,
                                 probs = c(0.025, 0.5, 0.975),
                                 model_col_widths = "5em",
                                 ...){

  type <- match.arg(type)
  value <- match.arg(value)

  if(is_iscam_model(models)){
    models <- list(models)
    class(models) <- mdl_lst_cls
  }

  if(!is_iscam_model_list(models)){
    stop("`models` does not have class `gfiscamutils::mdl_lst_cls`.",
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI",
         call. = FALSE)
  }
  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  val_nm <- paste0(value, "_quants")

  tab_lst <- imap(models, ~{
    j <- .x$mcmccalcs[[val_nm]] %>%
      t() %>%
      as_tibble(rownames = "year")

    quants <- imap_chr(prob_cols, ~{
      mtch <- grep(.x, names(j), value = TRUE)
      if(!length(mtch)){
        stop("One of the values in `probs` does not appear in the MCMC output data\n",
             .x, call. = FALSE)
      }
      mtch
    })

    median_df <<- j %>%
      mutate(val = f(!!sym(quants[2]), digits)) %>%
      select(year, val)
    ci_df <<- j %>%
      mutate(val = paste0(trimws(f(!!sym(quants[1]), digits)),
                          "-",
                          trimws(f(!!sym(quants[3]), digits)))) %>%
      select(year, val)

    if(type == "median" || length(models) == 1){
      out <- median_df
    }else{
      out <- ci_df
    }
    out
  })

  # Make sure all data frames have the same number of rows
  if(length(models) > 1 && var(tab_lst %>% map_dbl(~{nrow(.x)})) !=0){
    stop("The input models do not have the same number of years in the ",
         "$mcmccalcs$rt_quants data frames", call. = FALSE)
  }

  # Left join them all, one by one
  tab <- tab_lst[[1]]
  if(length(models) > 1){
    for(i in 2:length(tab_lst)){
      tab <- full_join(tab, tab_lst[[i]], by = "year")
    }
  }
  if(length(models) == 1){
    # Add the credible interval column
    tab <- full_join(tab, ci_df, by = "year")
    if(fr()){
      names(tab) <- c("Année", "Médiane", "Intervalle crédible")
    }else{
      names(tab) <- c("Year", "Median", "Credible interval")
    }
  }else{
    names(tab) <- c(ifelse(fr(), "Année", "Year"), names(models))
  }

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  if(!is.null(start_yr)){
    yrs <- unique(tab$year)
    if(!start_yr %in% yrs){
      stop("`start_yr` not in the range of years in the model output values",
           call. = FALSE)
    }
    tab <- tab %>%
      filter(year >= start_yr)
  }

  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(!is.null(model_col_widths)){
    out <- out %>%
      column_spec(2:ncol(tab), width = model_col_widths)
  }

  out
}

