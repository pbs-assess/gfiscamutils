#' Make a table of reference points for MCMC iSCAM models
#'
#' @description
#' Make a table of reference points for MCMC iSCAM models
#'
#' @inheritParams table_param_est_mcmc
#'
#' @return A [csasdown::csas_table()]
#' @export
table_ref_points_mcmc <- function(models,
                                  type = c("median", "ci"),
                                  digits = 2,
                                  probs = c(0.025, 0.5, 0.975),
                                  model_col_widths = "5em",
                                  ...){

  type <- match.arg(type)

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

  tab_lst <- imap(models, ~{
    j <- .x$mcmccalcs$rt_quants %>%
      t() %>%
      as_tibble(rownames = "year") %>%
      select(-MPD) %>%
      mutate(year = as.numeric(year))

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
      names(tab) <- c("Point de référence", "Médiane", "Intervalle crédible")
    }else{
      names(tab) <- c("Reference point", "Median", "Credible interval")
    }
  }else{
    names(tab)[-1] <- names(models)
  }

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

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

