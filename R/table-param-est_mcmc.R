#' Create a table of parameter estimates for iSCAM models
#'
#' @description
#' Create a table parameter estimates and priors for iSCAM models
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param type Use 'median' to show median parameter values or 'ci' for a
#' dash-separated range for the credible interval
#' @param digits Number of decimal places for the values in the table
#' @param probs A 3-element vector of probabilities that appear in the output
#' data frames. This is provided in case the data frames have more than three
#' different quantile levels
#' @param model_col_widths Widths for columns, except the Parameter column
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_param_est_mcmc <- function(models,
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

  param_col_name <- ifelse(fr(), "Paramètre", "Parameter")
  val_col_name <- ifelse(fr(), "Valeur", "Value")

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  tab_lst <- imap(models, ~{
    j <- .x$mcmccalcs$params_quants %>%
      t() %>%
      as_tibble(rownames = "param")

    quants <- imap_chr(prob_cols, ~{
      mtch <- grep(.x, names(j), value = TRUE)
      if(!length(mtch)){
        stop("One of the values in `probs` does not appear in the MCMC output data\n",
             .x, call. = FALSE)
      }
      mtch
    })

    change_param_names <- function(df){
      # Change q's to have names instead of numbers because each model has different
      # numbers for names
      q_real_nms <- .x$dat$index_gear_names
      q_inds <- grep("^q_gear", df$param)
      q_param_nms <- df[q_inds, "param"] %>% pull()
      df[q_inds, "param"] <- paste0("q_{",
                                    q_real_nms[as.numeric(gsub("q_gear([0-9]+)",
                                                               "\\1", q_param_nms))],
                                    "}")
      # Change sel's to have names instead of numbers because each model has different
      # numbers for names
      sel_real_nms <- .x$dat$gear_names
      sel_inds <- grep("^sel_", df$param)
      sel_param_nms <- j[sel_inds, "param"] %>% pull()
      pat <- "sel_(age|sd)50_(male|female)_gear([0-9]+)"
      age_sd <- gsub(pat, "\\1", sel_param_nms)
      sex <- gsub(pat, "\\2", sel_param_nms)
      gear <- sel_real_nms[as.numeric(gsub(pat, "\\3", sel_param_nms))]
      gear <- gear[!is.na(gear)]
      df[sel_inds, "param"] <- paste0("sel_", age_sd, "_", sex, "_{", gear, "}")
      df
    }

    median_df <<- j %>%
      mutate(val = f(!!sym(quants[2]), digits)) %>%
      select(param, val) %>%
      change_param_names()
    ci_df <<- j %>%
      mutate(val = paste0(trimws(f(!!sym(quants[1]), digits)),
                          "-",
                          trimws(f(!!sym(quants[3]), digits)))) %>%
      select(param, val) %>%
      change_param_names()

    if(type == "median" || length(models) == 1){
      out <- median_df
    }else{
      out <- ci_df
    }
    out
  })

  # Fetch vector of all parameter names used in all models
  param_names <- tab_lst %>%
    map(~{
      .x$param
    }) %>%
    flatten %>%
    unique %>%
    map_chr(~{.x})
  # Sort the param names, leading params first, then q's then sel's
  params_q <- grepl("^q_", param_names)
  params_sel <- grepl("^sel_", param_names)
  params_lead <- param_names[!(params_q | params_sel)]
  params_q <- sort(param_names[params_q])
  params_sel <- sort(param_names[params_sel])
  tab <- c(params_lead, params_q, params_sel) %>%
    enframe(name = NULL, value = "param")

  # Left join them all, one by one
  for(i in tab_lst){
    tab <- full_join(tab, i, by = "param")
  }
  # Remove some values (non-estimated parameters of calculated values)
  remove_pat <- "(^s?bo$)|^bmsy$|(^msy_)|(^fmsy)|(^umsy)|^SSB$|^f$"
  tab <- tab %>%
    filter(!grepl(remove_pat, param))
  median_df <- median_df %>%
    filter(!grepl(remove_pat, param))
  ci_df <- ci_df %>%
    filter(!grepl(remove_pat, param))

  if(length(models) == 1){
    # Add the credible interval column
    tab <- full_join(tab, ci_df, by = "param") %>%
      mutate(param = get_fancy_names(param))
    if(fr()){
      names(tab) <- c(param_col_name, "Médiane", "Intervalle crédible")
    }else{
      names(tab) <- c(param_col_name, "Median", "Credible interval")
    }
  }else{
    # Convert parameter names to fancy names for typesetting
    tab <- tab %>%
      mutate(param = get_fancy_names(param)) %>%
      rename(!!sym(param_col_name) := param)
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
