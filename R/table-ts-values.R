#' Create a table of time series estimates for MCMC iSCAM models
#'
#' @description
#' Make a table of medians and/or credible intervals for values such as
#' biomass, recruitment, depletion, and fishing mortality
#'
#' @inheritParams table_param_est_mcmc
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param model_desc A description for the models to be shown in multicolumn
#' headings in the table (if more than one model only). If `NULL`, names will
#' be created (Model 1, Model 2, etc.)
#' @param value An output value to produce the table for. Can be
#' 'sbt' (spawning biomass), 'rt' (recruitment), 'ft' (fishing mortality),
#' 'ut' (exploitation rate), or 'depl' (depletion)
#' @param start_yr Year to start the table with. If `NULL`, all years will
#' be included
#'
#' @return A [csasdown::csas_table()]
#' @export
table_ts_values_mcmc <- function(models,
                                 model_desc = NULL,
                                 value = c("sbt", "rt", "ft", "ut", "depl"),
                                 start_yr = NULL,
                                 digits = 2,
                                 probs = c(0.025, 0.5, 0.975),
                                 model_col_widths = NULL,
                                 bold_header = TRUE,
                                 ...){

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
    stop("`probs` has length ", length(probs), " but must be a vector ",
         "of three values ",
         "representing lower CI, median, and upper CI",
         call. = FALSE)
  }
  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  val_nm <- paste0(value, "_quants")

  model_val_df <- imap(models, function(model, model_ind){
    val_lst <- model$mcmccalcs[[val_nm]]
    if(!"list" %in% class(val_lst)){
      val_lst <- list(val_lst)
    }

    val_df <- imap(val_lst, function(val_gear, val_gear_ind){
      val_gear <- val_gear |>
        t() |>
        as_tibble(rownames = "year")

      if(val_gear_ind != 1){
        # Only the first data frame retains year because they are all column-
        # bound after and we only want one year column
        val_gear <- val_gear |>
          select(-year)
      }

      # Guarantee the probs values are present in the output quant value tables
      walk(prob_cols, function(prob){
        mtch <- any(grepl(prob, names(val_gear)))
        if(!mtch){
          stop("One of the values in `probs` does not appear in the MCMC ",
               "output data\n",
               prob, call. = FALSE)
        }
      })

      val_gear |>
        select(-MPD) |>
        mutate(Median = f(!!sym(prob_cols[2]), digits),
               `Credible interval` = paste0(trimws(f(!!sym(prob_cols[1]), digits)),
                                            "--",
                                            trimws(f(!!sym(prob_cols[3]), digits)))) |>
        select(-c(!!sym(prob_cols[1]), !!sym(prob_cols[2]), !!sym(prob_cols[3])))
    }) |>
      map_dfc(~{.x})

    if(model_ind != 1){
      # Only the first data frame retains year because they are all column-
      # bound after and we only want one year column
      val_df <- val_df |>
        select(-year)
    }
    val_df
  }) |>
  map_dfc(~{.x})

  if(!"list" %in% class(models[[1]]$mcmccalcs[[val_nm]])){
    len_gears <- 1
  }else{
    len_gears <- length(models[[1]]$mcmccalcs[[val_nm]])
  }

  rep_num <- length(models) * len_gears
  names(model_val_df) <- c(tr("Year"),
                           rep(c(tr("Median"),
                                 tr("Credible interval")),
                               rep_num))

  tab <- model_val_df

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  yr_sym <- sym(tr("Year"))
  if(!is.null(start_yr)){
    yrs <- tab[tr("Year")] |> unlist() |> as.numeric()
    if(!start_yr %in% yrs){
      stop("`start_yr` not in the range of years in the model output values",
           call. = FALSE)
    }
    tab <- tab |>
      filter(!!yr_sym >= start_yr)

    if(fr()){
      col <- sym("Ann\u00E9e")
    }else{
      col <- sym("Year")
    }
    tab <- tab |>
      filter(!!col >= start_yr)
  }

  if(value == "ut"){
    tab[[yr_sym]] <- as.character(as.numeric(tab[[yr_sym]]) + 1)
    tab <- tab[-nrow(tab) , ]
  }

  out <- csas_table(tab,
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    bold_header = bold_header,
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(len_gears > 1){
    gear_names <- models[[1]]$dat$fleet_gear_names
    if(length(gear_names) != len_gears){
      stop("The list of fleet names at the beginning of the iSCAM dat file ",
           "is not the same length as the number of gears found for the ",
           "value '", value, "' in the model", call. = FALSE)
    }
    gear_header_vec <- c(" " = 1)
    for(i in seq_along(models)){
      for(j in seq_len(len_gears)){
        header <- 2
        if(value == "ft"){
          names(header) <- paste0("$F_{", gear_names[j], "}$")
        }else if(value == "ut"){
          names(header) <- paste0("$U_{", gear_names[j], "}$")
        }
        gear_header_vec <- c(gear_header_vec, header)
      }
    }
    if(bold_header){
      names(gear_header_vec) <- gsub("^\\$", "$\\\\mathbf{", names(gear_header_vec))
      names(gear_header_vec) <- gsub("\\$$", "}$", names(gear_header_vec))
    }

    out <- out |>
      add_header_above(header = gear_header_vec, escape = FALSE)
  }

  if(length(models) > 1){
    model_header_vec <- c(" " = 1)
    for(i in seq_along(models)){
      header <- 2 * len_gears
      if(is.null(model_desc)){
        names(header) <- paste0("Model ", i)
      }else{
        names(header) <- model_desc[i]
      }
      model_header_vec <- c(model_header_vec, header)
    }
    out <- out |>
      add_header_above(header = model_header_vec, escape = FALSE)
  }

  if(!is.null(model_col_widths)){
    out <- out |>
      column_spec(2:ncol(tab), width = model_col_widths)
  }

  out
}

