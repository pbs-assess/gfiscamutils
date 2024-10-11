#' Plot MCMC catchabilities for iSCAM models
#'
#' @description
#' Plot catchability plots for MCMC output with credible intervals
#'
#' @details
#' This function can plot either multiple gears for a single model on one plot,
#' or a single gear from multiple models on a single plot.
#'
#' @inheritParams plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param text_title_size Size of text for the axis titles
#' @param gear The number of the gear to plot
#' @export
plot_q_mcmc <- function(models,
                        gear = 1,
                        append_base_txt = NULL,
                        legend_title = tr("Models"),
                        probs = c(0.025, 0.5, 0.975),
                        base_color = "black",
                        palette = iscam_palette,
                        leg_loc = c(1, 1),
                        text_title_size = 12,
                        angle_x_labels = FALSE){

  single_model <- FALSE
  if(is_iscam_model(models)){
    single_model <- TRUE
    model_desc <- attributes(models)$model_desc
    models <- list(models)
    class(models) <- mdl_lst_cls
    names(models) <- model_desc
  }

  if(!is_iscam_model_list(models)){
    stop("`models` is not an iscam model list object",
         call. = FALSE)
  }

  if(!palette %in% rownames(brewer.pal.info)){
    stop("`palette` name not found in `RColorBrewer::brewer.pal.info`",
         call. = FALSE)
  }

  palette_info <- brewer.pal.info[rownames(brewer.pal.info) == palette, ]

  if(length(models) > palette_info$maxcolors){
    stop("Cannot plot more than ", palette_info$maxcolors, " models because that is the ",
         "maximum number for the ", palette, " palette",
         call. = FALSE)
  }

  if(!is.null(append_base_txt)){
    length(append_base_txt) <- length(models)
    # If `append_base_txt` is shorter than the number of models, append empty strings
    # for remainder of items
    append_base_txt[which(is.na(append_base_txt))] <- ""
  }

  # Set up model names for the legend/title
  names(models) <- map_chr(models, ~{
    as.character(attributes(.x)$model_desc)
  })
  names(models) <- paste0(names(models), append_base_txt)

  # surv_abbrev will be in order of the gears in the models
  surv_abbrev_lst <- map(models, ~{
    .x$dat$index_abbrevs
  })
  surv_name_lst <- map(models, ~{
    .x$dat$index_gear_names
  })

  surv_abbrevs <- surv_abbrev_lst |>
    flatten() |>
    map_chr(~{.x}) |>
    unique()
  surv_names <- surv_name_lst |>
    flatten() |>
    map_chr(~{.x}) |>
    unique()

  if(length(surv_names) != length(surv_abbrevs)){
    stop("The total number of unique 'IndexGears' and 'IndexAbbrevs' defined in the data files ",
         "for the `models` do not match. It is likely you defined some of these slightly ",
         "differently in different data files. They must match exactly.")
  }

  if(!is.null(gear)){
    valid_gear_nums <- seq_along(surv_names)
    if(!all(gear %in% valid_gear_nums)){
      stop("One or more of the gear numbers you requested is outside the range of possible gears.\n",
           "Available gears numbers are: ", paste(valid_gear_nums, collapse = ", "), "\n",
           "Names for these are:\n", paste(surv_names, collapse = "\n"))
    }
    surv_abbrevs <- surv_abbrevs[gear]
    surv_names <- surv_names[gear]
  }

  # Add survey names to the table with a left join by survey_abbrev
  surv_abbrevs_df <- surv_abbrevs |>
    enframe(name = NULL)
  surv_names_df <- surv_names |>
    enframe(name = NULL)
  surv_df <- surv_abbrevs_df |>
    cbind(surv_names_df) |>
    `names<-`(c("survey_abbrev", "survey_name"))

  q_quants <- imap(models, ~{
    j <- .x$mcmccalcs$q_quants |>
      t() |>
      as_tibble(rownames = "gear") |>
      mutate(model = .y) |>
      select(-MPD)
  }) |>
    bind_rows() |>
    select(model, everything()) |>
    mutate(model = as.factor(model)) |>
    rename(survey_abbrev = gear) |>
    mutate(model = fct_relevel(model, names(models))) |>
    mutate(survey_abbrev = tr(survey_abbrev)) |>
    convert_prob_cols_language()

  q_quants <- q_quants |>
    left_join(surv_df, by = "survey_abbrev") |>
    select(-survey_abbrev) |>
    mutate(survey_name = factor(survey_name)) |>
    rename(gear = survey_name) |>
    select(model, gear, everything()) |>
    mutate(gear = fct_relevel(gear,  surv_names))

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(q_quants), value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data\n",
           .x, call. = FALSE)
    }
    mtch
  })

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color,
                    brewer.pal(name = palette,
                               n = palette_info$maxcolors))

  x_label <- ""
  y_label <- ""
  g <- q_quants |>
    ggplot(aes(x = model,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab(x_label) +
    ylab(y_label) +
    scale_color_manual(values = model_colors) +
    guides(color = guide_legend(title = legend_title)) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    scale_color_manual(values = model_colors,
                       labels = map(models, ~{tex(as.character(attributes(.x)$model_desc))}))


  g <- g +
    geom_pointrange(aes(color = model)) +
    facet_wrap(~gear, scales = "free")

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
    if(single_model){
      if(!is.null(text_title_size)){
        g <- g + ggtitle(tex(names(models))) +
          theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
      }
    }
  }else if(leg_loc[1] == "facet"){
    g <- g |> move_legend_to_empty_facet()
  }else{
    g <- g +
      theme(legend.justification = leg_loc,
            legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "white")) +
      labs(color = legend_title)
  }

  g
}
