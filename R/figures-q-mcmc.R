#' Plot catchability plots for MCMC output with credible intervals
#'
#' @details
#' This function can plot either multiple gears for a single model on one plot,
#' or a single gear from multiple models on a single plot. Errors will be
#' thrown if trying to plot multiple gears and multiple models together. To
#' do that, call [plot_q_grid_mcmc()] instead.
#'
#' @rdname plot_ts_mcmc
#' @family Time series plotting functions
#'
#' @param gear The number of the gear to plot
#' @export
plot_q_mcmc <- function(models,
                        gear = 1,
                        probs = c(0.025, 0.5, 0.975),
                        base_color = "black",
                        palette = "Paired",
                        ci_type = c("both", "line", "ribbon"),
                        ci_linetype =  c("dotted", "solid",
                                         "dashed", "dotdash",
                                         "longdash", "twodash"),
                        ci_color = "red",
                        ci_alpha = 0.3,
                        text_title_size = 12,
                        angle_x_labels = FALSE){

  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

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

  max_num_gears <- map_dbl(models, ~{
    length(.x$mpd$a_obs)
  }) %>%
    max()
  if(any(gear < 1) || any(gear > max_num_gears)){
    stop("all `gear` values must be between 1 and ", max_num_gears,
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  surv_name_lst <- map(models, ~{
    .x$dat$index_gear_names
  })
  surv_names <- surv_name_lst %>%
    flatten() %>%
    map_chr(~{.x}) %>%
    unique()

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

  browser()
  q_quants <- imap(models, ~{
    j <- .x$mcmccalcs$q_quants %>%
      t() %>%
      as_tibble(rownames = "gear") %>%
      mutate(model = .y) %>%
      select(-MPD)
  }) %>%
    bind_rows %>%
    select(model, everything()) %>%
    mutate(model = as.factor(model)) %>%
    mutate(gear = as.factor(gear)) %>%
    mutate(model = fct_relevel(model, names(models)))

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
  g <- q_quants %>%
    ggplot(aes(x = model,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab(x_label) +
    ylab(y_label) +
    scale_color_manual(values = model_colors) +
    theme(axis.text.x = element_blank())

  g <- g +
    geom_pointrange(aes(color = model)) +
    facet_wrap(~gear)
  #browser()

  g
}
