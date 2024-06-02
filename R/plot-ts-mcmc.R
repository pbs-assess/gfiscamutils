#' Plot MCMC time series plots for iSCAM models
#'
#' @description
#' Plot time series plots for MCMC output with credible intervals for single or
#' multiple models
#'
#' @details
#' Plot a single model or multiple models overlaid for comparison.
#' If a single model, it must have a class attribute of `mdl_cls`.
#' A list of multiple models must have a class attribute of `mdl_lst_cls` and
#' each model in the list must have a class attribute of `mdl_cls`.
#'
#' The model description text will appear in a legend if `models` length
#' is greater than 1, and as a title if there is only one model. The
#' description text is found in the `model_desc` attribute of a
#' `mdl_cls` class ("iscam_model") object. The attribute is set in the
#' [model_setup()] function, from either the `bridge_model_text` or
#' `sens_model_text` arguments.
#'
#' @family Time series plotting functions
#'
#' @param models A list of iscam model objects (class [mdl_lst_cls])
#' @param quant_df A name of a value which can be found in the
#' `model$mcmccalcs` list for each iscam model in the `models` list
#' @param facet_wrap_var A column in the `quant_df` data frame which will be
#' used to split the output into separate panel plots. If "none", no faceting
#' will be done
#' @param palette A palette value that is in [RColorBrewer::brewer.pal.info]
#' @param all_one_color If not `NULL`, overrides `palette` and the color that
#' this is defined as will be used for all lines on the plot (all models)
#' @param base_color A color to prepend to the brewer colors which are set by
#' `palette`. This is called `base_color` because it is likely to be a base
#' model
#' @param bo_dodge The amount to offset the initial value (B0 or R0) values
#' from each other so the values and uncertainty can be easily seen for
#' multiple models
#' @param x_space The amount of x-interval space to pad the left and right of
#' the plot with. To remove all padding, make this 0
#' @param y_space The amount of y-interval space to pad the top and bottom of
#' the plot with. To remove all padding, make this 0
#' @param append_base_txt A vector of strings to append to the model names for
#' display on the plot legend or title
#' @param alpha The transparency with values from 0 to 1 of the ribbon shading
#' when `first_model_ribbon` is `TRUE`
#' @param legend_title Title for legend
#' @param xlim The x limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param ylim The y limits for the plot. If `NULL`, the limits of the data
#' will be used
#' @param line_width Width of all median lines on the plot
#' @param point_size Point size for all median points on the plot
#' @param first_model_ribbon Logical. If `TRUE`, give the first model a shaded
#' credible interval instead of dotted lines
#' @param probs A 3-element vector of probabilities that appear in the output
#' data frames. This is provided in case the data frames have more than three
#' different quantile levels
#' @param leg_loc A two-element vector describing the X-Y values between 0 and
#' 1 to anchor the legend to. eg. c(1, 1) is the top right corner and c(0, 0)
#' is the bottom left corner
#' @param title_text_size Add the model description as a title with this font
#' size. The text comes from the `model_desc` attribute of `model`. If this is
#' `NULL`, don't show a title
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_ts_mcmc <- function(models,
                         quant_df = "sbt_quants",
                         facet_wrap_var = c("none", "sex", "gear"),
                         palette = iscam_palette,
                         all_one_color = NULL,
                         base_color = "black",
                         legend_title = tr("Models"),
                         x_label = tr("Year"),
                         y_label = tr("Spawning Biomass (thousand t)"),
                         append_base_txt = NULL,
                         x_space = 0.5,
                         y_space = 0,
                         xlim = NULL,
                         ylim = NULL,
                         line_width = 1,
                         point_size = 2,
                         first_model_ribbon = TRUE,
                         alpha = 0.2,
                         leg_loc = c(1, 1),
                         probs = c(0.025, 0.5, 0.975),
                         text_title_size = 12,
                         angle_x_labels = FALSE,
                         ...){

  facet_wrap_var <- match.arg(facet_wrap_var)

  single_model <- FALSE
  if(is_iscam_model(models)){
    single_model <- TRUE
    models <- list(models)
    class(models) <- mdl_lst_cls
  }

  if(!is_iscam_model_list(models)){
    stop("The `models` list is not a gfiscamutils::mdl_lst_cls class.",
         call. = FALSE)
  }

  if(!palette %in% rownames(brewer.pal.info)){
    stop("`palette` name not found in `RColorBrewer::brewer.pal.info`",
         call. = FALSE)
  }
  palette_info <- brewer.pal.info[rownames(brewer.pal.info) == palette, ]
  palette_func <- colorRampPalette(brewer.pal(palette_info$maxcolors, palette))
  palette_colors <- palette_func(n = length(models))
  if(!is.null(all_one_color)){
    palette_colors <- rep(all_one_color, length(models))
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI",
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
  # Set model names for plotting
  names(models) <- paste(names(models), append_base_txt) |>
    trimws()
  models <- imap(models, ~{
    attributes(.x)$model_desc <- .y
    .x
  })

  start_yr <- map_dbl(models, ~{
    .x$dat$start.yr + 1
    })  |>
    min()
  end_yr <- map_dbl(models, ~{
    # Use max in the data
    if(!is.list(.x$mcmccalcs[[quant_df]])){
      .x$mcmccalcs[[quant_df]] <- list(.x$mcmccalcs[[quant_df]])
    }
    map_dbl(.x$mcmccalcs[[quant_df]], function(df){
      max(df |> as_tibble() |> names() |> as.numeric(),
          .x$dat$end.yr + 1)
    }) |>
      max()
  }) |>
    max()
  if(is.na(end_yr)){
    end_yr <- models[[1]]$dat$end.yr
  }
  end_yr <- end_yr + 1
  if(is.null(xlim)){
    xlim <- c(start_yr, end_yr)
  }else{
    if(start_yr > xlim[1]){
      stop("Start year in xlim comes before the data start year (", start_yr, ")",
           call. = FALSE)
    }
    if(end_yr < xlim[2]){
      stop("End year in xlim comes after the data end year (", end_yr, ")",
           call. = FALSE)
    }
    start_yr <- xlim[1]
    end_yr <- xlim[2]
  }
  ts_len <- end_yr - start_yr + 1

  # Main time series values
  var_quants <- imap(models, ~{

    if(is.null(.x$mcmccalcs[[quant_df]])){
      stop("`$mcmccalcs[[", quant_df, "]]` is `NULL` for model:\n",
           "'", .y, "'\n",
           "All models in the `models` list must have this data frame present",
           call. = FALSE)
    }
    out <- .x$mcmccalcs[[quant_df]]
    fleet_names <- .x$dat$fleet_gear_names
    if("list" %in% class(out)){
      if(facet_wrap_var == "sex"){
        if(length(out) > 2){
          stop("Length of the `mcmccalcs` output list for ", quant_df,
               " for the model:\n", "'", .y, "'\n", "is greater than 2, ",
               "meaning more than two sexes or an error in the\n",
               "function used to build this list `calc_mcmc()`",
               call. = FALSE)
        }
      }else if(facet_wrap_var == "gear"){
        if(length(out) > length(fleet_names)){
          stop("Length of the `mcmccalcs` output list for ", quant_df,
               " for the model:\n", "'", .y, "'\n", "is greater than ",
               length(fleet_names), ", meaning more than ",
               length(fleet_names), " fleets or an error in the\n",
               "function used to build this list `calc_mcmc()`",
               call. = FALSE)
        }
      }

      out <- imap(out, function(.x, y = .y){
        # Make French prob names in caswe they are not already
        rownames(.x) <- gsub("\\.", ",", rownames(.x))
        .x |>
          t() |>
          as.data.frame() |>
          rownames_to_column(var = "year") |>
          mutate(model = .y) |>
          mutate(!!sym(facet_wrap_var) := ifelse(facet_wrap_var == "gear",
                                                 fleet_names[y],
                                                 ifelse(y == 1, "Male", "Female"))) |>
          select(model, year, everything()) |>
          mutate(year = as.numeric(year))
      }) |>
        bind_rows() |>
        as_tibble() |>
        mutate(!!sym(facet_wrap_var) := as.factor(!!sym(facet_wrap_var))) |>
        mutate(!!sym(facet_wrap_var) := fct_relevel(!!sym(facet_wrap_var),
                                                    ifelse(facet_wrap_var == "gear",
                                                           fleet_names,
                                                           c("Female", "Male"))))

    }else{
      # Data frame of non-split-sex output
      # Make French prob names in caswe they are not already
      if(fr()){
        rownames(out) <- gsub("\\.", ",", rownames(out))
      }
      out <- out |>
          t() |>
          as.data.frame() |>
          rownames_to_column(var = "year") |>
          mutate(model = .y) |>
          select(model, year, everything()) |>
          mutate(year = as.numeric(year))
      if(ts_len < ncol(out)){
        out <- out[, 1:ts_len]
      }
    }
    if("MPD" %in% names(out)){
      out <- out |>
        select(-MPD)
    }
    out
  })

  var_quants <- var_quants |>
    bind_rows() |>
    as_tibble() |>
    mutate(model = as.factor(model)) |>
    mutate(model = fct_relevel(model, names(models))) |>
    convert_prob_cols_language()

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")

  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(var_quants), value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data\n",
           .x, call. = FALSE)
    }
    mtch
  })

  # Set up x and y axes
  if(!is.null(xlim)){
    var_quants <- var_quants |>
      filter(year %in% xlim[1]:xlim[2])
  }

  # Dodge years for recruitment values. Need to do this before setting xlim values
  # because only the first model will be plotted in the last year if dodge is not
  # accounted for
  dodge_val <- 0
  var_dodge <- var_quants |>
    split(~model) |>
    map(~{
      x <- .x |> mutate(year = year + dodge_val)
      dodge_val <<- dodge_val + 0.1
      x
    }) |>
    bind_rows()

  # Color values have black prepended as it is the base model
  model_colors <- c(base_color, palette_colors)

  g <- var_quants |>
    ggplot(aes(x = year,
               y = !!sym(quants[2]),
               ymin = !!sym(quants[1]),
               ymax = !!sym(quants[3]))) +
    xlab(x_label) +
    ylab(y_label) +
    scale_color_manual(values = model_colors,
                       labels = map(models, ~{tex(as.character(attributes(.x)$model_desc))}))

  if(first_model_ribbon){
    first_model_nm <- as.character(var_quants$model[1])
    var_quants_first <- var_quants |>
      filter(model == first_model_nm)
    g <- g +
      geom_ribbon(data = var_quants_first,
                  fill = model_colors[1],
                  alpha = alpha) +
      geom_line(aes(color = model),
                size = line_width) +
      geom_point(aes(color = model),
                 size = point_size) +
      geom_line(aes(y = !!sym(quants[1]),
                    color = model),
                size = 0.5,
                lty = 2) +
      geom_line(aes(y = !!sym(quants[3]),
                    color = model),
                size = 0.5,
                lty = 2)
  }else{
    g <- g +
      geom_line(aes(color = model),
                size = line_width) +
      geom_point(aes(color = model),
                 size = point_size) +
      geom_line(aes(y = !!sym(quants[1]),
                    color = model),
                size = 0.5,
                lty = 2) +
      geom_line(aes(y = !!sym(quants[3]),
                    color = model),
                size = 0.5,
                lty = 2)
  }

  if(facet_wrap_var != "none"){
    if(!facet_wrap_var %in% names(var_quants)){
      stop("`facet_wrap_var` value of '", facet_wrap_var, "' is not present in the column\n",
           "names of the data frame your are plotting. The names available are:\n\n",
           paste(names(var_quants), collapse = "\n"),
           call. = FALSE)
    }
    g <- g +
      facet_wrap(as.formula(paste("~", facet_wrap_var)))
  }

  g <- g +
    scale_x_continuous(limits = c(xlim[1], xlim[2]),
                       breaks = min(xlim):max(xlim),
                       labels = xlim[1]:xlim[2],
                       expand = expansion(add = x_space)) +
    scale_y_continuous(expand = expansion(add = y_space)) +
    coord_cartesian(xlim = xlim) +
    coord_cartesian(ylim = ylim)

  if(single_model){
    if(!is.null(text_title_size)){
      g <- g + ggtitle(tex(names(models))) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
    }
    leg_loc <- NULL
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.justification = leg_loc,
            legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "white")) +
      labs(color = legend_title)
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
