#' Plot MCMC age fits for an iSCAM model
#'
#' @family Age plotting functions
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param gear The number of the gear to plot
#' @param type The type of residual plot to create. Options are "age", "year",
#' and "birth_year"
#' @param yrs A vector of years to include in the plot. If the maximum extends
#' past the range of years in the data, the maximum in the data will be used.
#' If `NULL`, all years will be included
#' @param comp_color Color for the age comp data lines and points
#' @param comp_point_size Size of the age comp data points
#' @param ci_type One of "line", "ribbon", "both" to show the credible interval
#' @param ci_linetype See `linetype` in [ggplot2]. Only used if `ci_type` is
#' "line" or "both"
#' @param ci_color Color for the lines or ribbon for the credible intervals
#' @param ci_alpha Opacity between 0 and 1 for the credible intervals ribbons.
#' Only used if `ci_type` is "ribbon" or "both"
#' @param ylim For residual plots, set the y-axis limits with this two-element
#' vector
#' @param title_text_size Add the model description as a title with this font
#' size. The text comes from the `model_desc` attribute of `model`. If this is
#' `NULL`, don't show a title
#' @param text_title_inc_mdl_nm Logical. If `TRUE`, make the model name the
#' main title and the gear the subtitle. If `FALSE`, do not show the model
#' name, and make the gear the main title (if `text_title_size` is `NULL`,
#' no title will be shown)
#' @param leg_loc A two-element vector describing the X-Y values between 0 and
#' 1 to anchor the legend to. eg. c(1, 1) is the top right corner and c(0, 0)
#' is the bottom left corner
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#' @param show_sample_size_f_only If `TRUE`, show the sample size in the female
#' panels only. If `FALSE`, show the sample size in both male and female panels.
#' Included for cases where the sample size is shared or repeated for both sexes.
#' @param sample_size_x The x value (age) to place the sample size label
#' @param sample_size_y The y value (0-1) to place the sample size label
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_fits_mcmc <- function(model,
                               gear = 1,
                               yrs = NULL,
                               comp_color = "black",
                               comp_width = 0.5,
                               comp_alpha = 0.5,
                               ci_linetype =  c("dotted", "solid",
                                                "dashed", "dotdash",
                                                "longdash", "twodash"),
                               ci_color = "red",
                               ci_alpha = 0.3,
                               text_title_size = 12,
                               text_title_inc_mdl_nm = FALSE,
                               angle_x_labels = FALSE,
                               show_sample_size_f_only = TRUE,
                               sample_size_x = 17,
                               sample_size_y = 0.2){

  ci_linetype <- match.arg(ci_linetype)

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object",
           call. = FALSE)
    }
    stop("`model` is not an iscam model object",
         call. = FALSE)
  }

  # Set up model description for the title
  model_desc <- as.character(attributes(model)$model_desc)

  if(gear < 1 || gear > length(model$mpd$a_obs)){
    stop("gear must be between 1 and ", length(model$mpd$a_obs),
         call. = FALSE)
  }

  nsex <- model$dat$num.sex
  ages <- as.character(model$dat$start.age:model$dat$end.age)
  gear_names <- model$dat$age_gear_names
  gear_name <- gear_names[gear]

  comps <- model$mpd$a_obs[[gear]] |>
    pivot_longer(ages, names_to = "age") |>
    group_by(year, sex) |>
    mutate(prop = value / sum(value)) |>
    select(-c(gear, area, group)) |>
    mutate(sex = ifelse(sex %in% c(0, 1),
                        tr("Female"),
                        tr("Male"))) |>
    select(-value) |>
    mutate(age = as.numeric(age))

  sample_size <- comps |>
    distinct(year, sex, sample_size) |>
    mutate(x = sample_size_x,
           y = sample_size_y,
           prop = 0, # Not used but necessary to add sample size text to plot
           sample_size = paste0("n = ", sample_size))

  if(show_sample_size_f_only){
    sample_size <- sample_size |>
      mutate(sample_size = ifelse(sex == tr("Male"), "", sample_size))
  }

  comps <- comps |>
    select(-sample_size)

  # Translate gear names
  d <- model$mcmc$agefits |>
    mutate(gear = tr(gear))

  fits <- d |>
    filter(gear == gear_name) |>
    pivot_longer(ages, names_to = "age") |>
    group_by(gear, year, sex, age) |>
    summarize(lo = quantile(value, probs = 0.025),
              med = quantile(value, probs = 0.5),
              hi = quantile(value, probs = 0.975)) |>
    mutate(sex = ifelse(sex %in% c(0, 1),
                        tr("Female"),
                        tr("Male"))) |>
    mutate(age = as.numeric(age))

  gear_name <- fits$gear |> unique()
  if(!is.null(yrs)){
    if(!any(c("numeric", "integer") %in% class(yrs))){
      stop("`yrs` must be a numeric or integer type",
           call. = FALSE)
    }

    if(!all(min(yrs) %in% fits$year & min(yrs) %in% comps$year)){
      stop("Not all `yrs` exist in the `comps` and `fits` years",
           call. = FALSE)
    }
    sample_size <- sample_size |>
      filter(year %in% yrs)
    comps <- comps |>
      filter(year %in% yrs)
    fits <- fits |>
      filter(year %in% yrs)
  }

  g <- ggplot(fits,
              aes(x = age,
                  y = med,
                  ymin = lo,
                  ymax = hi)) +
    geom_col(data = comps,
             mapping = aes(x = as.numeric(age),
                           y = prop),
             width = comp_width,
             alpha = comp_alpha,
             color = comp_color,
             inherit.aes = FALSE) +
    geom_ribbon(fill = ci_color, alpha = ci_alpha) +
    geom_point() +
    geom_line() +
    geom_line(aes(x = age, y = lo), inherit.aes = FALSE, linetype = ci_linetype, color = ci_color) +
    geom_line(aes(x = age, y = hi), inherit.aes = FALSE, linetype = ci_linetype, color = ci_color) +
    facet_grid(year ~ sex) +
    xlab(tr("Age")) +
    ylab(tr("Proportion"))

  if(!is.null(text_title_size)){
    if(text_title_inc_mdl_nm){
      subtitle <- ifelse(model$dat$age_gear_names[gear] %in% model$dat$index_gear_names,
                         paste(model$dat$age_gear_names[gear], tr("Index")),
                         model$dat$age_gear_names[gear])
      g <- g + ggtitle(model_desc,
                       subtitle = model$dat$age_gear_names[gear]) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size),
              plot.subtitle = element_text(hjust = 0.5, size = text_title_size))
    }else{
      g <- g + ggtitle(firstup(gear_name)) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
    }
  }

  # Add sample sizes
  g <- g + geom_text(sample_size, mapping = aes(x = x, y = y, label = sample_size), inherit.aes = FALSE)

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}

