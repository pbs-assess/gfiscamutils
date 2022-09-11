#' Plot MCMC age fits for an iSCAM model
#'
#' @family Age plotting functions
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param gear The number of the gear to plot
#' @param type The type of residual plot to create. Options are "age", "year",
#' and "birth_year"
#' @param probs A 3-element vector of probabilities that appear in the
#' output data frames. This is provided in case the data frames have more than
#' three different quantile levels
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
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_fits_mcmc <- function(model,
                               gear = 1,
                               probs = c(0.025, 0.5, 0.975),
                               yrs = NULL,
                               comp_color = "black",
                               comp_point_size = 0.5,
                               ci_type = c("both", "line", "ribbon"),
                               ci_linetype =  c("dotted", "solid",
                                                "dashed", "dotdash",
                                                "longdash", "twodash"),
                               ci_color = "red",
                               ci_alpha = 0.3,
                               text_title_size = 12,
                               text_title_inc_mdl_nm = FALSE,
                               angle_x_labels = FALSE,
                               show_sample_size_f_only = TRUE){

  ci_type <- match.arg(ci_type)
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

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  nsex <- model$dat$num.sex
  ages <- as.character(model$dat$start.age:model$dat$end.age)
  gear_names <- tolower(model$dat$age_gear_names)
  gear_name <- gear_names[gear]

  comps <- model$mpd$a_obs[[gear]] |>
    select(-c(gear, area, group)) |>
    pivot_longer(-c(year, sample_size, sex), names_to = "age", values_to = "prop") |>
    mutate(age = as.numeric(age)) |>
    mutate(sex = ifelse(sex %in% c(0, 2),
                        en2fr("Female"),
                        en2fr("Male")))

  sample_size <- comps |>
    distinct(year, sex, sample_size) |>
    mutate(x = 17,
           y = 0.2,
           prop = 0, # Not used but necessary to add sample size text to plot
           sample_size = paste0("n = ", sample_size))

  if(show_sample_size_f_only){
    sample_size <- sample_size |>
      mutate(sample_size = ifelse(sex == en2fr("Male"), "", sample_size))
  }

  comps <- comps |>
    select(-sample_size)

  vals <- model$mcmccalcs$agefit_quants |>
    filter(tolower(gear) == gear_name) |>
    select(-gear) |>
    mutate(sex = ifelse(sex %in% c(0, 2),
                        en2fr("Female"),
                        en2fr("Male")))

  if(!is.null(yrs)){
    if(!any(c("numeric", "integer") %in% class(yrs))){
      stop("`yrs` must be a numeric or integer type",
           call. = FALSE)
    }
    if(!all(min(yrs) %in% vals$year & min(yrs) %in% comps$year)){
      stop("Not all `yrs` exist in the `comps` and `fits` years",
           call. = FALSE)
    }
    comps <- comps |>
      filter(year %in% yrs)
    vals <- vals |>
      filter(year %in% yrs)
  }

  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  quant_vals <- unique(vals$quants)
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, quant_vals, value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data: ", .x)
    }
    mtch
  })

  get_val <- function(d, q){
    d |>
      filter(quants == q) |>
      select(-quants) |>
      pivot_longer(-c(year, sex), names_to = "age", values_to = "prop") |>
      mutate(age = as.numeric(age))
  }
  lo_vals <- get_val(vals, quants[1]) |>
    mutate(lo_prop = prop)
  med_vals <- get_val(vals, quants[2])
  hi_vals <- get_val(vals, quants[3]) |>
    mutate(hi_prop = prop)
  rib_vals <- lo_vals |>
    left_join(hi_vals, by = c("year", "sex", "age")) |>
    select(-c(prop.x, prop.y))

  g <- ggplot(comps, aes(x = factor(age), ymax = prop, ymin = 0)) +
    geom_linerange(color = comp_color) +
    geom_point(aes(y = prop), color = comp_color, size = comp_point_size) +
    geom_point(data = med_vals, aes(y = prop, group = year), color = ci_color) +
    geom_line(data = med_vals, aes(y = prop, group = year), color = ci_color)

  if(ci_type %in% c("line", "both")){
    g <- g +
      geom_line(data = lo_vals, aes(y = lo_prop, group = year),
                color = ci_color,
                linetype = ci_linetype) +
      geom_line(data = hi_vals, aes(y = hi_prop, group = year),
                color = ci_color,
                linetype = ci_linetype)
  }
  if(ci_type %in% c("ribbon", "both")){
    g <- g +
      geom_ribbon(data = rib_vals, aes(ymin = lo_prop, ymax = hi_prop, group = year),
                  fill = ci_color, alpha = ci_alpha)
  }
  g <- g +
    facet_grid(year ~ sex) +
    xlab(en2fr("Age")) +
    ylab(en2fr("Proportion"))

  if(!is.null(text_title_size)){
    if(text_title_inc_mdl_nm){
      subtitle <- ifelse(model$dat$age_gear_names[gear] %in% model$dat$index_gear_names,
                         paste(model$dat$age_gear_names[gear], en2fr("Index")),
                         model$dat$age_gear_names[gear])
      g <- g + ggtitle(model_desc,
                       subtitle = model$dat$age_gear_names[gear]) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size),
              plot.subtitle = element_text(hjust = 0.5, size = text_title_size))
    }else{
      g <- g + ggtitle(gear_name) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
    }
  }

  # Add sample sizes
  g <- g + geom_text(sample_size, mapping = aes(x = x, y = y, label = sample_size))

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}

