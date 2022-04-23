#' Plot the age fits for an MCMC model
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param gear The number of the gear to plot
#' @param type The type of residual plot to create. Options are "age",
#' "year", and "birth_year"
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param comp_color Color for the age comp data lines and points
#' @param comp_point_size Size of the age comp data points
#' @param ci_type One of "line", "ribbon", "both" to show the credible interval
#' @param ci_linetype See `linetype` in [ggplot2]. Only used if `ci_type` is "line" or "both"
#' @param ci_color Color for the lines or ribbon for the credible intervals
#' @param ci_alpha Opacity between 0 and 1 for the credible intervals ribbons. Only used if
#' `ci_type` is "ribbon" or "both"
#' @param ylim For residual plots, set the y-axis limits with this two-element vector
#' @param title_text_size Add the model description as a title with this font size. The text
#' comes from the `model_desc` attribute of `model`. If this is `NULL`, don't show a title
#' @param leg_loc A two-element vector describing the X-Y values between 0 and 1 to anchor
#' the legend to. eg. c(1, 1) is the top right corner and c(0, 0) is the bottom left corner
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @family Age plotting functions
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_fits_mcmc <- function(model,
                               gear = 1,
                               probs = c(0.025, 0.5, 0.975),
                               comp_color = "black",
                               comp_point_size = 0.5,
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

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
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
  gear_names <- model$dat$age_gear_names
  gear_name <- gear_names[gear]

  comps <- model$mpd$a_obs[[gear]] %>%
    select(-c(gear, area, group)) %>%
    pivot_longer(-c(year, sample_size, sex), names_to = "age", values_to = "prop") %>%
    mutate(age = as.numeric(age)) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))
  sample_size <- comps %>%
    distinct(year, sex, sample_size)
  comps <- comps %>%
    select(-sample_size)

  vals <- model$mcmccalcs$agefit_quants %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  prob_cols <- paste0(prettyNum(probs * 100), "%")
  quant_vals <- unique(vals$quant)
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, quant_vals, value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data: ", .x)
    }
    mtch
  })

  get_val <- function(d, q){
    d %>%
      filter(quant == q) %>%
      select(-quant) %>%
      pivot_longer(-c(year, sex), names_to = "age", values_to = "prop") %>%
      mutate(age = as.numeric(age))
  }
  lo_vals <- get_val(vals, quants[1]) %>%
    mutate(lo_prop = prop)
  med_vals <- get_val(vals, quants[2])
  hi_vals <- get_val(vals, quants[3]) %>%
    mutate(hi_prop = prop)
  rib_vals <- lo_vals %>%
    left_join(hi_vals, by = c("year", "sex", "age")) %>%
    select(-c(prop.x, prop.y))

  g <- ggplot(comps, aes(x = factor(age), ymax = prop, ymin = 0)) +
    geom_linerange(color = comp_color) +
    geom_point(aes(y = prop), color = comp_color, size = comp_point_size) +
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
    xlab("Age") +
    ylab("Proportion")

  if(!is.null(text_title_size)){
    g <- g + ggtitle(model_desc,
                     subtitle = paste0(model$dat$age_gear_names[gear], " Index")) +
      theme(plot.title = element_text(hjust = 0.5, size = text_title_size),
            plot.subtitle = element_text(hjust = 0.5, size = text_title_size))
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}

