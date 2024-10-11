#' Plot MPD age fits for an iSCAM model
#'
#' @inheritParams plot_age_fits_mcmc
#' @family Age plotting functions
#'
#' @param comp_point_size Size of the age composition points
#' @param fit_line_color Color fot the age fit line
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_linerange facet_grid
#' @export
plot_age_fits_mpd <- function(model,
                              gear = 1,
                              comp_color = "black",
                              comp_point_size = 0.5,
                              fit_line_color = "red",
                              text_title_size = 12,
                              angle_x_labels = FALSE){

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

  vals <- model$mpd$a_hat %>%
    bind_rows() %>%
    select(year, gear, sex, ages) %>%
    mutate(gear = gear_names[gear]) %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  # Make long versions of the fits data frame
  vals <- vals %>%
    pivot_longer(!c(year, sex), names_to = "age", values_to = "prop") %>%
    mutate(age = factor(as.numeric(age)))

  g <- ggplot(comps, aes(x = factor(age), ymax = prop, ymin = 0)) +
    geom_linerange(color = comp_color) +
    geom_point(aes(y = prop), color = comp_color, size = comp_point_size) +
    geom_line(data = vals, aes(y = prop, group = year), color = fit_line_color) +
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
