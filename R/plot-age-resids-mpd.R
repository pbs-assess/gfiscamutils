#' Plot the age fits for an MPD model
#'
#' @inheritParams plot_age_fits_mcmc
#' @family Age plotting functions
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_linerange facet_grid
#' @export
plot_age_resids_mpd <- function(model,
                                gear = 1,
                                type = c("age", "year", "birth_year"),
                                comp_color = "black",
                                comp_point_size = 0.5,
                                fir_line_color = "red",
                                ylim = c(-3, 3),
                                text_title_size = 12,
                                leg_loc = c(1, 1),
                                angle_x_labels = FALSE){

  type <- match.arg(type)

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

  vals <- model$mpd$a_nu[[gear]] %>%
    select(year, sex, ages) %>%
    mutate(gear = gear_names[gear]) %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  # Make long version of the resids data frame
  vals <- vals %>%
    pivot_longer(!c(year, sex), names_to = "age", values_to = "prop") %>%
    mutate(age = factor(as.numeric(age))) %>%
    mutate(year = factor(year))

  if(type == "age"){
  }else if(type == "year"){
  }else if(type == "birth_year"){
    vals <- vals %>%
      mutate(birth_year = factor(as.numeric(as.character(year)) - as.numeric(as.character(age))))
  }

  g <- ggplot(vals, aes(!!sym(type), prop, fill = sex)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(outlier.colour = "black",
                 outlier.shape = 3,
                 outlier.size = 0.5) +
    scale_fill_manual(values = c("#FF000050", "#0000FF50")) +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 0.3) +
    ylab("Standardized residuals") +
    xlab(ifelse(type == "birth_year", "Year of birth", firstup(type)))

  if(!is.null(ylim)){
    g <- g + coord_cartesian(ylim = ylim)
  }

  if(!is.null(text_title_size)){
    g <- g + ggtitle(model_desc,
                     subtitle = paste0(model$dat$age_gear_names[gear], " Index")) +
      theme(plot.title = element_text(hjust = 0.5, size = text_title_size),
            plot.subtitle = element_text(hjust = 0.5, size = text_title_size))
  }

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
  }else{
    g <- g +
      theme(legend.position = leg_loc,
            legend.justification = leg_loc,
            legend.background = element_rect(fill = "white", color = "white")) +
      labs(fill = "Sex")
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
