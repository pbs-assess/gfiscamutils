#' Plot the age fits for an MPD model
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param gear The number of the gear to plot
#' @param resid_type The type of residual plot to make if `type == "resids"`.
#' Options are "age", "year", and "birth_year"
#' @param comp_color Color for the age comp data lines and points
#' @param comp_point_size Size of the age comp data points
#' @param fit_line_color Color for the fit line
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_linerange facet_grid
#' @export
plot_agefits_mpd <- function(model,
                             gear = 1,
                             type = c("fits", "resids"),
                             resid_type = c("age", "year", "birth_year"),
                             comp_color = "black",
                             comp_point_size = 0.5,
                             fir_line_color = "red",
                             angle_x_labels = FALSE){

  type <- match.arg(type)
  resid_type <- match.arg(resid_type)

  if(class(model) != mdl_cls){
    if(class(model) != mdl_lst_cls){
      stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, "), ",
           "it is a list of models (",mdl_cls, "). Call the function ",
           "again passing only a single model")
    }
    stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, ")")
  }

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

  if(type == "fits"){
    vals <- model$mpd$a_hat %>%
      bind_rows() %>%
      select(year, gear, sex, ages)
  }else if(type == "resids"){
    vals <- model$mpd$a_nu[[gear]] %>%
      select(year, sex, ages)
  }
  vals <- vals %>%
    mutate(gear = gear_names[gear]) %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  # Make long versions of the fit/resid data frame
  vals <- vals %>%
    pivot_longer(!c(year, sex), names_to = "age", values_to = "prop") %>%
    mutate(age = factor(as.numeric(age)))

  if(type == "fits"){
    g <- ggplot(comps, aes(x = factor(age), ymax = prop, ymin = 0)) +
      geom_linerange(color = comp_color) +
      geom_point(aes(y = prop), color = comp_color, size = comp_point_size) +
      geom_line(data = vals, aes(y = prop, group = year), color = fit_line_color) +
      facet_grid(year ~ sex) +
      xlab("Age") +
      ylab("Proportion")
  }else if(type == "resids"){
    if(resid_type == "age"){
    }else if(resid_type == "year"){
    }else if(resid_type == "birth_year"){
      vals <- vals %>%
        mutate(birth_year = factor(as.numeric(as.character(year)) - as.numeric(as.character(age))))
    }
    g <- ggplot(vals, aes(!!sym(resid_type), prop, fill = sex)) +
      stat_boxplot(geom = "errorbar") +
      geom_boxplot(outlier.colour = "black",
                   outlier.shape = 3,
                   outlier.size = 0.5) +
      scale_fill_manual(values = c("#FF000050", "#0000FF50")) +
      geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 0.3) +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      ylab("Standardized residuals") +
      xlab(ifelse(resid_type == "birth_year", "Year of birth", firstup(resid_type)))
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}
