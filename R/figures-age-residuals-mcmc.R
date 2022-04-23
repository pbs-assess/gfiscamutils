#' Plot the age residuals for an MCMC model
#'
#' @rdname plot_age_fits_mcmc
#'
#' @family Age plotting functions
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_resids_mcmc <- function(model,
                                 gear = 1,
                                 type = c("age", "year", "birth_year"),
                                 probs = c(0.025, 0.5, 0.975),
                                 comp_color = "black",
                                 comp_point_size = 0.5,
                                 ci_type = c("both", "line", "ribbon"),
                                 ci_linetype =  c("dotted", "solid",
                                                  "dashed", "dotdash",
                                                  "longdash", "twodash"),
                                 ci_color = "red",
                                 ci_alpha = 0.3,
                                 ylim = c(-3, 3),
                                 text_title_size = 12,
                                 leg_loc = c(1, 1),
                                 angle_x_labels = FALSE){

  type <- match.arg(type)
  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
  }

  if(gear < 1 || gear > length(model$mpd$a_obs)){
    stop("gear must be between 1 and ", length(model$mpd$a_obs),
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector ",
         "of three values representing lower CI, median, and upper CI")
  }

  # Set up model description for the title
  model_desc <- as.character(attributes(model)$model_desc)

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

  vals <- model$mcmccalcs$ageresids_quants %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  prob_cols <- paste0(prettyNum(probs * 100), "%")
  quant_vals <- unique(vals$quant)
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, quant_vals, value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC ",
           "output data: ", .x)
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

  med_vals <- med_vals %>%
    mutate(age = factor(age),
           year = factor(year))
  if(type == "age"){
  }else if(type == "year"){
  }else if(type == "birth_year"){
    med_vals <- med_vals %>%
      mutate(birth_year = factor(as.numeric(as.character(year)) - as.numeric(as.character(age))))
  }

  g <- ggplot(med_vals, aes(!!sym(type), prop, fill = sex)) +
    stat_boxplot(geom = "errorbar") +
    geom_boxplot(outlier.colour = "black",
                 outlier.shape = 3,
                 outlier.size = 0.5) +
    scale_fill_manual(values = c("#FF000050", "#0000FF50")) +
    geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 0.3) +
    theme(plot.title = element_text(size = 14, face = "bold")) +
    ylab("Standardized residuals") +
    xlab(ifelse(type == "birth_year", "Year of birth", firstup(type)))

  if(!is.null(ylim)){
    g <- g + coord_cartesian(ylim = ylim)
  }

  if(!is.null(text_title_size)){
    g <- g + ggtitle(model_desc, subtitle = model$dat$age_gear_names[gear]) +
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

