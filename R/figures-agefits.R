#' Plot the age fits for an MCMC model
#'
#' @param models An iscam model object (class [mdl_cls])
#' @param gear The number of the gear to plot
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param comp_color Color for the age comp data lines and points
#' @param comp_point_size Size of the age comp data points
#' @param ci_type One of "line", "ribbon", "both" to show the credible interval
#' @param ci_linetype See `linetype` in [ggplot2]. Only used if `ci_type` is "line" or "both"
#' @param ci_color Color for the lines or ribbon for the credible intervals
#' @param ci_alpha Opacity between 0 and 1 for the credible intervals ribbons. Only used if
#' `ci_type` is "ribbon" or "both"
#'
#' @return
#' @export
plot_agefits <- function(model,
                         gear = 1,
                         probs = c(0.025, 0.5, 0.975),
                         comp_color = "black",
                         comp_point_size = 0.5,
                         ci_type = c("both", "line", "ribbon"),
                         ci_linetype =  c("dotted", "solid",
                                          "dashed", "dotdash",
                                          "longdash", "twodash"),
                         ci_color = "red",
                         ci_alpha = 0.3){

  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

  # if(!type %in% c("fits", "resids")){
  #   stop("Type '", type, "' is not one of the implemented types")
  # }

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

  fits <- model$mcmccalcs$agefit_quants %>%
    filter(gear == gear_name) %>%
    select(-gear) %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  prob_cols <- paste0(prettyNum(probs * 100), "%")
  quant_vals <- unique(fits$quant)
  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, quant_vals, value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data: ", .x)
    }
    mtch
  })

  get_fit <- function(d, q){
    d %>%
      filter(quant == q) %>%
      select(-quant) %>%
      pivot_longer(-c(year, sex), names_to = "age", values_to = "prop") %>%
      mutate(age = as.numeric(age))
  }
  lo_fits <- get_fit(fits, quants[1]) %>%
    mutate(lo_prop = prop)
  med_fits <- get_fit(fits, quants[2])
  hi_fits <- get_fit(fits, quants[3]) %>%
    mutate(hi_prop = prop)
  rib_fits <- lo_fits %>%
    left_join(hi_fits, by = c("year", "sex", "age")) %>%
    select(-c(prop.x, prop.y))

browser()
  g <- ggplot(comps, aes(x = factor(age), ymax = prop, ymin = 0)) +
    geom_linerange(color = comp_color) +
    geom_point(aes(y = prop), color = comp_color, size = comp_point_size) +
    geom_line(data = med_fits, aes(y = prop, group = year), color = ci_color)

  if(ci_type %in% c("line", "both")){
    g <- g +
      geom_line(data = lo_fits, aes(y = lo_prop, group = year),
                color = ci_color,
                linetype = ci_linetype) +
      geom_line(data = hi_fits, aes(y = hi_prop, group = year),
                color = ci_color,
                linetype = ci_linetype)
  }
  if(ci_type %in% c("ribbon", "both")){
    g <- g +
      geom_ribbon(data = rib_fits, aes(ymin = lo_prop, ymax = hi_prop, group = year),
                  fill = ci_color, alpha = ci_alpha)
  }
  g <- g +
    facet_grid(year ~ sex) +
    xlab("Age") +
    ylab("Proportion")

  g
}
