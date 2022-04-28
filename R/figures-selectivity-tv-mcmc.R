#' Plot MCMC selectivities by single gear for iSCAM models
#'
#' @description
#' Plot the (possibly time-varying) selectivity for a particular gear in
#' an iscam model
#'
#' @inheritParams plot_selex_mcmc
#' @family Selectivity plotting functions
#'
#' @param gear The gear number in the model

#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_gear_selex_mcmc <- function(model,
                                 gear = 1,
                                 probs = c(0.025, 0.5, 0.975),
                                 show_maturity = FALSE,
                                 ci_type = c("both", "line", "ribbon"),
                                 ci_linetype = c("dotted", "solid",
                                                 "dashed", "dotdash",
                                                 "longdash", "twodash"),
                                 ci_alpha = 0.3){

  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

  if(class(model) != mdl_cls){
    if(class(model) != mdl_lst_cls){
      stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, "), ",
           "it is a list of models (",mdl_cls, "). Call the function ",
           "again passing only a single model")
    }
    stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, ")")
  }

  if(length(gear) != 1){
    stop("Exactly one `gear` number must be supplied")
  }
  if(class(gear) != "numeric" && gear != "integer"){
    stop("`gear` must be a number")
  }
  if(gear < 1){
    stop("`gear` must be a positive number 1 or greater")
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  # Extract selectivity parameters
  vals <- model$mcmccalcs$selest_quants
  if(is.null(vals)){
    stop("MCMC selectivity estimates not found, see `model$mcmc$selest` ",
         "which is created in `read_mcmc()` and `load_special()`")
  }

  vals <- vals %>%
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

  ages <- as.character(model$mpd$age)

  # Rename the parameter columns because the ages columns would
  # have these same names
  vals <- vals %>%
    rename(p1 = `1`, p2 = `2`)

  # Filter out the gear
  gear_name <- model$dat$gear_names[gear]
  if(!length(gear_name)){
    stop("`gear` number ", gear, " was not found in the model")
  }

  vals <- vals %>%
    filter(gear == gear_name)
  if(!nrow(vals)){
    stop("`gear` ", gear_name, " was not found in the model selectivity output")
  }

  # Add age columns with logistic selectivity calculations
  for(i in ages){
    vals <- vals %>%
      mutate(!!sym(i) := 1 / (1 + exp(-(as.numeric(i) - p1) / p2)))
  }

  get_val <- function(d, q){
    d %>%
      filter(quant == q) %>%
      select(-quant) %>%
      pivot_longer(-c(gear, start_year, end_year, Sex, p1, p2),
                   names_to = "age",
                   values_to = "value") %>%
      mutate(age = as.numeric(age))
  }
  vals <- vals %>%
    rename(Sex = sex)
  lo_vals <- get_val(vals, quants[1]) %>%
    mutate(lo_value = value) %>%
    mutate(yearspan = paste0(gear_name, "(", start_year, "-", end_year, ")"))
  med_vals <- get_val(vals, quants[2]) %>%
    mutate(yearspan = paste0(gear_name, "(", start_year, "-", end_year, ")"))
  hi_vals <- get_val(vals, quants[3]) %>%
    mutate(hi_value = value) %>%
    mutate(yearspan = paste0(gear_name, "(", start_year, "-", end_year, ")"))
  rib_vals <- lo_vals %>%
    left_join(hi_vals,
              by = c("gear", "start_year", "end_year", "Sex", "age")) %>%
    select(-c(p1.x, p1.y, p2.x, p2.y, value.x, yearspan.x)) %>%
    rename(value = value.y,
           yearspan = yearspan.y)

  g <- ggplot(med_vals, aes(x = factor(age),
                            y = value,
                            group = Sex,
                            color = Sex,
                            fill = Sex)) +
    geom_line() +
    geom_point() +
    xlab("Age") +
    ylab("Selectivity") +
    scale_x_discrete(breaks = seq(0, max(as.numeric(ages)), 5)) +
    scale_color_manual(values = c("red", "blue"))

  if(ci_type %in% c("ribbon", "both")){
    g <- g +
      geom_ribbon(data = rib_vals,
                  aes(ymin = lo_value,
                      ymax = hi_value,
                      group = Sex),
                  alpha = ci_alpha,
                  color = NA)
  }
  if(ci_type %in% c("line", "both")){
    g <- g +
      geom_line(data = lo_vals, aes(y = lo_value,
                                    group = Sex,
                                    color = Sex),
                linetype = ci_linetype) +
      geom_line(data = hi_vals, aes(y = hi_value,
                                    group = Sex,
                                    color = Sex),
                linetype = ci_linetype)
  }
  g <- g +
    #facet_wrap(~ start_year, labeller = facet_labeller(start_year, end_year)) +
    facet_wrap(~ yearspan) +
    xlab("Age") +
    ylab("Proportion")

  if(show_maturity){
    model$mpd$ma
    if(model$dat$num.sex == 2){
      a50_male <- model$dat$age.at.50.mat[1]
      sigma_a50_male <- model$dat$sd.at.50.mat[1]
      a50_female <- model$dat$age.at.50.mat[2]
      sigma_a50_female <- model$dat$sd.at.50.mat[2]
      g <- g +
        geom_function(fun = function(x){1 / (1 + exp(-(x - a50_male) / sigma_a50_male))},
                      color = "blue",
                      linetype = "dashed")
    }else{
      a50_female <- model$dat$age.at.50.mat[1]
      sigma_a50_female <- model$dat$sd.at.50.mat[1]
    }
    g <- g +
      geom_function(fun = function(x){1 / (1 + exp(-(x - a50_female) / sigma_a50_female))},
                    color = "red",
                    linetype = "dashed")
  }

  suppressWarnings(print(g))
}
