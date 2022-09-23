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
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_selex_gear_mcmc <- function(model,
                                 gear = 1,
                                 probs = c(0.025, 0.5, 0.975),
                                 show_maturity = FALSE,
                                 show_ci = TRUE,
                                 ages = as.character(model$dat$start.age:model$dat$end.age),
                                 breaks = seq(0, model$dat$end.age, 5),
                                 ci_type = c("both", "line", "ribbon"),
                                 ci_linetype = c("dotted", "solid",
                                                 "dashed", "dotdash",
                                                 "longdash", "twodash"),
                                 ci_alpha = 0.3,
                                 angle_x_labels = FALSE){

  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

  if(!is.character(ages)){
    ages <- as.character(ages)
  }

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

  if(gear < 1 || gear > length(model$mpd$a_obs)){
    stop("gear must be between 1 and ", length(model$mpd$a_obs),
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI")
  }

  # Extract selectivity parameters %>%

  vals <- model$mcmc$selest
  if(is.null(vals)){
    stop("MCMC selectivity estimates not found, see `model$mcmc$selest` ",
         "which is created in `read_mcmc()` and `load_special()`")
  }

  # Remove male "estimates" for models with number of sexes == 1. iSCAM outputs the
  # parameter values even if they were not estimated so they are gibberish
  if(model$dat$num.sex == 1){
    vals <- vals %>%
      filter(sex != 1)
  }

  vals <- vals |>
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  # Rename the parameter columns because the ages columns would
  # have these same names
  vals <- vals %>%
    rename(p1 = "a_hat", p2 = "g_hat")

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

  if(length(unique(vals$block)) < 2){
    stop("The gear `", gear_name, "` does not have at least two year blocks for ",
         "selectivity. Use `plot_selex_mcmc()` for this gear", call. = FALSE)
  }

  # Add age columns with logistic selectivity calculations
  for(i in ages){
    vals <- vals %>%
      mutate(!!sym(i) := 1 / (1 + exp(-(as.numeric(i) - p1) / p2)))
  }

  vals <- vals |>
    rename(Sex = sex)

  # Re-order the posteriors by group in order of a_hat smallest to largest
  block_lst <- vals |>
   split(~ block)
  vals <- map_dfr(block_lst, function(blk_df){
    sex_lst <- blk_df |>
      split(~ Sex)
    sex_lst <- map_dfr(sex_lst, function(sex_df){
      sex_df[order(sex_df$p1), ]
    })
  }) |>
    mutate(yearspan = paste0(gear_name, "(", start_year, "-", end_year, ")")) |>
    select(-c(gear, start_year, end_year, posterior, block, p1, p2)) |>
    select(yearspan, Sex, everything())

  num_posts <- nrow(model$mcmc$params)
  probs <- as.integer(probs * num_posts)

  make_longer <- function(d){
    d |>
      pivot_longer(-c(yearspan, Sex),
                   names_to = "age",
                   values_to = "value") |>
      mutate(age = as.numeric(age))
  }

  lo_vals <- vals |>
    group_by(yearspan, Sex) |>
    slice(probs[1]) |>
    make_longer() |>
    rename(lo_value = value)

  med_vals <- vals |>
    group_by(yearspan, Sex) |>
    slice(probs[2]) |>
    make_longer()

  hi_vals <- vals |>
    group_by(yearspan, Sex) |>
    slice(probs[3]) |>
    make_longer() |>
    rename(hi_value = value)

  rib_vals <- lo_vals %>%
    left_join(hi_vals,
              by = c("yearspan", "Sex", "age")) |>
    mutate(value = lo_value)

  g <- ggplot(med_vals, aes(x = factor(age),
                            y = value,
                            group = Sex,
                            color = Sex,
                            fill = Sex)) +
    geom_line() +
    geom_point() +
    xlab("Age") +
    ylab("Selectivity") +
    scale_x_discrete(breaks = breaks) +
    scale_color_manual(values = c("red", "blue"))

  if(show_ci && ci_type %in% c("ribbon", "both")){
    g <- g +
      geom_ribbon(data = rib_vals,
                  aes(ymin = lo_value,
                      ymax = hi_value,
                      group = Sex),
                  alpha = ci_alpha,
                  color = NA)
  }

  if(show_ci &&ci_type %in% c("line", "both")){
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
    facet_wrap(~ yearspan, scales = "free_x") +
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

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }


  suppressWarnings(print(g))
}
