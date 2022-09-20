#' Plot MCMC selectivities for iSCAM models
#'
#' @description
#' Plot the selectivity for any combination of gears in an iscam model
#'
#' @family Selectivity plotting functions
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param show_maturity If `TRUE`, overlay the maturity ogive on the selectivity plots
#' @param ages A character vector from 1 to the maximum age to show on
#' the plot. Defaults to the max age in the model. This may be needed when
#' The selectivity goes past the maximum age, so that we may see the
#' selectivity curve
#' @param breaks A vector representing the tick locations on the x-axis
#' @param ci_type One of "line", "ribbon", "both" to show the credible interval
#' @param ci_linetype See `linetype` in [ggplot2]. Only used if `ci_type` is "line" or "both"
#' @param ci_alpha Opacity between 0 and 1 for the credible intervals ribbons. Only used if
#' `ci_type` is "ribbon" or "both"
#' @param leg_loc A two-element vector describing the X-Y values between 0 and
#' 1 to anchor the legend to. eg. c(1, 1) is the top right corner and c(0, 0)
#' is the bottom left corner. It can also be the string "facet" in which case
#' the legend will appear in the empty facet if it exists.

#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_selex_mcmc <- function(model,
                            gear = NULL,
                            probs = c(0.025, 0.5, 0.975),
                            show_maturity = FALSE,
                            ages = as.character(model$dat$start.age:model$dat$end.age),
                            breaks = seq(0, model$dat$end.age, 5),
                            ci_type = c("both", "line", "ribbon"),
                            ci_linetype = c("dotted", "solid",
                                            "dashed", "dotdash",
                                            "longdash", "twodash"),
                            ci_alpha = 0.3,
                            leg_loc = c(1, 1),
                            angle_x_labels = FALSE){

  ci_type <- match.arg(ci_type)
  ci_linetype <- match.arg(ci_linetype)

  if(!is.character(ages)){
    ages <- as.character(ages)
  }

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(class(model) != mdl_cls){
    if(class(model) != mdl_lst_cls){
      stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, ")")
    }
    stop("`model` is a `gfiscamutils::mdl_lst_cls` class (",mdl_lst_cls, ")\n",
         "  It should be a `gfiscamutils::mdl_cls` class (",mdl_cls, ")")
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values\n",
         "  representing lower CI, median, and upper CI")
  }

  # Extract selectivity parameters
  vals <- model$mcmc$selest

  if(is.null(vals)){
    stop("MCMC selectivity estimates not found for this model, see\n",
         "  `model$mcmc$selest` which is created in `read_mcmc()` and `load_special()`")
  }

  # Remove male "estimates" for models with number of sexes == 1. iSCAM outputs the
  # parameter values even if they were not estimated so they are gibberish
  if(model$dat$num.sex == 1){
    vals <- vals %>%
      filter(sex != 1)
  }

  vals <- vals %>%
    mutate(sex = ifelse(sex %in% c(0, 2), "Female", "Male"))

  gear_names <- model$dat$gear_names
  if(length(unique(vals$gear)) != length(gear_names)){
    stop("`model$dat$gear_names` is not the same length as the number of gears present\n",
         "  in the MCMC selectivity parameter outputs. Check your declaration of the names\n",
         "  in the iSCAM data file and try again.")
  }
  if(!is.null(gear)){
    valid_gear_nums <- seq_along(gear_names)
    if(!all(gear %in% valid_gear_nums)){
      stop("One or more of the gear numbers you requested is outside the range of possible gears.\n\n",
           "Available gears numbers are: ", paste(valid_gear_nums, collapse = ", "), "\n\n",
           "Names for these are:\n", paste(gear_names, collapse = "\n"))
    }
    vals <- vals %>%
      filter(gear %in% gear_names[!!gear])
    gear_names <- gear_names[gear]
  }

  # Rename the parameter columns because the ages columns would
  # have these same names
  vals <- vals %>%
    rename(p1 = "a_hat", p2 = "g_hat")

  # Remove gears with TV selectivity and give a warning
  vals <- vals %>%
    split(~gear) %>%
    imap(~{
      yrs <- unique(.x$start_year)
      if(length(yrs) > 1){
        warning("`gear` ", unique(.x$gear), " has selectivity blocks (is time-varying)\n",
                "  and must be plotted seperately using `plot_tv_selex_mcmc()`\n")
        gear_names <<- gear_names[-which(gear_names == unique(.x$gear))]
        return(NULL)
      }
      return(.x)
    })

  # Remove NULL list elements (fixed parameters)
  vals <- vals[!sapply(vals, is.null)] %>%
    bind_rows()

  # Add age columns with logistic selectivity calculations
  for(i in ages){
    vals <- vals %>%
      mutate(!!sym(i) := 1 / (1 + exp(-(as.numeric(i) - p1) / p2)))
  }

  get_val <- function(d, q){
    d %>%
      filter(quants == q) %>%
      select(-quants) %>%
      pivot_longer(-c(gear, start_year, end_year, Sex, p1, p2),
                   names_to = "age",
                   values_to = "value") %>%
      mutate(age = as.numeric(age))
  }
  vals <- vals %>%
    rename(Sex = sex)

  # Re-order the posteriors by group in order of a_hat smallest to largest
  gear_lst <- vals |>
    split(~ gear)
  vals <- gear_lst |>
    map_dfr(function(gear_df){
      sex_lst <- gear_df |>
        split(~ Sex)
      sex_lst <- map_dfr(sex_lst, function(sex_df){
        sex_df[order(sex_df$p1), ]
      })
  }) |>
    select(-c(posterior, block, start_year, end_year, p1, p2)) |>
    select(gear, Sex, everything())

  num_posts <- nrow(model$mcmc$params)
  probs <- as.integer(probs * num_posts)

  make_longer <- function(d){
    d |>
      pivot_longer(-c(gear, Sex),
                   names_to = "age",
                   values_to = "value") |>
      mutate(age = as.numeric(age))
  }

  vals <- vals |>
    mutate(gear = factor(gear)) |>
    mutate(gear = fct_relevel(gear, gear_names))
  lo_vals <- vals |>
    group_by(gear, Sex) |>
    slice(probs[1]) |>
    make_longer() |>
    rename(lo_value = value)

  med_vals <- vals |>
    group_by(gear, Sex) |>
    slice(probs[2]) |>
    make_longer()

  hi_vals <- vals |>
    group_by(gear, Sex) |>
    slice(probs[3]) |>
    make_longer() |>
    rename(hi_value = value)

  rib_vals <- lo_vals %>%
    left_join(hi_vals,
              by = c("gear", "Sex", "age")) |>
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
    facet_wrap(~ gear) +
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

  if(is.null(leg_loc)){
    g <- g +
      theme(legend.position = "none")
  }else if(leg_loc[1] == "facet"){
    g <- g %>% move_legend_to_empty_facet()
  }else{
    g <- g +
      theme(legend.justification = leg_loc,
            legend.position = leg_loc,
            legend.background = element_rect(fill = "white", color = "white"))
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
  #suppressWarnings(print(g))
}
