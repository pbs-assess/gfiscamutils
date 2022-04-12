#' Plot the selectivity for all gears in the iscam model
#'
#' @param model An iscam model object as output by [arrowtooth::model_setup()]
#' @param probs A 3-element vector of probabilities that appear in the output data frames
#' This is provided in case the data frames have more than three different quantile levels
#' @param show_maturity If `TRUE`, overlay the maturity ogive on the selectivity plots
#' @param ci_type One of "line", "ribbon", "both" to show the credible interval
#' @param ci_linetype See `linetype` in [ggplot2]. Only used if `ci_type` is "line" or "both"
#' @param ci_alpha Opacity between 0 and 1 for the credible intervals ribbons. Only used if
#' `ci_type` is "ribbon" or "both"

#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_selex_mcmc <- function(model,
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

  ages <- as.character(model$dat$start.age:model$dat$end.age)

  # Rename the parameter columns because the ages columns would
  # have these same names
  vals <- vals %>%
    rename(p1 = `1`, p2 = `2`)

  # Remove gears with TV selectivity and give a warning
  vals <- vals %>%
    split(~gear) %>%
    imap(~{
      yrs <- unique(.x$start_year)
      if(length(yrs) > 1){
        warning("`gear` ", unique(.x$gear), " has selectivity blocks (is time-varying) ",
                "and must be plotted using `plot_tv_selex_mcmc()`\n")
        return(NULL)
      }
      return(.x)
    })
  # Remove NULL list elements (fixed parameters)
  vals <- vals[!sapply(vals, is.null)] %>%
    bind_rows()

  # Add age columns with selectivity calculations
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
    mutate(lo_value = value)
  med_vals <- get_val(vals, quants[2])
  hi_vals <- get_val(vals, quants[3]) %>%
    mutate(hi_value = value)
  rib_vals <- lo_vals %>%
    left_join(hi_vals,
              by = c("gear", "start_year", "end_year", "Sex", "age")) %>%
    select(-c(p1.x, p1.y, p2.x, p2.y, value.x)) %>%
    rename(value = value.y)

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

  suppressWarnings(print(g))
}
