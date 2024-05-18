#' Plot MCMC age residuals bubbles for an iSCAM model
#'
#' @inheritParams plot_age_fits_mcmc
#' @family Age plotting functions
#' @param include Two-element vector of the minimum and maximum values
#' to include. Eliminates large outliers which bias the bubble sizes
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_resids_mcmc <- function(model,
                                 gear = 1,
                                 include = c(-5, 5),
                                 text_title_size = 12,
                                 text_title_inc_mdl_nm = FALSE,
                                 angle_x_labels = FALSE){

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

  # Set up model description for the title
  model_desc <- as.character(attributes(model)$model_desc)

  if(gear < 1 || gear > length(model$mpd$a_obs)){
    stop("gear must be between 1 and ", length(model$mpd$a_obs),
         call. = FALSE)
  }

  nsex <- model$dat$num.sex
  ages <- as.character(model$dat$start.age:model$dat$end.age)
  gear_names <- tolower(model$dat$age_gear_names)
  gear_name <- gear_names[gear]

  resids <- model$mcmc$ageresids |>
    filter(tolower(gear) == tolower(gear_name)) |>
    pivot_longer(ages, names_to = "age") |>
    group_by(gear, year, sex, age) |>
    summarize(lo = quantile(value, probs = 0.025),
              med = quantile(value, probs = 0.5),
              hi = quantile(value, probs = 0.975)) |>
    mutate(sex = ifelse(sex %in% c(0, 1),
                        tr("Female"),
                        tr("Male"))) |>
    mutate(age = as.numeric(age)) |>
    mutate(Sign = ifelse(med >= 0, tr("Positive"), tr("Negative"))) |>
    filter(med >= include[1] & med <= include[2]) |>
    rename(`Median residual` = med)

  # If French, this will change the column Sign to Signe
  sign_sym <- sym(tr("Sign"))
  resids <- resids |>
    mutate(!!sign_sym := Sign)

  gear_name <- resids$gear |> unique()

  g <- ggplot(resids, aes(x = year, y = age)) +
    geom_point(aes(size = `Median residual`,
                   color = !!sign_sym),
               alpha = 0.1) +
    geom_point(aes(size = `Median residual`,
                   color = !!sign_sym),
               pch = 21) +
    scale_color_manual(values = c("red", "black")) +
    scale_size_area() +
    facet_grid(~sex) +
    xlab(tr("Year")) +
    ylab(tr("Age"))

  if(!is.null(text_title_size)){
    if(text_title_inc_mdl_nm){
      g <- g + ggtitle(model_desc,
                       subtitle = model$dat$age_gear_names[gear]) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size),
              plot.subtitle = element_text(hjust = 0.5, size = text_title_size))
    }else{
      g <- g + ggtitle(gear_name) +
        theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
    }
  }

  if(angle_x_labels){
    g <- g +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
  }

  g
}

