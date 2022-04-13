#' Plot MPD age comp data along with the model fits for models
#' with both sexes
#'
#' @param model Model list as output by [model_setup()]
#' @param gear The gear number to plot
#' @param type One of 'bars' or 'pearson'
#' @param pearson_type One of 'age', 'year', or 'birthyear'
#' @param angle_x_labels If `TRUE` put 45 degree angle on x-axis tick labels
#'
#' @return a [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_linerange facet_grid
#' @export
plot_agecomp_fits_mpd <- function(model,
                                  gear,
                                  model_name = "",
                                  type = "bars",
                                  pearson_type = "age",
                                  angle_x_labels = FALSE){

  if(!type %in% c("bars", "pearson")){
    stop("type must be one of 'bars' or 'pearson'",
         call. = FALSE)
  }
  if(!pearson_type %in% c("age", "year", "birthyear")){
    stop("pearson_type must be one of 'age', 'year', or 'birthyear'",
         call. = FALSE)
  }
  if(gear < 0 || gear > length(model$mpd$a_obs)){
    stop("gear must be between 1 and ", length(model$mpd$a_obs),
         call. = FALSE)
  }
  nsex <- model$dat$num.sex
  ages <- as.character(model$dat$start.age:model$dat$end.age)
  if(type == "bars"){
    fit <- model$mpd$a_hat[[gear]] %>%
      select(ages) %>%
      as.matrix() %>%
      prop.table(1) %>%
      as_tibble()
    comp <- model$mpd$a_obs[[gear]] %>%
      select(ages) %>%
      as.matrix() %>%
      prop.table(1) %>%
      as_tibble()
    year_sex <- model$mpd$a_obs[[gear]] %>% select(year, sex)
    comp <- bind_cols(year_sex, comp)
    fit <- bind_cols(year_sex, fit)

    # Make long versions of the data frames
    comp <- comp %>% pivot_longer(!c(year, sex), names_to = "Age", values_to = "Proportion") %>%
      mutate(sex = factor(ifelse(sex == 1, "Female", "Male"), levels = c("Female", "Male")),
             Age = factor(as.numeric(Age)))
    fit <- fit %>% pivot_longer(!c(year, sex), names_to = "Age", values_to = "Proportion") %>%
      mutate(sex = factor(ifelse(sex == 1, "Female", "Male"), levels = c("Female", "Male")),
             Age = factor(as.numeric(Age)))

    g <- ggplot(comp, aes(x = Age, ymax = Proportion, ymin = 0)) +
      geom_linerange() +
      facet_grid(year ~ sex) +
      geom_line(data = fit, aes(x = Age, y = Proportion, group = 1, color = sex)) +
      scale_color_manual(values = c("red", "blue"))
  }else if(type == "pearson"){
    resids <- model$mpd$a_nu[[gear]] %>% select(!c(gear, area, group))
    resids <- resids %>% pivot_longer(!c(year, sex), names_to = "Age", values_to = "Proportion") %>%
      mutate(Sex = factor(ifelse(sex == 1, "Female", "Male"), levels = c("Female", "Male")),
             Age = factor(as.numeric(Age)),
             Year = factor(year)) %>%
      select(-c(sex, year)) %>%
      rename(`Standardized Residuals` = Proportion)

    if(pearson_type == "age"){
      aes_x <- "Age"
    }else if(pearson_type == "year"){
      aes_x <- "Year"
    }else if(pearson_type == "birthyear"){
      resids <- resids %>%
        mutate(`Year of birth` = factor(as.numeric(as.character(Year)) - as.numeric(as.character(Age)))) %>%
        select(-Year)
      aes_x <- "`Year of birth`"
    }
    g <- ggplot(resids, aes_string(aes_x, "`Standardized Residuals`", fill = "Sex")) +
      stat_boxplot(geom = "errorbar") +
      geom_boxplot(outlier.colour = "black",
                   outlier.shape = 3,
                   outlier.size = 0.5) +
      scale_fill_manual(values = c("#FF000050", "#0000FF50")) +
      geom_hline(aes(yintercept = 0), color = "red", linetype = "dashed", size = 0.3) +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      ggtitle(model_name) +
      ylab("")

    if(angle_x_labels){
      g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))
    }
  }
  g
}

#' Plot a three-panel residual figure. The panels are Age, Year, and Birth year residuals
#'
#' @param model An iSCAM model
#' @param model_name Name of the model for plot title
#' @param gear Number of gear to plot
#' @param gear_name Name of the gear for plot label
#' @param ylim_age ylim for the Age plot
#' @param ylim_year ylim for the Year plot
#' @param ylim_birthyear ylim for the birthyear plot
#' @param legend_pos Position of the legend inside the plot area (two-element vector of x, y)
#' @param show_title If `TRUE`, show the title (model and gear names) on top of the three panels
#' @param angle_x_labels A three-element vector of logicals. If `TRUE`, angle the x-axis tick
#' labels for that plot, where index 1 = Age, 2 = Year, and 3 = Birthyear plots
#'
#' @return a [cowplot::plot_grid()] object
#' @export
#' @importFrom cowplot plot_grid
plot_residuals <- function(model,
                           model_name = "Model",
                           gear = 1,
                           gear_name = model$dat$age_gear_abbrevs[gear],
                           ylim_age = c(-1, 1),
                           ylim_year = c(-1, 1),
                           ylim_birthyear = c(-1, -1),
                           legend_pos = c(0.05, 0.15),
                           show_title = TRUE,
                           angle_x_labels = c(FALSE, FALSE, TRUE)){

  plot_grid(
    plot_agecomp_fits_mpd(model,
                          gear = gear,
                          model_name = ifelse(show_title, paste(model_name, ", ", gear_name), ""),
                          type = "pearson",
                          pearson_type = "age",
                          angle_x_labels = angle_x_labels[1]) +
      coord_cartesian(ylim = ylim_age) +
      theme(strip.text.x = element_blank(),
            strip.background = element_rect(colour = "white", fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.box.background = element_rect(colour = "black"),
            legend.position = legend_pos),

    plot_agecomp_fits_splitsex(model,
                               gear = gear,
                               type = "pearson",
                               pearson_type = "year",
                               angle_x_labels = angle_x_labels[2]) +
      ylab("Standardized Residuals") +
      theme(axis.title.y = element_text(size = 24)) +
      coord_cartesian(ylim = ylim_year) +
      theme(legend.position = "none"),

    plot_agecomp_fits_splitsex(model,
                               gear = gear,
                               type = "pearson",
                               pearson_type = "birthyear",
                               angle_x_labels = angle_x_labels[3]) +
      coord_cartesian(ylim = ylim_birthyear) +
      theme(legend.position = "none"),

    ncol = 1,
    align = "v")
}
