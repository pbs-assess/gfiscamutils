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
plot_agecomp_fits_splitsex <- function(model,
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
plot_age_year_birthyear_residuals <- function(model,
                                              model_name = "iSCAM model",
                                              gear = 1,
                                              gear_name = model$dat$age_gear_abbrevs[gear],
                                              ylim_age = c(-1, 1),
                                              ylim_year = c(-1, 1),
                                              ylim_birthyear = c(-1, -1),
                                              legend_pos = c(0.05, 0.15),
                                              show_title = TRUE,
                                              angle_x_labels = c(FALSE, FALSE, TRUE)){

  plot_grid(
    plot_agecomp_fits_splitsex(model,
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

#' Plot age comp estimates
#'
#' @param model An iscam model object
#' @param which.gear which gear to use. Integer index
#' @param fg foreground color
#' @param bg background color
#' @param inches fraction of inches to use for bubbles
#'
#' @export
#' @importFrom PBSmodelling plotBubbles
#' @importFrom grDevices gray
make.age.comp.estimates.plot <- function(model,
                                         which.gear,
                                         fg = gray(level = 0.1, alpha = 0.5),
                                         bg = gray(level = 0.5, alpha = 0.5),
                                         inches = 0.12){

  ## ahat <- t(model$mpd$ahat[[which.gear]])
  ## ahat is a matrix which needs to be broken up into smaller
  ##  matrices, one for each gear
  ahat <- model$mpd$A_hat
  nagv <- model$dat$num.age.gears.vec
  sage <- model$mpd$n_A_sage[1]
  nage <- model$mpd$n_A_nage[1]
  ## Need the age comp data for the years
  age.comps <- model$dat$age.comps

  a.props <- list()
  ind <- 1
  for(i in 1:length(nagv)){
    yrs <- as.data.frame(age.comps[[i]])$year
    a.props[[i]] <- ahat[ind:(ind + length(yrs) - 1),]
    rownames(a.props[[i]]) <- yrs
    colnames(a.props[[i]]) <- sage:nage
    ind <- ind + length(yrs)
  }
  dat <- a.props[[which.gear]]

  x <- data.frame(expand.grid(as.numeric(rownames(dat)),
                              as.numeric(colnames(dat))),
                  prop = as.vector(dat))
  yrs <- as.numeric(rownames(dat))
  names(x) <- c("yr", "age", "prop")
  max.prop <- max(dat)
  symbols(c(x[,1], -1),
          c(x[,2], -1),
          circles = sqrt(c(x[,3], max.prop)),
          inches = inches,
          ylim = c(sage, nage),
          xlim = c(min(yrs), max(yrs)),
          xlab = "",
          ylab = "Age",
          xaxt = "n",
          fg = fg,
          bg = bg)
  axis(1, yrs)
  axis(4)
}

#' Make age comp fit plot
#'
#' @rdname make.age.comp.estimates.plot
#' @export
#' @importFrom PBSmodelling plotBubbles
#' @importFrom grDevices gray
make.age.comp.fit.plot <- function(model,
                                   which.gear,
                                   fg = gray(level = 0.1, alpha = 0.5),
                                   bg = gray(level = 0.5, alpha = 0.5),
                                   inches = 0.12){

  fit.dat <- as.data.frame(model$mpd$A_hat)
  comp.dat <- as.data.frame(model$mpd$d3_A)

  s.age <- model$mpd$n_A_sage[which.gear]
  n.age <- model$mpd$n_A_nage[which.gear]
  ages <- s.age:n.age
  n.ages <- length(s.age:n.age)

  ## resid.dat has unlabelled rows, which need to be extracted
  ##  correctly based on the values in the comp.dat data.frame
  which.rows <- which(comp.dat[,2] == which.gear)
  comp.dat <- comp.dat[which.rows ,]
  yrs <- comp.dat[, 1]

  fit.dat <- fit.dat[which.rows,]
  ## Remove the information columns, leaving only the comps
  comp.dat <- as.matrix(comp.dat[, -seq(1, 5)])
  obs.prop <- prop.table(comp.dat, 1)

  max.y <- max(fit.dat, obs.prop)
  n.side <- get_rows_cols(length(yrs))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))
  for(yr in 1:length(yrs)){
    year <- yrs[yr]
    obs <- obs.prop[yr,]
    est <- fit.dat[yr,]
    plot(ages,
         obs,
         type = "h",
         xlab = "",
         ylab = "",
         main = year,
         las = 1,
         ylim = c(0, max.y))
    lines(ages, est, lty = 1, lwd = 2, col = 2)
  }
  mtext("Age", side = 1, line = 0, outer = TRUE)
  mtext("Proportion", side = 2, line = 1, outer = TRUE)
}

#' Make age comp residuals plot
#' @rdname make.age.comp.estimates.plot
#' @export
#' @importFrom PBSmodelling plotBubbles
#' @importFrom grDevices gray
make.age.comp.residuals.plot <- function(model,
                                         which.gear,
                                         fg = gray(level = 0.1, alpha = 0.5),
                                         bg = gray(level = 0.5, alpha = 0.5),
                                         inches = 0.12){

  resid.dat <- as.data.frame(model$mpd$A_nu)
  comp.dat <- as.data.frame(model$mpd$d3_A)

  s.age <- model$mpd$n_A_sage[which.gear]
  n.age <- model$mpd$n_A_nage[which.gear]
  ages <- s.age:n.age
  n.ages <- length(s.age:n.age)


  ## resid.dat has unlabelled rows, which need to be extracted
  ##  correctly based on the values in the comp.dat data.frame
  which.rows <- which(comp.dat[,2] == which.gear)
  comp.dat <- comp.dat[which.rows ,]
  yrs <- comp.dat[, 1]

  resid.dat <- resid.dat[which.rows,]
  ## Remove the information columns, leaving only the comps
  comp.dat <- as.matrix(comp.dat[, -seq(1, 5)])
  obs.prop <- prop.table(comp.dat, 1)

  plotBubbles(t(resid.dat),
              xval = yrs,
              yval = ages,
              size = 0.1,
              powr = 0.5,
              xlab = "Year",
              ylab = "Age",
              las = 1,
              cex = 1.25,
              axes = FALSE)
  axis(1, at = yrs, labels = yrs)
}

#' Make age comp data plot
#'
#' @rdname make.age.comp.estimates.plot
#' @export
#' @importFrom PBSmodelling plotBubbles
make.age.comp.data.plot <- function(model,
                                    which.gear){

  a <- model$dat$age.comps[[which.gear]]
  yrs <- a[,1]
  a <- a[,-c(1, 2, 3, 4, 5)]
  plotBubbles(t(a),
              xval = yrs,
              yval = colnames(a),
              size = 0.1,
              powr = 0.5,
              xlab = "Year",
              ylab = "Age",
              cex = 0.75)
##              axes = FALSE)

}
