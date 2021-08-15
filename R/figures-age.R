#' Plot MPD age comp data along with the model fits for models
#' with both sexes
#'
#' @param model Model list as output by [model_setup()]
#' @param gear The gear number to plot
#'
#' @return a [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_linerange facet_grid
#' @export
plot_agecomp_fits_splitsex <- function(model, gear){

  fit <- model$mpd$A_hat %>%
    as.data.frame() %>%
    as_tibble() %>%
    `names<-`(1:20)
  comp <- model$mpd$d3_A %>%
    as.data.frame() %>%
    as_tibble() %>%
    `names<-`(c("year", "gear", "area", "group", "sex", 1:20))

  # fit has unlabelled rows, which need to be extracted
  # correctly based on the values in the comp data.frame
  gear_rows <- which(comp$gear == gear)
  comp <- comp[gear_rows, ]
  year_sex <- comp %>% select(year, sex)
  comp <- comp %>%
    select(-c(year, gear, area, group, sex))
  comp <- comp %>%
    as.matrix %>%
    prop.table(1) %>%
    as_tibble()
  comp <- bind_cols(year_sex, comp)
  fit <- fit[gear_rows, ] %>%
    as.matrix %>%
    prop.table(1) %>%
    as_tibble()
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
    geom_line(data = fit, aes(x = Age, y = Proportion, group = 1), color = "blue")

  g
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
  n.side <- get.rows.cols(length(yrs))
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
