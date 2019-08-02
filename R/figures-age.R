#' Plot proportions-at-age time series from a data frames as extracted from iscam data (dat) files
#'
#' @param df a data frame as constructed by [get_pa()]
#' @param age_plus age plus group
#' @param conf confidence value for the envelope
#' @param xlim limits for the years shown on the plot
#' @param ylim limits for the ages shown on the plot
#' @param translate Logical. If TRUE, translate to French
#'
#' @return A ggplot object
#' @importFrom dplyr filter as_tibble rename select group_by ungroup summarize
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line geom_point coord_cartesian expand_limits
#'  labs xlab ylab facet_wrap geom_ribbon
#' @importFrom stats qnorm
#' @export
plot_pa <- function(df,
                    age_plus = 10,
                    conf = 0.9,
                    xlim = c(1000, 3000),
                    ylim = c(0, NA),
                    translate = FALSE){
  df <- df %>%
    filter(year >= xlim[1])
  dfm <- melt(df, id.vars = c("year", "area", "group", "sex", "region", "Gear")) %>%
    as_tibble() %>%
    rename(Region = region,
           Year = year,
           Age = variable,
           Number = value) %>%
    select(-c(area, group, sex)) %>%
    mutate(Age = as.numeric(as.character(Age)),
           Age = ifelse(Age > age_plus, age_plus, Age)) %>%
    group_by(Region, Year, Age) %>%
    summarize(Number = sum(Number)) %>%
    mutate(Proportion = Number / ifelse(all(is.na(Number)), NA, sum(Number, na.rm = TRUE))) %>%
    ungroup() %>%
    mutate(Age = factor(Age))

  # Determine weighted mean and approximate CI age by year
  dfm_ci <- dfm %>%
    select(Region, Year, Age, Proportion) %>%
    mutate(Age = as.numeric(Age)) %>%
    group_by(Region, Year) %>%
    summarize(MeanAge = weighted.mean(x = Age, w = Proportion),
              sBar = qnorm(1 - (1 - conf) / 2) * sum(sqrt(Proportion * (1 - Proportion)) / sqrt(Age)),
              Lower = exp(log(MeanAge) - log(sBar)),
              Upper = exp(log(MeanAge) + log(sBar))) %>%
    ungroup() %>%
    mutate(GroupID = consecutive_group(Year))

  g <- ggplot(dfm, aes(x = Year)) +
    geom_point(aes(y = Age, size = ifelse(Proportion, Proportion, NA))) +
    geom_path(data = dfm_ci, aes(y = MeanAge, group = GroupID), size = 2) +
    geom_ribbon(data = dfm_ci,
                aes(ymin = Lower, ymax = Upper, group = GroupID),
                alpha = 0.25) +
    coord_cartesian(xlim, ylim) +
    expand_limits(x = xlim[1]:xlim[2]) +
    labs(size = en2fr("Proportion", translate)) +
    ylab(en2fr("Age", translate)) +
    xlab(en2fr("Year", translate)) +
    facet_wrap(~ Region, ncol = 2, dir = "v", scales = "free_y" ) +
    theme(legend.position="top")
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
