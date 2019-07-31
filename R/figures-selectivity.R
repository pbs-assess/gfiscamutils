#' Plot the selectivity for selected gears for the iscam model
#'
#' @param model An iscam model object
#' @param gear.names A vector of the names of the gears to include.
#'   Typically read in from the csv file in the data directory
#' @param show.mat plot the maturity as well
#' @param bio used for maturity data is show.mat is TRUE
#' @param leg Position of the legend. NULL means no legend is shown
#' @param color.brew.class RColorBrewer class. e.g. "Paired"
#'
#' @return Nothing
#' @export
#' @importFrom RColorBrewer brewer.pal
make.selex.comparison.plot <- function(model,
                                       gear.names,
                                       show.mat = FALSE,
                                       bio = NULL,
                                       leg = NULL,
                                       color.brew.class = "Paired"){

  ind <- 1:nrow(gear.names)

  selex <- model$mpd$sel
  est.phz <- model$ctl$sel[6,]

  gear.names <- gear.names[ind, 2]
  ## Append " (Fixed)" to indices which have a negative phase
  gear.names[est.phz < 0] <- paste0(gear.names[est.phz < 0], " (Fixed)")

  age <- model$mpd$age
  log.sel.dat <- model$mpd$log_sel

  sel.lst <- split.data.frame(log.sel.dat, f = log.sel.dat[,1])
  sel.lst <- lapply(sel.lst,
                    function(x){
                      exp(x[nrow(x), -c(1,2,3)])
                    })

  ## Make matrix for plotting
  sel.mat <- t(do.call(rbind, sel.lst))

  col <- brewer.pal(ncol(sel.mat), color.brew.class)

  lty <- rep(1, ncol(sel.mat))
  lwd <- rep(2, ncol(sel.mat))

  matplot(age,
          sel.mat,
          type = "l",
          lwd = lwd,
          col = col,
          lty = lty,
          las = 1,
          xlim = c(1, max(age)),
          ylim = c(0, 1.1),
          ylab = "Selectivity",
          xlab = "Age")

  if(show.mat){
    if(is.null(bio)){
      warning("bio is NULL when you specified show.mat = TRUE.")
      return(NULL)
    }
    ## Add maturity ogive to selectivity plot
    ## Plots female only - number 2 in next line signifies female
    data <- bio$ma
    sex <- 2
    a50 <- data[[sex]][[2]][1,]
    sigma_a50 <- data[[sex]][[2]][2,]
    if(is.null(a50) | is.null(sigma_a50)){
      warning("element 'ma' of object 'bio' does not exist. Run the ",
              "maturity/age model from the Biotool tab.")
      return(NULL)
    }
    gear.names <- c(gear.names, "Female maturity")
    col <- c(col, ncol(sel.mat) + 1)
    lty <- c(lty, 2)
    lwd <- c(lwd, 3)
    curve(1 / (1 + exp(-(x - a50) / sigma_a50)),
          col = "black",
          lty = 2,
          lwd = 3,
          add = TRUE)

  }
  if(!is.null(leg)){
    legend(leg,
           legend = gear.names,
           col = col,
           lty = lty,
           lwd = lwd)
  }
}

