#' Verify that models object is a list of iscam models and that models is the same length as models_names
#'
#' @param models a list of iscam model objects
#' @param models_names a vector of names for the models
#'
#' @export
verify_models <- function(models,
                          models_names){
  if(length(models) != length(models_names)){
    stop("models_names must be the same length as models.", call. = FALSE)
  }
  for(i in seq_along(models)){
    if(class(models[[i]]) != mdl_cls){
      stop("Model ", i, " in the list is not of the type ", mdl_cls, call. = FALSE)
    }
  }
}

#' Calculate column quantiles for a data matrix
#'
#' @param data a matrix
#' @param probs a vector of probabilities to be passed to [stats::quantile()]
#'
#' @export
#' @importFrom stats quantile
get.quants <- function(data,
                       probs){

  if(is.null(dim(data))){
    ## It is a single posterior, e.g. sbo
    quants <- quantile(data, probs)
  }else{
    ## It is a timeseries posterior, e.g. sbt
    quants <- apply(data, 2, quantile, probs)
  }
  quants
}

#' Get priors information from prior.str
#'
#' @param prior.str is a string like Lognormal(2.0,1.01)
#' @param dec.points number of decimal points to return
#' @param first.to.lower makes the first letter of the name of the prior lower case
#'
#' @return a vector of length 3, e.g.:"Lognormal", 2.0, 1.01
#' @export
#' @importFrom gfutilities f
split_prior_info <- function(prior.str,
                             dec.points = 1,
                             first.to.lower = FALSE){
  p <- strsplit(prior.str, "\\(")[[1]]
  if(first.to.lower){
    ## Make the name of the prior lower case
    p[1] <- paste0(tolower(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
  }
  p.type <- p[1]
  p <- strsplit(p[2], ",")[[1]]
  p.mean <- f(as.numeric(p[1]), dec.points)
  p.sd <- f(as.numeric(gsub(")", "", p[2])), dec.points)
  return(c(p.type, p.mean, p.sd))
}

#' Get the value at given rank
#'
#' @param vec a vector of values
#' @param rank 1=max, 2-second highest, etc.
#'
#' @return the value at the rank and the index where it was found
#' @export
get.age.prop <- function(vec, rank = 1){
  prop <- rev(sort(vec))
  prop <- prop[ran]
  age <- as.numeric(names(vec[vec == prop]))
  c(age, prop)
}

#' Get a pretty version of the parameter name. Italicizes
#' parameter names.
#'
#' @param name iscam parameter to pretty-up
#'
#' @return an R expression which represents the pretty version of the parameter name
#' @importFrom gfutilities firstup
#' @export
get_latex_name <- function(name){

  if(length(grep("^q_gear[1-9]+$", name))){
    digit <- as.numeric(sub("^q_gear([1-9]+)$", "\\1", name))
    return(substitute(italic(q)[digit], list(digit = digit)))
  }
  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(substitute(italic(q)[digit], list(digit = digit)))
  }

  if(length(grep("^log_q_gear[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q_gear([1-9]+)$", "\\1", name))
    return(substitute("ln("*italic(q)[digit]*")", list(digit = digit)))
  }
  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(substitute("ln("*italic(q)[digit]*")", list(digit = digit)))
  }

  #' Get the pretty name for selectivity parameters
  #' @param nm The parameter name starting with selage or selsd
  #' @return The pretty name or `NULL`
  get_sel_name <- function(nm){
    if(length(grep("selage", nm))){
      j <- sub("selage", "", nm)
      sex <- sub("[0-9]+_", "\\1", j)
      flt <- sub("_female|_male", "\\1", j)
      sexflt <- paste0(firstup(sex), ",", flt)
      bquote(hat(italic(a))[.(sexflt)])
    }else if(length(grep("selsd", nm))){
      j <- sub("selsd", "", nm)
      sex <- sub("[0-9]+_", "\\1", j)
      flt <- sub("_female|_male", "\\1", j)
      sexflt <- paste0(firstup(sex), ",", flt)
      bquote(hat(italic(gamma))[.(sexflt)])
    }else{
      NULL
    }
  }

  if(length(grep("^selage|^selsd", name))){
    return(get_sel_name(name))
  }

  switch(name,
         "ro" = expression(italic(R)[0]),
         "rbar" = expression(bar(italic(R))),
         "rinit" = expression(bar(italic(R))[init]),
         "m" = expression(italic(M)),
         "m1" = expression(italic(M)[Male]),
         "m2" = expression(italic(M)[Female]),
         "bo" = expression("B"[0]),
         "sbo" = expression("SB"[0]),
         "vartheta" = expression(vartheta),
         "rho" = expression(rho),
         "bmsy" = expression("B"[MSY]),
         "msy" = expression("MSY"),
         "msy1" = expression(MSY[1]),
         "msy2" = expression(MSY[2]),
         "msy3" = expression(MSY[3]),
         "msy4" = expression(MSY[4]),
         "msy5" = expression(MSY[5]),
         "fmsy" = expression("F"[MSY]),
         "fmsy1" = expression("F"[MSY1]),
         "fmsy2" = expression("F"[MSY2]),
         "fmsy3" = expression("F"[MSY3]),
         "fmsy4" = expression("F"[MSY4]),
         "fmsy5" = expression("F"[MSY5]),
         "umsy" = expression("U"[MSY]),
         "umsy1" = expression("U"[MSY1]),
         "umsy2" = expression("U"[MSY2]),
         "umsy3" = expression("U"[MSY3]),
         "umsy4" = expression("U"[MSY4]),
         "umsy5" = expression("U"[MSY5]),
         "ssb" = expression("SSB"),
         "log_ro" = expression("ln("*italic(R)[0]*")"),
         "h" = expression(italic("h")),
         "log_m_sex1" = expression("ln("*italic(M)[Male]*")"),
         "log_m_sex2" = expression("ln("*italic(M)[Female]*")"),
         "log_rbar"  = expression("ln("*bar(italic(R))*")"),
         "log_rinit" = expression("ln("*bar(italic(R)[init])*")"))
}

#' Draw a time series envelope on a device on which [plot.new()] has already been called
#'
#' @param yrs the years to plot
#' @param quants a 3-row matrix, where the middle row is the median and the other two are
#' the lower and upper values for some confidence interval.
#' @param col color of the envelope
#' @param first boolean. If TRUE, [plot.new()] will be called. If FALSE, [lines()] will be
#'  called.
#' @param opacity how opaque the envelope shading is. Percentage value
#' @param ... other graphical parameters
#'
#' @export
draw.envelope <- function(yrs,
                          quants,
                          col = "black",
                          first,
                          opacity = 75,
                          ...){

  lower  <- quants[1,]
  median <- quants[2,]
  upper  <- quants[3,]

  if(first){
    plot(yrs,
         median,
         type = "l",
         col = col,
         lty = 1,
         lwd = 2,
         ...)
    shade <- get.shade(col, opacity)
    poly.yrs <- c(yrs, rev(yrs))
    poly.ci    <- c(lower, rev(upper))
    polygon(poly.yrs, poly.ci, col = shade)
  }else{
    lines(yrs,
          median,
          type = "l",
          col = col,
          lty = 1,
          lwd = 2,
          ...)
    ## Upper and lower part of CI
    lines(yrs,
          lower,
          col = col,
          lty = 5,
          lwd = 1)
    lines(yrs,
          upper,
          col = col,
          lty = 5,
          lwd = 1)
  }
}

#' Extract the model class objects from the list of model lists,
#' and merge them into a single model list containing all the model
#' class objects
#'
#' @param ... one or more lists of iscam model objects
#'
#' @return an iscam model object list
#' @export
c.model.list <- function(...){

  lst <- list(...)
  ret.lst <- NULL
  ind <- 1
  for(i in 1:length(lst)){
    if(class(lst[[i]]) != mdl_lst_cls){
      stop("List element ", i, " is not of the class '", mdl_lst_cls, "'.")
    }
    for(j in 1:length(lst[[i]])){
      if(class(lst[[i]][[j]]) != mdl_cls){
        stop("Sublist element ", j, " of list element ", i,
             " is not of the class '", mdl_cls, "'.")
      }
      ret.lst[[ind]] <- lst[[i]][[j]]
      ind <- ind + 1
    }
  }
  class(ret.lst) <- mdl_lst_cls
  ret.lst
}

#' Calculation of sigma and tau from rho and vartheta
#'
#' @description Total variance is given by ϑ (`vartheta`) and the proportion of the total variance that is
#' associated with observation errors is given by ρ (`rho`), the variance is partitioned into
#' observation errors (σ^2) (`sigma`^2) and process errors (τ^2) (`tau`^2)
#'
#' @param rho parameter rho from iscam model
#' @param vartheta parameter vartheta from iscam model
#'
#' @return a list of length 2, the calculated tau and sigma parameters
#' @export
calc_sig_tau <- function(rho, vartheta){

  tau <- sqrt((1 - rho) / vartheta)
  sigma <- sqrt(rho / vartheta)
  list(tau, sigma)
}

#' Calculate the number of rows and columns to have in a grid for plotting multiple figures
#'
#' @param num the number of figures
#'
#' @return a vector of length 2 representing the number of rows and columns
#' @export
get_rows_cols <- function(num){
  if(num <= 64 && num > 49){
    if(num <= 56){
      nside <- c(8,7)
    }else{
      nside <- c(8,8)
    }
  }else if(num <= 49 && num > 36){
    if(num <= 42){
      nside <- c(7,6)
    }else{
      nside <- c(7,7)
    }
  }else if(num <= 36 && num > 25){
    if(num <= 30){
      nside <- c(6,5)
    }else{
      nside <- c(6,6)
    }
  }else if(num <= 25 && num > 16){
    if(num <= 20){
      nside <- c(5,4)
    }else{
      nside <- c(5,5)
    }
  }else if(num <= 16 && num > 9){
    if(num <= 12){
      nside <- c(4,3)
    }else{
      nside <- c(4,4)
    }
  }else if(num <=  9 && num > 4){
    if(num <= 6){
      nside <- c(3,2)
    }else{
      nside <- c(3,3)
    }
  }else if(num <=  4 && num > 1){
    if(num == 2){
      nside <- c(2,1)
    }else{
      nside <- c(2,2)
    }
  }else{
    nside <- c(1,1)
  }
  return(nside)
}

#' Change numbers into words (1:9)
#' @param x x
num_to_word <- function( x ) {
  # Get the list of numbers and words
  vec <- c( one=1, two=2, three=3, four=4, five=5, six=6, seven=7, eight=8,
            nine=9 )
  # Get the index corresponding to the number
  ind <- which( vec == x )
  # Get the name
  res <- names( vec[ind] )
  # Error if the name isn't there
  if( length(res)==0 ) stop( "Numbers can be from 1 to 9 only." )
  res
}

#' Read psv file
#' @param fn filename
#' @param nsamples num samples
read.psv <- function(fn, nsamples = 10000){
  #This function reads the binary output from ADMB
  #-mcsave command line option.
  #fn = paste(ifile,'.psv',sep='')
  filen <- file(fn, "rb")
  nopar <- readBin(filen, what = integer(), n = 1)
  mcmc <- readBin(filen, what = numeric(), n = nopar * nsamples)
  mcmc <- matrix(mcmc, byrow = TRUE, ncol = nopar)
  close(filen)
  return(mcmc)
}
