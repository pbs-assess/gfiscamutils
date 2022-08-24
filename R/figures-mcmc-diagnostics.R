#' Plot priors and posteriors for the given isam model
#'
#' @param model An iscam model object
#' @param priors.only Plot the priors only? TRUE/FALSE
#'
#' @details Plots the priors overlaid on the posteriors for the given iscm model.
#'   If priors.only is TRUE, only the priors will be plotted.
#'   The values in the control file (model$ctl$params) for each
#'   prior are:
#'   1. ival  = initial value
#'   2. lb    = lower bound
#'   3. ub    = upper bound
#'   4. phz   = ADMB phase
#'   5. prior = prior distribution funnction
#'          0 = Uniform
#'          1 = normal    (p1=mu,p2=sig)
#'          2 = lognormal (p1=log(mu),p2=sig)
#'          3 = beta      (p1=alpha,p2=beta)
#'          4 = gamma     (p1=alpha,p2=beta)
#'   6. p1 (defined by 5 above)
#'   7. p2 (defined by 5 above)
#'
#' @return Nothing
#' @export
make.priors.posts.plot <- function(model,
                                   priors.only = TRUE){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  f.names <- c(dunif, dnorm, dlnorm, dbeta, dgamma)

  mc <- model$mcmccalcs$p.dat.log
  ## Remove selectivity parameters, bo, vartheta, sigma, tau from the posts
  sel_ind <- grep("^sel.*", names(mc))
  if(length(sel_ind)){
    mc <- mc[, -sel_ind]
  }
  bo_ind <- grep("bo", names(mc))
  if(length(bo_ind)){
    mc <- mc[, -bo_ind]
  }
  vartheta_ind <- grep("vartheta", names(mc))
  if(length(vartheta_ind)){
    mc <- mc[, -vartheta_ind]
  }
  tau_ind <- grep("tau", names(mc))
  if(length(tau_ind)){
    mc <- mc[, -tau_ind]
  }
  sigma_ind <- grep("sigma", names(mc))
  if(length(sigma_ind)){
    mc <- mc[, -sigma_ind]
  }
  post.names <- names(mc)

  prior.specs <- as.data.frame(model$ctl$params)
  ## Remove fixed parameters
  prior.specs <- prior.specs[prior.specs$phz > 0,]
  ## Remove upper and lower bound, and phase information, but keep initial
  ##  value
  prior.specs <- prior.specs[, -c(2:4)]
  ## Remove kappa
  prior.specs <- prior.specs[rownames(prior.specs) != "kappa",]
  prior.names <- rownames(prior.specs)

  ## Add the q parameters to the prior specs table
  q.params <- model$ctl$surv.q
  num.q.params <- ncol(q.params)
  q.specs <- lapply(1:num.q.params,
                    function(x){
                      c(q.params[2, x],
                        q.params[1, x],
                        q.params[2, x],
                        q.params[3, x])
                    })
  q.specs <- as.data.frame(do.call(rbind, q.specs))
  rownames(q.specs) <- paste0("log_q", 1:num.q.params)
  colnames(q.specs) <- colnames(prior.specs)
  prior.specs <- rbind(prior.specs, q.specs)

  ## Get MPD estimates for the parameters in the posteriors
  mpd <- model$mpd
  q.pattern <- "^log_q([1-9]+)$"
  mpd.lst <- lapply(1:length(post.names),
                    function(x){
                      mle <- NULL
                      p.name <- post.names[x]
                      if(p.name == "log_m1" | p.name == "log_m"){
                        mle <- log(mpd$m[1])
                      }else if(p.name == "log_m2"){
                        mle <- log(mpd$m[2])
                      }else if(p.name == "h"){
                        mle <- mpd$steepness
                      }else if(length(grep(q.pattern, p.name)) > 0){
                        num <- as.numeric(sub(q.pattern, "\\1", p.name))
                        mle <- log(mpd$q[num])
                      }else{
                        mle <- as.numeric(mpd[post.names[x]])
                      }
                      mle})
  mpd.param.vals <- do.call(c, mpd.lst)
  names(mpd.param.vals) <- post.names

  n.side <- get.rows.cols(length(post.names))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  for(i in 1:length(post.names)){
    specs <- prior.specs[i,]
    prior.fn <- f.names[[as.numeric(specs[2] + 1)]]
    xx <- list(p = mc[,i],
               p1 = as.numeric(specs[3]),
               p2 = as.numeric(specs[4]),
               fn = prior.fn,
               nm = post.names[i],
               mle = as.numeric(mpd.param.vals[i]))

    xx$nm <- get.latex.name(xx$nm)

    if(priors.only){
      func <- function(x){xx$fn(x, xx$p1, xx$p2)}
      if(specs[2] == 0){
        ## Uniform, plot from p1-1 to p2+1
        curve(func,
              from = xx$p1 - 1,
              to = xx$p2 + 1,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }else if(specs[2] == 1){
        ## Normal, plot from -(p1-p2*4) to (p1+p2*4)
        curve(func,
              from = xx$p1 - 4 * xx$p2,
              to = xx$p2 + 4 * xx$p2,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }else{
        curve(func,
              xlab = "",
              ylab = "",
              col = "black",
              lwd = 2)
      }
    title(xx$nm)
    }else{
      plot_marg(xx,
                breaks = "sturges",
                col = "wheat")
    }
  }
  mtext(text = "Parameter value", side = 1, line = 1, outer = TRUE)
  mtext(text = "Frequency", side = 2, line = -0.5, outer = TRUE)
}

#' Plot posterior histograms
#'
#' @param xx A list containing values used to plot a posterior histogram.
#'   See [make.biomass.mcmc.plot()] to see how this is constructed
#' @param breaks Same as the 'breaks' argument in [graphics::hist()]
#' @param ex.factor A factor to change the x-axis range
#' @param ... Other graphical arguments
#'
#' @export
plot_marg <- function(xx,
                      breaks = "sturges",
                      ex.factor = 1.0,
                      ...){
  post.no.plot <- hist(as.matrix(xx$p),
                       breaks = breaks,
                       plot = FALSE)
  xvals <- seq(min(post.no.plot$breaks) / ex.factor,
               max(post.no.plot$breaks) / ex.factor,
               length = 1000)
  pd <- xx$fn(xvals, xx$p1, xx$p2)
  z <- cbind(xvals, pd)

  xlim <- c(min(xvals), max(xvals))
  ss <- hist(as.matrix(xx$p),
             prob = TRUE,
             breaks = breaks,
             main = xx$nm,
             xlab = "",
             cex.axis = 1.2,
             xlim = xlim,
             ylab = "",
             ...)
  func <- function(x){xx$fn(x, xx$p1, xx$p2)}
  ## Plot prior
  curve(func,
        xlim[1],
        xlim[2],
        xlab = "",
        ylab = "",
        col = "black",
        lwd = 2,
        add = TRUE)
  ## Plot MPD
  abline(v = xx$mle,
         lwd = 2,
         lty = 2,
         col = 2)
}

#' Make trace plots for all paramaters from the mcmc output
#'
#' @param model An iscam model object
#' @param axis.lab.freq the frequency of x-axis labelling
#'
#' @export
make.traces.plot <- function(model,
                             mcmc_burnin = 1000,
                             axis.lab.freq = 200){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmc$params.est
  n.side <- get.rows.cols(ncol(mc))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  for(param in 1:ncol(mc)){
    mcmc.trace <- as.matrix(mc[mcmc_burnin:(length(mc[,1])-1),param])
    name <- colnames(mc)[param]
    name <- get.latex.name(name)
    plot(mcmc.trace,
         main = name,
         type = "l",
         ylab = "",
         xlab = "",
         axes = FALSE)
    mtext(text = "Iteration", side = 1, line = 1, outer = TRUE)
    mtext(text = "Parameter value", side = 2, line = -1, outer = TRUE)
    box()
    at <- labels <- seq(0,
                        nrow(mc),
                        axis.lab.freq)
    axis(1,
         at = at,
         labels = labels)
    axis(2)
  }
}

#' Plot the autocorrelation of estimated parameters
#'
#' @param model An iscam model object
#' @param mcmc_burnin Number for burnin
#' @param lag_max Maximum value at which to calculate acf
#'
#' @return Nothing
#' @export
make.autocor.plot <- function(model,
                              mcmc_burnin = 1000,
                              lag_max = 100){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmc$params.est
  n.side <- get.rows.cols(ncol(mc))
  par(mfrow = n.side,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  for(param in 1:ncol(mc)){
    mcmc.autocor <- as.matrix(mc[mcmc_burnin:(length(mc[,1])-1),param])
    name <- colnames(mc)[param]
    name <- get.latex.name(name)
    autocorr.plot(mcmc.autocor,
                  lag.max = lag_max,
                  main = name,
                  auto.layout = FALSE)
  }
}

#' autocorr.plot from coda package, but the package source had the
#' ylab = "Autocorrelation" for all plots and no way to override it.
#' That caused latex to place the plot in landscape mode which was ugly.
#'
#' @param x A Markov Chain
#' @param lag.max Maximum value at which to calculate acf
#' @param auto.layout If TRUE then, set up own layout for plots, otherwise use existing one.
#' @param ask If TRUE then prompt user before displaying each page of plots. Default is
#'  dev.interactive() in R and interactive() in S-PLUS.
#' @param ... graphical parameters
#'
#' @export
#' @importFrom grDevices dev.interactive
#' @importFrom coda is.mcmc.list mcmc.list as.mcmc nvar varnames chanames nchain
#' @importFrom stats acf
autocorr.plot <- function(x,
                          lag.max,
                          auto.layout = TRUE,
                          ask,
                          ...){
  if (missing(ask)){
    ask <- if(is.R()) {
      dev.interactive()
    }else{
      interactive()
    }
  }
  oldpar <- NULL
  on.exit(par(oldpar))
  if(auto.layout)
    oldpar <- par(mfrow = set.mfrow(Nchains = nchain(x),
                                    Nparms = nvar(x)))
  if(!is.mcmc.list(x))
    x <- mcmc.list(as.mcmc(x))
  for(i in 1:nchain(x)) {
    xacf <- if(missing(lag.max))
              acf(as.tsmcmc(x[[i]]),
                  plot = FALSE)
            else
              acf(as.tsmcmc(x[[i]]),
                  lag.max = lag.max,
                  plot = FALSE)
    for(j in 1:nvar(x)){
      plot(xacf$lag[, j, j],
           xacf$acf[, j, j],
           type = "h",
           ylab = "",
           xlab = "Lag",
           ylim = c(-1, 1), ...)
      mtext(text = "Lag", side = 1, line = 1, outer = TRUE)
      mtext(text = "Autocorrelation", side = 2, line = -1, outer = TRUE)
      title(paste0(varnames(x)[j],
                  ifelse(is.null(chanames(x)),
                         "",
                         ":"),
                  chanames(x)[i]))
      if(i == 1 & j == 1)
        oldpar <- c(oldpar, par(ask = ask))
    }
  }
  invisible(x)
}

#' Copied from coda package source, to fulfill
#'  autocorr.plot requirement
#'
#' @param x an mcmc object
#' @param ... unused arguments for compatibility with [coda::as.ts.mcmc()]
#'
#' @importFrom coda as.mcmc thin
#' @importFrom stats ts start end
#' @export
as.tsmcmc <- function(x, ...){

  x <- as.mcmc(x)
  y <- ts(x,
          start = start(x),
          end = end(x),
          deltat = thin(x))
  attr(y, "mcpar") <- NULL
  y
}

#'  Plot the pairs scatterplots for the estimated parameters
#'
#' @param model An iscam model object
#' @param params Parameters to include in plot
#'
#' @return Nothing
#' @export
make.pairs.plot <- function(model,
                            params = c("ro",
                                       "h",
                                       "m",
                                       "rbar",
                                       "rinit",
                                       "q1",
                                       "q2",
                                       "sel1",
                                       "selsd1",
                                       "sel2",
                                       "selsd2",
                                       "sel4",
                                       "selsd4",
                                       "sel5",
                                       "selsd5")){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  panel.hist <- function(x, ...){
    usr    <- par("usr");
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h      <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y      <- h$counts; y <- y/max(y)
    rect(breaks[-nB],
         0,
         breaks[-1],
         y,
         col="wheat",
         cex=0.75,
         ...)
  }

  mc <- as.data.frame(model$mcmc$params.est)

  mc <- mc[,params]
  c.names <- colnames(mc)
  latex.names <- NULL
  for(param in 1:ncol(mc)){
    name <- c.names[param]
    latex.names <- c(latex.names, get.latex.name(name))
  }
  pairs(mc,
        labels = latex.names,
        cex.labels = 1.0,
        pch = ".",
        upper.panel = panel.smooth,
        diag.panel = panel.hist,
        lower.panel = panel.smooth,
        gap = 0.0)
  mtext(text = "Parameter value", side = 1, line = 0, outer = TRUE)
  mtext(text = "Parameter value", side = 2, line = -1, outer = TRUE)
}
