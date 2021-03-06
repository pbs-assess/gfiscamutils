% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures-mcmc-diagnostics.R
\name{autocorr.plot}
\alias{autocorr.plot}
\title{autocorr.plot from coda package, but the package source had the
ylab = "Autocorrelation" for all plots and no way to override it.
That caused latex to place the plot in landscape mode which was ugly.}
\usage{
autocorr.plot(x, lag.max, auto.layout = TRUE, ask, ...)
}
\arguments{
\item{x}{A Markov Chain}

\item{lag.max}{Maximum value at which to calculate acf}

\item{auto.layout}{If TRUE then, set up own layout for plots, otherwise use existing one.}

\item{ask}{If TRUE then prompt user before displaying each page of plots. Default is
dev.interactive() in R and interactive() in S-PLUS.}

\item{...}{graphical parameters}
}
\description{
autocorr.plot from coda package, but the package source had the
ylab = "Autocorrelation" for all plots and no way to override it.
That caused latex to place the plot in landscape mode which was ugly.
}
