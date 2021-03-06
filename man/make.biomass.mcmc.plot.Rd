% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures-biomass.R
\name{make.biomass.mcmc.plot}
\alias{make.biomass.mcmc.plot}
\alias{make.depletion.mcmc.plot}
\title{Plot the biomass trajectories for iscam models}
\usage{
make.biomass.mcmc.plot(
  models,
  model.names = NULL,
  ylim,
  opacity = 75,
  offset = 0.1,
  append.base.txt = NULL,
  show.bmsy.line = FALSE,
  show.bo.line = FALSE,
  ind.letter = NULL,
  leg = NULL,
  ...
)

make.depletion.mcmc.plot(
  models,
  model.names = NULL,
  ylim = c(0, 1),
  opacity = 75,
  append.base.txt = NULL,
  ind.letter = NULL,
  leg = NULL,
  ...
)
}
\arguments{
\item{models}{A list of iscam model objects}

\item{model.names}{A vector of names to show on the legend}

\item{ylim}{The y limits for the plot}

\item{opacity}{How opaque the credibility envelopes are}

\item{offset}{The amount on the x-axis to offset each point and line for
multiple models}

\item{append.base.txt}{Text to append to the name of the first model}

\item{show.bmsy.line}{Show the reference lines 0.4 and 0.8bmsy}

\item{show.bo.line}{Show the reference lines 0.2 and 0.4bo}

\item{ind.letter}{A letter to show on the plot (for panel plots)}

\item{leg}{Position of the legend. NULL means no legend is shown}

\item{...}{Other graphical arguments}
}
\value{
Nothing
}
\description{
Plot the biomass with credibility intervals for the mcmc
cases of the models

Plot the relative biomass with credibility intervals for the mcmc
cases of the models
}
