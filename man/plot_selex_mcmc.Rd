% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-selex-mcmc.R
\name{plot_selex_mcmc}
\alias{plot_selex_mcmc}
\title{Plot MCMC selectivities for iSCAM models}
\usage{
plot_selex_mcmc(
  model,
  gear = NULL,
  probs = c(0.025, 0.5, 0.975),
  show_maturity = FALSE,
  ages = as.character(model$dat$start.age:model$dat$end.age),
  breaks = seq(0, model$dat$end.age, 5),
  ci_type = c("both", "line", "ribbon"),
  ci_linetype = c("dotted", "solid", "dashed", "dotdash", "longdash", "twodash"),
  ci_alpha = 0.3,
  leg_loc = c(1, 1),
  angle_x_labels = FALSE
)
}
\arguments{
\item{model}{An iscam model object (class \link{mdl_cls})}

\item{gear}{A gear number to plot}

\item{probs}{A 3-element vector of probabilities that appear in the output data frames
This is provided in case the data frames have more than three different quantile levels}

\item{show_maturity}{If \code{TRUE}, overlay the maturity ogive on the selectivity plots}

\item{ages}{A character vector from 1 to the maximum age to show on
the plot. Defaults to the max age in the model. This may be needed when
The selectivity goes past the maximum age, so that we may see the
selectivity curve}

\item{breaks}{A vector representing the tick locations on the x-axis}

\item{ci_type}{One of "line", "ribbon", "both" to show the credible interval}

\item{ci_linetype}{See \code{linetype} in \link{ggplot2}. Only used if \code{ci_type} is "line" or "both"}

\item{ci_alpha}{Opacity between 0 and 1 for the credible intervals ribbons. Only used if
\code{ci_type} is "ribbon" or "both"}

\item{leg_loc}{A two-element vector describing the X-Y values between 0 and
1 to anchor the legend to. eg. c(1, 1) is the top right corner and c(0, 0)
is the bottom left corner. It can also be the string "facet" in which case
the legend will appear in the empty facet if it exists.}

\item{angle_x_labels}{If \code{TRUE} put 45 degree angle on x-axis tick labels}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Plot the selectivity for any combination of gears in an iscam model
}
\seealso{
Other Selectivity plotting functions: 
\code{\link{plot_selex_gear_mcmc}()},
\code{\link{plot_selex_mpd}()},
\code{\link{plot_tv_selex_mpd}()}
}
\concept{Selectivity plotting functions}
