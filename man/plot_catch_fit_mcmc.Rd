% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-catch-fit-mcmc.R
\name{plot_catch_fit_mcmc}
\alias{plot_catch_fit_mcmc}
\title{Plot catch fit against catch data}
\usage{
plot_catch_fit_mcmc(
  model,
  append_base_txt = "",
  xlim = NULL,
  ylim = NULL,
  leg_loc = c(0.95, 0.95),
  angle_x_labels = FALSE,
  probs = c(0.025, 0.5, 0.975)
)
}
\arguments{
\item{model}{An iscam model object (\code{mdl_cls})}

\item{append_base_txt}{A vector of strings to append to the model names for
display on the plot legend or title}

\item{xlim}{The x limits for the plot. If \code{NULL}, the limits of the data
will be used}

\item{ylim}{The y limits for the plot. If \code{NULL}, the limits of the data
will be used}

\item{leg_loc}{A two-element vector describing the X-Y values between 0 and
1 to anchor the legend to. eg. c(1, 1) is the top right corner and c(0, 0)
is the bottom left corner}

\item{angle_x_labels}{If \code{TRUE} put 45 degree angle on x-axis tick labels}

\item{probs}{A 3-element vector of probabilities that appear in the output
data frames. This is provided in case the data frames have more than three
different quantile levels}
}
\description{
Plot catch fit against catch data
}
\seealso{
Other Time series plotting functions: 
\code{\link{plot_biomass_grid_mcmc}()},
\code{\link{plot_biomass_mcmc}()},
\code{\link{plot_biomass_mpd}()},
\code{\link{plot_biomass_proj_mcmc}()},
\code{\link{plot_f_mcmc}()},
\code{\link{plot_index_mcmc}()},
\code{\link{plot_index_mpd}()},
\code{\link{plot_q_mcmc}()},
\code{\link{plot_rdevs_mcmc}()},
\code{\link{plot_recr_grid_mcmc}()},
\code{\link{plot_recr_mcmc}()},
\code{\link{plot_recr_mpd}()},
\code{\link{plot_ts_mcmc}()},
\code{\link{plot_vuln_mcmc}()}
}
\concept{Time series plotting functions}
