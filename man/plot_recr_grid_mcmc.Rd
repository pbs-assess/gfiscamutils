% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-recr-grid-mcmc.R
\name{plot_recr_grid_mcmc}
\alias{plot_recr_grid_mcmc}
\title{Plot a grid of MCMC recruitment plots for iSCAM models}
\usage{
plot_recr_grid_mcmc(models, ...)
}
\arguments{
\item{models}{A list of iscam model objects (class \link{mdl_lst_cls})}

\item{...}{Arguments passed to \code{\link[=plot_biomass_mcmc]{plot_biomass_mcmc()}} and \code{\link[=plot_recr_mcmc]{plot_recr_mcmc()}}}
}
\value{
A \code{\link[cowplot:plot_grid]{cowplot::plot_grid()}} object
}
\description{
Plot a grid of recruitment plots, each with one model only so that
the R0 line and credible intervals can be viewed
}
\seealso{
Other Time series plotting functions: 
\code{\link{plot_biomass_grid_mcmc}()},
\code{\link{plot_biomass_mcmc}()},
\code{\link{plot_biomass_mpd}()},
\code{\link{plot_biomass_proj_mcmc}()},
\code{\link{plot_catch_fit_mcmc}()},
\code{\link{plot_f_mcmc}()},
\code{\link{plot_index_mcmc}()},
\code{\link{plot_index_mpd}()},
\code{\link{plot_q_mcmc}()},
\code{\link{plot_rdevs_mcmc}()},
\code{\link{plot_recr_mcmc}()},
\code{\link{plot_recr_mpd}()},
\code{\link{plot_ts_mcmc}()},
\code{\link{plot_vuln_mcmc}()}
}
\concept{Time series plotting functions}
