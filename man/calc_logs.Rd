% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc-logs.R
\name{calc_logs}
\alias{calc_logs}
\title{Calculate logs for several parameters using MCMC}
\usage{
calc_logs(
  mc,
  log.params = c("^ro$", "^m$", "^m_sex1$", "^m_sex2$", "^rbar$", "^rinit$",
    "^q_gear[1-9]+$")
)
}
\arguments{
\item{mc}{A data frame of posteriors as seen in the MCMC output csv files}

\item{log.params}{A vector of regular expressions to determine which
parameters to apply the log function to}
}
\value{
a data frame (mc with the log columns appended)
}
\description{
Calculate logs for several parameters using MCMC
}
\details{
The column names will be prepended with log_ for the parameters which
had the log function applied
}
