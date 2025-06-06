% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-sens-q.R
\name{table_sens_q}
\alias{table_sens_q}
\title{Create a table comparing estimates of q across models}
\usage{
table_sens_q(
  models,
  model.names,
  digits = 3,
  xcaption = "default",
  xlabel = "default",
  font.size = 9,
  space.size = 10,
  placement = "H"
)
}
\arguments{
\item{models}{A list of iscam model objects (class \link{mdl_lst_cls})}

\item{model.names}{A vector of nice names (for plotting) for the \code{models}}

\item{digits}{Number of decimal points to show in table}

\item{xcaption}{Caption}

\item{xlabel}{Latex label}

\item{font.size}{Size of font in table}

\item{space.size}{Size of spaces between cells}

\item{placement}{See \code{\link[xtable:print.xtable]{xtable::print.xtable()}}}
}
\value{
an \code{\link[xtable:xtable]{xtable::xtable()}} object
}
\description{
Make a table of the values of q, including quantiles
}
