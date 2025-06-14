% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table-param-settings.R
\name{table_param_settings}
\alias{table_param_settings}
\title{Create a table summarizing parameterization of an iSCAM model}
\usage{
table_param_settings(
  model,
  ret_df = FALSE,
  col_widths = "5em",
  bold_header = TRUE,
  ...
)
}
\arguments{
\item{model}{A single output model}

\item{ret_df}{Logical. If \code{TRUE} return a data frame with the values,
instead of the \code{\link[knitr:kable]{knitr::kable()}} formatted table}

\item{col_widths}{The width of all columns other than the first. If \code{NULL},
do not set}

\item{bold_header}{Logical. If \code{TRUE} make the table header row boldface}

\item{...}{Arguments to pass to \code{\link[csasdown:csas_table]{csasdown::csas_table()}}}
}
\description{
Create a table showing number of parameters estimated and prior
parameterizations
}
