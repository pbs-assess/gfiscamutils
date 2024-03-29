% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{tr}
\alias{tr}
\title{Translate from English to French}
\usage{
tr(x, ...)
}
\arguments{
\item{x}{A character vector to translate}

\item{...}{Arguments to pass to \code{\link[rosettafish:trans]{rosettafish::en2fr()}}}
}
\value{
The possibly translated character vector
}
\description{
Translate from English to French
}
\details{
If options(french) has been set, the translation will happen,
If options(french) is \code{NULL} or \code{FALSE}, no translation will happen
}
