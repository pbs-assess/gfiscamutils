% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load-models.R
\name{load.iscam.files}
\alias{load.iscam.files}
\title{Construct an iscam model object from its input and output files}
\usage{
load.iscam.files(model.dir, mcmc.subdir = "mcmc", ...)
}
\arguments{
\item{model.dir}{The directory the model is in}

\item{mcmc.subdir}{subdirectory in which mcmc results for models are stored.
Can be the empty string in which case they will be in the model's root directory}

\item{...}{arguments to pass to [calc.mcmc()]}
}
\value{
An iscam model object
}
\description{
Construct an iscam model object from its input and output files
}
\details{
Load all the iscam files for output and input, and return the model object
  If MCMC directory is present, load that and perform calculations for mcmc
 parameters.
}
