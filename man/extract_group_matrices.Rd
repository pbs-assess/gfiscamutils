% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-group-matrices.R
\name{extract_group_matrices}
\alias{extract_group_matrices}
\title{Extract the given data frame into a list of matrices by iscam 'group'}
\usage{
extract_group_matrices(data = NULL, prefix = NULL)
}
\arguments{
\item{data}{A data frame read in from one of the MCMC csv output files}

\item{prefix}{See details}
}
\value{
A list of matrices, one element per group
}
\description{
Extract the given data frame into a list of matrices by iscam 'group'
}
\details{
Extract the data frame given (data) by unflattening into a list of matrices
by group. The group number is located in the names of the columns of the
data frame in this format: "prefix(groupnum)_year" where (groupnum) is one
or more digits representing the group number and prefix is the string
given as an argument to the function.
}
