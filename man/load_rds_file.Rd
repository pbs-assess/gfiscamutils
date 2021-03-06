% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create-rds-file.R
\name{load_rds_file}
\alias{load_rds_file}
\title{Load RDS file which was generated by [create_rds_file()]}
\usage{
load_rds_file(model_dir = NULL)
}
\arguments{
\item{model_dir}{Directory name of model to be loaded}
}
\value{
The results of [base::readRDS()] or `NULL` if the file does not exist
}
\description{
Load RDS file which was generated by [create_rds_file()]
}
