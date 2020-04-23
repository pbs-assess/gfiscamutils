#' gfiscamutils package
#'
#' See the README on
#' \href{https://github.com/pbs-assess/gfiscamutils#readme}{GitHub}
#'
#' @docType package
#' @name gfiscamutils

#' @importFrom rlang .data
#' @importFrom stats quantile
#' @importFrom utils capture.output
#'
NULL

# from: https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
# quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c("."))
