#' Fetch a data frame of the estimated MCMC parameters only
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details If all values in a given column are different, it is assumed that
#'   the parameter was estimated.
#'
#' @return A data frame of estimated parameters
#' @export
get_estimated_params <- function(mc){

  posts <- apply(mc,
                 2,
                 function(x){
                   if(length(unique(x)) > 1)
                     return(x)
                 })
  # Remove NULL list elements (fixed parameters)
  posts_lst <- posts[!sapply(posts, is.null)]
  do.call(cbind, posts_lst)
}
