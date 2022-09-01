#' Change the column name 'm1' to 'm' unless both 'm1' and 'm2' exist
#'
#' @param mc A data frame of posteriors as seen in the MCMC output csv files
#'
#' @details Fo column names of data frame mc:
#'   If m1 and m2 both exist, no change
#'   If only m1 exists, change the name to m
#'
#' @return A data frame the same as mc but with modifications (see details)
#' @export
fix_m <- function(mc){

  grp <- grep("m[12]", colnames(mc))
  if(length(grp) == 1){
    colnames(mc)[grp] <- "m"
  }
  mc
}
