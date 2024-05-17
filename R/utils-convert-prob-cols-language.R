#' Convert any columns in a data frame that appear as 22.5% (for example)
#' to 22,5% which is how it appears in French
#'
#' @param d A data frame
#'
#' @return `d`, but with column names modified as described in the function
#' title
#' @export
convert_prob_cols_language <- function(d){

  if(fr()){
    nms <- names(d)
    pat <- "^([[:digit:]]+)(\\.[[:digit:]]+)?%"
    prob_cols <- grep(pat, nms)
    if(!length(prob_cols)){
      return(d)
    }
    nms[prob_cols] <- gsub("\\.", ",", nms[prob_cols])
    names(d) <- nms
  }

  d
}
