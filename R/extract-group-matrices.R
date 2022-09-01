#' Extract the given data frame into a list of matrices by iscam 'group'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by group. The group number is located in the names of the columns of the
#'   data frame in this format: "prefix[groupnum]_year" where [groupnum] is one
#'   or more digits representing the group number and prefix is the string
#'   given as an argument to the function.
#'
#' @return A list of matrices, one element per group
#' @export
extract_group_matrices <- function(data = NULL,
                                   prefix = NULL){

  if(is.null(data) || is.null(prefix)){
    stop("You must give two arguments (data & prefix).")
  }
  tmp <- list()

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_[[:digit:]]+")
  groups  <- sub(pattern, "\\1", names)
  unique_groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique_groups))
  # This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique_groups)){
    # Get all the column names (group_names) for this group by making a specific
    #  pattern for it
    group_pattern <- paste0(prefix, group, "_[[:digit:]]+")
    group_names <- names[grep(group_pattern, names)]
    # Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_([[:digit:]]+)")
    group_names <- sub(pattern, "\\1", group_names)

    # Now, the data must be extracted
    # Get the column numbers that this group are included in
    dat <- data[, grep(group_pattern, names)]
    colnames(dat) <- group_names
    tmp[[group]] <- dat
  }
  tmp
}
