#' Extract the given data frame into a list of matrices by iscam 'area'
#' and 'sex'
#'
#' @param data A data frame read in from one of the MCMC csv output files
#' @param prefix See details
#'
#' @details Extract the data frame given (data) by unflattening into a list of matrices
#'   by area-sex and gear. The area-sex number is located in the names of the
#'   columns of the data frame in this format:
#'   "prefix[areasexnum]_gear[gearnum]_year" where [areasexnum] and [gearnum]
#'   are one or more digits and prefix is the string given as an argument
#'   to the function.
#'
#' @return a list (area-sex) of lists (gears) of matrices, one element
#'  per group
#' @export
extract_area_sex_matrices <- function(data = NULL,
                                      prefix = NULL){

  if(is.null(data) || is.null(prefix)){
    stop("You must give two arguments (data & prefix).")
  }

  names <- names(data)
  pattern <- paste0(prefix, "([[:digit:]]+)_gear[[:digit:]]+_[[:digit:]]+")
  groups <- sub(pattern, "\\1", names)
  unique_groups <- unique(as.numeric(groups))
  tmp <- vector("list", length = length(unique_groups))
  # This code assumes that the groups are numbered sequentially from 1,2,3...N
  for(group in 1:length(unique_groups)){
    # Get all the column names (group_names) for this group by making a
    #  specific pattern for it
    group_pattern <- paste0(prefix, group, "_gear[[:digit:]]+_[[:digit:]]+")
    group_names <- names[grep(group_pattern, names)]
    # Remove the group number in the name, as it is not needed anymore
    pattern <- paste0(prefix, "[[:digit:]]+_gear([[:digit:]]+_[[:digit:]]+)")
    group_names <- sub(pattern, "\\1", group_names)
    # At this point, group_names' elements look like this: 1_1963
    # The first value is the gear, and the second, the year.
    # Get the unique gears for this area-sex group
    pattern <- "([[:digit:]]+)_[[:digit:]]+"
    gears <- sub(pattern, "\\1", group_names)
    unique.gears <- unique(as.numeric(gears))
    tmp2 <- vector("list", length = length(unique.gears))
    for(gear in 1:length(unique.gears)){
      gear.pattern <- paste0(prefix, group,"_gear", gear, "_[[:digit:]]+")
      # Now, the data must be extracted
      # Get the column numbers that this group are included in
      dat <- data[,grep(gear.pattern, names)]
      tmp2[[gear]] <- dat
    }
    tmp[[group]] <- tmp2
  }
  tmp
}
