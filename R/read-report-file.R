#' Read in the iscam report (.rep) file
#'
#' @param fn Filename
#'
#' @return A list representing everything in the report file
#' @export
#'
#' @details Read in the iscam report (.rep) file:
#'   File structure:
#'   It is assumed that each text label will be on its own line,
#'   followed by one or more lines of data.
#'   If the label is followed by a single value or line of data,
#'   a vector will be created to hold the data.
#'   If the label is followed by multiple lines of data,
#'   a matrix will be created to hold the data. The matrix might be
#'   ragged so a check is done ahead of time to ensure correct
#'   matrix dimensions.
#'
#'   If a label has another label following it but no data,
#'   that label is thrown away and not included in the returned list.
#'
#'   A label must start with an alphabetic character followed by
#'   any number of alphanumeric characters (includes underscore and .)
read_report_file <- function(fn){

  if(!file.exists(fn)){
    warning("Report file ", basename(fn)," not found in ", dirname(fn), ". Setting MPD output to NA.")
    return(NA)
  }

  dat <- readLines(fn, warn = FALSE)
  # Remove preceeding and trailing whitespace on all elements,
  #  but not 'between' whitespace.
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Find the line indices of the labels
  # Labels start with an alphabetic character followed by
  # zero or more alphanumeric characters
  # A vector of the object names
  objs  <- grep("^[[:alpha:]]+[[:alnum:]]*", dat, value = TRUE)
  nobj <- length(objs)
  ret  <- list()
  indname <- 0

  for(obj in seq_len(nobj)){
    indname <- match(objs[obj], dat)
    if(obj != nobj){ # If this is the last object
      inddata <- match(objs[obj + 1], dat)
    }else{
      inddata <- length(dat) + 1 # Next row
    }
    # 'inddiff' is the difference between the end of data
    # and the start of data for this object. If it is zero,
    # throw away the label as there is no data associated with it.
    inddiff <- inddata - indname
    tmp <- NA
    if(inddiff > 1){
      if(inddiff == 2){
        # Create and populate a vector
        vecdat <- dat[(indname + 1) : (inddata - 1)]
        vecdat <- strsplit(vecdat, "[[:blank:]]+")[[1]]
        vecnum <- as.numeric(vecdat)
        ret[[objs[obj]]] <- vecnum
      }else if(inddiff > 2){
        # Create and populate a (possible ragged) matrix
        matdat <- dat[(indname + 1) : (inddata - 1)]
        matdat <- strsplit(c(matdat), "[[:blank:]]+")
        # Now we have a vector of strings, each representing a row
        # of the matrix, and not all may be the same length
        rowlengths <- unlist(lapply(matdat, "length"))
        nrow <- max(rowlengths)
        ncol <- length(rowlengths)
        # Create a new list with elements padded out by NAs
        matdat <- lapply(matdat, function(x){c(x, rep(NA, nrow))[1:nrow]})
        matnum <- do.call(rbind, matdat)
        mode(matnum) <- "numeric"
        ret[[objs[obj]]] <- matnum
      }
    }else{
      # Throw away this label since it has no associated data.
    }
  }
  ret
}
