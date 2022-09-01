#' Read the iscam starter file to get the iscam input file names
#'
#' @param path Full path to the file
#' @param filename Filename
#'
#' @return A list with three names:
#'   1. Data file name
#'   2. Control file name
#'   3. Projection file name
#'
#' @export
fetch_file_names <- function(path, filename){

  fn <- file.path(path, filename)
  if(!file.exists(fn)){
    stop("The file ", fn, " does not exist. It must be present and it must have three lines: ",
         " data file name, control file name, and projection file name.", call. = FALSE)
  }
  d <- readLines(fn, warn = FALSE)
  ## Remove comments
  d <- gsub("#.*", "", d)
  ## Remove trailing whitespace
  d <- gsub(" +$", "", d)
  list(file.path(path, d[1]),
       file.path(path, d[2]),
       file.path(path, d[3]))
}
