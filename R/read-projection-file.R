#' Read in the contents of the iscam projection file
#'
#' @param file Filename
#'
#' @return A list representing the contents of the iscam projection file
#' @export
read_projection_file <- function(file = NULL){

  if(!file.exists(file)){
    warning("Projection file ", basename(file)," not found in ", dirname(file), ". Is your ",
            iscam.starter.file, " set up correctly? Setting data to NA.")
    return(NA)
  }

  data <- readLines(file, warn = FALSE)

  # Remove any empty lines
  data <- data[data != ""]

  # Remove preceeding whitespace if it exists
  data <- gsub("^[[:blank:]]+", "", data)

  # Get the element numbers which start with #.
  dat <- grep("^#.*", data)
  # Remove the lines that start with #.
  dat <- data[-dat]

  # Remove comments which come at the end of a line
  dat <- gsub("#.*", "", dat)

  # Remove preceeding and trailing whitespace
  dat <- gsub("^[[:blank:]]+", "", dat)
  dat <- gsub("[[:blank:]]+$", "", dat)

  # Now we have a nice bunch of string elements which are the inputs for iscam.
  # Here we parse them into a list structure.
  # This is dependent on the current format of the DAT file and needs to
  # be updated whenever the proj file changes format.
  tmp <- list()
  ind <- 0

  # Get the number of years to project
  tmp$num.projyrs <- as.numeric(dat[ind <- ind + 1])
  # Get the TAC values
  tmp$num.tac  <- as.numeric(dat[ind <- ind + 1])
  for(tac in 1:tmp$num.tac){
    # Read in the tacs, one, per line
    tmp$tac.vec[tac] <- as.numeric(dat[ind <- ind + 1])
  }

  # If the tac vector is on one line
  # tmp$tac.vec <- as.numeric(strsplit(dat[ind <- ind + 1],"[[:blank:]]+")[[1]])

  # Get the control options vector
  tmp$num.ctl.options <- as.numeric(dat[ind <- ind + 1])
  n.rows <- tmp$num.ctl.options
  n.cols <- 1
  tmp$ctl.options  <- matrix (NA, nrow = n.rows, ncol = n.cols)
  for(row in 1:n.rows){
    tmp$ctl.options[row, 1] <- as.numeric(dat[ind <- ind + 1])
  }
  # Rownames here are hardwired, so if you add a new row you must add a name
  #  or it here.
  option.names <- c("syrmeanm",
                    "nyrmeanm",
                    "syrmeanfecwtageproj",
                    "nyrmeanfecwtageproj",
                    "syrmeanrecproj",
                    "nyrmeanrecproj",
                    "shortcntrlpts",
                    "longcntrlpts",
                    "bmin")
  rownames(tmp$ctl.options) <- option.names[1:tmp$num.ctl.options]
  tmp$eof <- as.numeric(dat[ind <- ind + 1])
  tmp
}
