#' Load the specially-formatted age fit or age residuals file
#'
#' @param fn The filename
#' @param type Either 'fits' or 'resids' for the age fits (A_hat in iscam)
#' or residuals (A_nu in iscam)
#'
#' @return The list of output data frames representing the age fits
#' by gear and year
#' @export
load_agefit <- function(fn, type = "fits", gear_names = NULL){
  d <- readLines(fn)
  d <- d[d != ""]
  label_inds <- grep("gear", d)
  labels <- d[label_inds]

  gears <- sort(unique(as.numeric(gsub(".*gear([0-9]+)", "\\1", d[label_inds]))))
  posts <- sort(unique(as.numeric(gsub("posterior([0-9]+).*", "\\1", d[label_inds]))))

  # Make a list of gear groups
  j <- map(gears, ~{
    inds <- grep(paste0("gear", .x), d)
    start <- inds + 1
    # Each gear has a number of posteriors
    map(start, ~{
      end <- label_inds[which(.x < label_inds)[1]] - 1
      if(is.na(end)){
        # End of the file is reached
        end <- length(d)
      }
      d[.x:end]
    })
  }) %>%
    `names<-`(gears)

  if(!is.null(gear_names)){
    if(length(j) == length(gear_names)){
      names(j) <- gear_names
    }else{
      warning("`gear_names` was not the same length as what was reported in the age file ", fn)
    }
  }
  browser()

}
