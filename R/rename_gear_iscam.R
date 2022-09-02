#' Read in an ISCAM data file, replace all instances of the old name
#' with the new name and save the file, overwriting it
#'
#' @param fn The iSCAM data file
#' @param old_gear_name The old name or abbreviation for the gear/fleet
#' @param new_gear_name The new name or abbreviation for the gear/fleet
#'
#' @return Nothing
#' @export
rename_gear_iscam <- function(fn,
                              old_gear_name = NULL,
                              new_gear_name = NULL){

  if(!file.exists(fn)){
    stop("File `", fn, "` does not exist", call. = FALSE)
  }
  d <- readLines(fn)
  if(!any(grepl(old_gear_name, d))){
    message("The `old_gear_name` = ", old_gear_name, " was not found in the ",
            "file `", fn, "`")
  }else{
    d <- gsub(old_gear_name, new_gear_name, d)
    writeLines(d, fn)
    message("The `old_gear_name` = ", old_gear_name, " was found and ",
            "renamed in the file `", fn, "`")
  }
  invisible()
}
