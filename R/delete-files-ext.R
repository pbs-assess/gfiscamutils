#' Delete all files with a specific extension recursively. By default, .rds files
#'
#' @param path The directory to start in.
#' @param ext The extension of files to include for deletion
#' @export
#' @examples
#' \dontrun{
#' delete_files_ext(file.path(drs$models_dir, "01-bridge-models"))
#' }
delete_files_ext <- function(path = NULL, ext = "rds"){
  stopifnot(!is.null(path))
  ext_full <- paste0(".", ext, "$")
  ans <- readline(paste0("Warning - you are about to delete all ", ext, " files recursively in the\n",
                         path, "\n",
                         "directory. You cannot undo this operation. Are you sure? (y/n) <ENTER> "))
  if(ans == "n"){
    message("No files were deleted.")
    return(invisible())
  }
  dirs <- file.path(list.dirs(path, recursive = TRUE))
  map(dirs, ~{
    unlink(file.path(.x, grep(ext_full, dir(.x), value = TRUE)))
  })
  message("Deleted all ", ext, " files.")
  invisible()
}
