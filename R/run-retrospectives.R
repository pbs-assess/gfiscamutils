#' Run retrospectives for the given iscam model directory
#'
#' @param yrs the number of years to run back for retrospectives.
#' @param overwrite if the retrospectives directory exists and this is TRUE, re-run the retrospectives.
#'   If the retrospectives directory does not exist, this is ignored and the retrospectives are run.
#' @param path location of the iscam model directory for which to run retrospectives
#'
#' @export
#' @importFrom stringr str_ends
run_retro <- function(path,
                      yrs = 1:5,
                      overwrite = FALSE){

  retro_path <- file.path(path, retro.dir)
  if(dir.exists(retro_path) && !overwrite){
    message("Directory ", retro_path, " already exists and overwrite is FALSE so retrospectives were not run.")
    return(invisible())
  }
  unlink(retro_path, recursive = TRUE, force = TRUE)

  # Choose files to copy into retrospective folders
  # output.files is defined in the gfiscamutils package
  files_to_copy <- dir(path)
  for(i in seq_along(output.files)){
    # Replace * wildcard with an alphanumeric wildcard
    .x <- sub("\\*", "[A-Za-z0-9_-]+", output.files[i])
    gr <- grep(.x, files_to_copy)
    if(length(gr) > 0){
      files_to_copy <- files_to_copy[-gr]
    }
  }
  files_to_copy <- file.path(path, files_to_copy)
  dir.create(retro_path, showWarnings = FALSE)

  nul <- map(yrs, ~{
    retro_subdir <- file.path(retro_path, paste0("retro-", pad.num(.x, 2)))
    dir.create(retro_subdir, showWarnings = FALSE)
    file.copy(files_to_copy, retro_subdir)
    shell_command <- paste0("cd ", retro_subdir, " & ", iscam.exe.file, " -retro ", .x, " -nox")
    shell(shell_command, wait = FALSE, intern = TRUE)
  })
}
