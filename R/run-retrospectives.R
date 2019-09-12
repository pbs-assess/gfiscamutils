#' Run retrospectives for the given iscam model directory
#'
#' @param model a directory containing a valid iscam model
#' @param yrs the number of years to run back for retrospectives.
#' @param overwrite if the retrospectives directory exists and this is TRUE, re-run the retrospectives.
#'   If the retrospectives directory does not exist, this is ignored and the retrospectives are run.
#'
#' @export
#' @importFrom stringr str_ends
run_retro <- function(path,
                      yrs = 2,
                      overwrite = FALSE){
  stopifnot(is.numeric(yrs),
            length(yrs) == 1)
  while(stringr::str_ends(path, "\\/")){
    path <- substr(path, 1, nchar(path) - 1)
  }
  j <- grep("//", path)
  calling_path <- getwd()
  on.exit(setwd(calling_path))
  setwd(path)
  if(dir.exists(retro.dir) && !overwrite){
    message("Directory ", file.path(path, retro.dir), " already exists and overwrite is FALSE so retrospectives were not run.")
    return(invisible())
  }
  unlink(retro.dir, recursive = TRUE, force = TRUE)
  files <- dir()
  for(ind in seq_along(output.files)){
    # Remove any output files from the copy source so as not to mess up the retro directory
    pattern <- output.files[ind]
    # Replace * wildcard with an alphanumeric wildcard
    pattern <- sub("\\*", "[A-Za-z0-9_-]+", pattern)
    gr <- grep(pattern, files)
    if(length(gr) > 0){
      files <- files[-gr]
    }
  }
  dir.create(retro.dir, showWarnings = FALSE)
  for(i in seq_len(yrs)){
    dest_dir <- as.character(i)
    if(nchar(dest_dir) == 1){
      dest_dir <- paste0("0", dest_dir)
    }
    dest_dir <- file.path(retro.dir, dest_dir)
    dir.create(dest_dir, showWarnings = FALSE)
    file.copy(files,
              file.path(dest_dir, files))
    setwd(dest_dir)
    model_call <- paste0(iscam.exe.file, " -retro ", i)
    if(Sys.info()[["sysname"]] != "Windows"){
      model_call <- paste0("./", model_call, " ", linux.pipe.to.log)
    }else{
      model_call <- paste0(model_call, " ", dos.pipe.to.log)
    }
    message("Please wait while retrospective model runs for model ", path, ".\n Retrospective ", i, " of ", yrs, "...")
    shell(model_call)
    setwd("../..")
  }
}
