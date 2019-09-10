#' Run retrospectives for the given iscam model object
#'
#' @param model an iscam model object
#' @param yrs the number of years to run back for retrospectives.
#' @param overwrite if the retrospectives directory exists and this is TRUE, re-run the retrospectives.
#'   If the retrospectives directory does not exist, this is ignored and the retrospectives are run.
#'
#' @export
#' @importFrom gfiscamutils output.files retro.dir
run_retro <- function(model, yrs = 2, overwrite = FALSE){
  stopifnot(is.numeric(yrs),
            length(yrs) == 1)
  retro_path <- file.path(model$path, retro.dir)
  if(dir.exists(retro_path) && !overwrite){
    return(invisible())
  }
  unlink(retro_path, recursive = TRUE, force = TRUE)
  files <- dir(model$path)
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
  dir.create(retro_path, showWarnings = FALSE)
  for(i in seq_len(yrs)){
    retrostr <- as.character(i)
    if(nchar(retrostr) == 1){
      retrostr <- paste0("0", retrostr)
    }
    dest_dir <- file.path(retro_path, retrostr)
    dir.create(dest_dir, showWarnings = FALSE)
    file.copy(file.path(model$path, files),
              file.path(dest_dir, files))
    setwd(dest_dir)
    model_call <- paste0(iscam.exe.file, " -retro ", i)
    if(Sys.info()[["sysname"]] != "Windows"){
      model_call <- paste0("./", model_call, " ", linux.pipe.to.log)
    }else{
      model_call <- paste0(model_call, " ", dos.pipe.to.log)
    }
    message("Please wait while retrospective model runs for model ", model$path, ".\n Retrospective ", i, " of ", yrs, "...")
    shell(model_call)
  }
  setwd(here::here())
}
