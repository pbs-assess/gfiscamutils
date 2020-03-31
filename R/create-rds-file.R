#' Create an rds file to hold the model's data and outputs.
#'
#' @param model_dir Directory name of model to be loaded
#' @param overwrite Logical. If TRUE, overwrite the RDS file if it exists
#'
#' @return [base::invisible()]
#' @export
create_rds_file <- function(model_dir = NULL,
                            overwrite = FALSE,
                            ...){

  stopifnot(!is.null(model_dir))

  if(!dir.exists(model_dir)){
    stop("Error - the directory ", model_dir, " does not exist.\n",
         "Fix the problem and try again.", call. = FALSE)
  }

  # The RDS file will have the same name as the directory it is in
  rds_file <- file.path(model_dir, paste0(model_dir, ".rds"))
  if(file.exists(rds_file)){
    if(overwrite){
      unlink(rds_file, force = TRUE)
    }else{
      message("Skipping ", rds_file, ". To overwrite it, set overwrite = TRUE")
      return(invisible())
    }
  }

  message("Creating a new RDS file in ", model_dir, "\n")

  # If this point is reached, no RData file exists so it has to be built from scratch
  model <- load.iscam.files(model_dir, ...)

  # Load retrospectives. If none are found or there is a problem, model$retros will be NA
  # model$retropath <- file.path(model_dir, retro.dir)
  # if(dir.exists(model$retropath)){
  #   model$retros <- fetch_retrospectives(model$retropath,
  #                                        retrospective_yrs)
  # }else{
  #   model$retros <- NA
  # }

  saveRDS(model, file = rds_file)
  invisible()
}
