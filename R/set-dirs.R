#' Set and return all model directories for the project
#'
#' @details
#' Sensitivity groups will have the base model directory prepended,
#' The bridge model groups can have `last_yr_base_model_dir`
#' prepended if `prepend_to_bridge` is set to `TRUE` for the model group.
#' If directories do not exist, they will be assigned `NULL` and a warning
#' issued. If the `models_dir` does not exist, an error will be thrown.
#'
#' @param models_dir Path in which the model directories are
#' located for the current assessment year
#' @param nongit_dir Path in which the non-git files for the assessment are
#' located
#' @param base_models_dir Name of the base models directory
#' @param bridge_models_dir Name of the bridging models directory
#' @param sens_models_dir Name of the sensitivity models directory
#' @param retrospectives_models_dir Name of the retrospectives models directory
#' @param base_models_dirs A vector of subdirectory names in
#' `base_models_dir` that each contain an individual base model
#' @param bridge_models_dirs A vector of subdirectory names in
#' `bridge_models_dir` that each contain an individual bridge model
#' @param sens_models_dirs A vector of subdirectory names in `sens_models_dir`
#' that each contain an individual sensitivity model
#' @param retrospectives_models_dirs A vector of subdirectory names in
#' `retrospectives_models_dir`that each contain an individual retrospective
#' model
#' @param suppress_warnings If `TRUE`, warnings about directories not existing
#' will not be shown
#' @param base_model_dir Path for the base model, if it is outside the
#' file structure for an assessment. This is used for SR documents, where there
#' is a new base model with more data included in a subsequent year. If this
#' is `NULL`, the usual base model location will be used
#'
#' @return A list of vectors of directory names, which will have `NA` elements
#' for those which do not exist:
#' 1.  The input directory for `models_dir`
#' 2.  The input directory for `nongit_dir`
#' 3.  The input directory for `last_yr_base_model_dir`
#' 4.  The input directory for `base_model_dir`
#' 5.  The input directory for `bridge_models_dir`
#' 6.  The input directory for `sens_models_dir`
#' 7.  The input directory for `retrospective_models_dir`
#' 8.  The base model directories
#' 9.  The bridge model directories
#' 10. The sensitivity model directories
#'
#' @export
set_dirs <- function(
    models_dir = NA,
    nongit_dir = NA,

    base_models_dir = "01-base-models",
    bridge_models_dir = "02-bridge-models",
    sens_models_dir = "03-sens-models",
    retro_models_dir = "04-retrospective-models",

    base_models_dirs = "01-base-model",
    bridge_models_dirs = NA,
    sens_models_dirs = NA,
    retro_models_dirs = NA,
    prepend_to_bridge = NA,
    suppress_warnings = FALSE,
    base_model_dir = NULL){

  if(is.null(models_dir) || is.na(models_dir)){
    stop("`models_dir` must not be `NULL` or `NA`",
         call. = FALSE)
  }

  if(!dir.exists(models_dir)){
    stop("`models_dir` does not exist",
         call. = FALSE)
  }

  if(is.null(nongit_dir) || is.na(nongit_dir)){
    stop("`nongit_dir` must not be `NULL` or `NA`",
         call. = FALSE)
  }

  if(!dir.exists(nongit_dir)){
    stop("`nongit_dir` does not exist",
         call. = FALSE)
  }

  if(is.null(base_models_dir) ||
     is.null(bridge_models_dir) ||
     is.null(sens_models_dir) ||
     is.null(retro_models_dir) ||
     is.null(base_models_dirs) ||
     is.null(bridge_models_dirs) ||
     is.null(sens_models_dirs) ||
     is.null(retro_models_dirs)){
    stop("None of the following directory names can be `NULL`:\n",
         "(If you want them to be ignored, set them to `NA`)\n",
         "`base_models_dir`\n`bridge_models_dir`\n`sens_models_dir`\n",
         "`retro_model_dir`\n`base_models_dirs`\n`bridge_models_dirs`\n",
         "`sens_models_dirs`\n`retro_models_dirs`\n",
         call. = FALSE)
  }

  root_dirs_rel <- c(base_models_dir,
                     bridge_models_dir,
                     sens_models_dir,
                     retro_models_dir)
  root_dirs <- file.path(models_dir, root_dirs_rel)

  # This works if any of `root_dirs_rel` are `NA` so there is no explicit check
  # for `NA` here
  root_dirs_exist <- map_lgl(root_dirs, ~{
    dir.exists(.x)
  })

  if(!suppress_warnings){
    if(!all(root_dirs_exist)){
      if(sum(root_dirs_exist) == length(root_dirs_exist) - 1){
        warning("The following directory does not exist:\n",
                root_dirs[!root_dirs_exist], "\n")
      }else{
        warning("The following directories do not exist:\n",
                paste(root_dirs[!root_dirs_exist], collapse = "\n"), "\n")
      }
    }
  }

  subdirs_rel <- list(base_models_dirs,
                      bridge_models_dirs,
                      sens_models_dirs,
                      retro_models_dirs)

  has_models_subdirs <- map(subdirs_rel, function(type){
    map(type, function(group){
      map_lgl(group, ~{!is.na(.x)})
    })
  })

  # Make full paths. Some may contain NA
  dirs <- map2(root_dirs, subdirs_rel, function(root_dir, subdir){
    map(subdir, function(group){
      file.path(root_dir, group)
    })
  })

  # Prepend the base model to each of the sensitivity model groups
  dirs[[3]] <- map(dirs[[3]], function(sns){
    if(is.na(sns[1])){
      NA
    }else{
      c(dirs[[1]][[1]], sns)
    }
  })

  # Prepend the base model to each of the retrospectives model groups
  dirs[[4]] <- map(dirs[[4]], function(retro){
    if(is.na(retro[1])){
      NA
    }else{
      c(dirs[[1]][[1]], retro)
    }
  })

  # Remove `NA` entries
  dirs <- dirs |>
    map(~{
      if(is.na(.x[1])){
        return(NA)
      }
      .x
    })

  dirs[[1]] <- base_model_dir %||% dirs[[1]]

  list(models_dir = models_dir,
       nongit_dir = nongit_dir,
       base_model_dir = base_models_dir,
       bridge_models_dir = bridge_models_dir,
       sens_models_dir = sens_models_dir,
       retro_models_dir = retro_models_dir,
       base_models_dirs = dirs[[1]],
       bridge_models_dirs = dirs[[2]],
       sens_models_dirs = dirs[[3]],
       retro_models_dirs = dirs[[4]])
}
