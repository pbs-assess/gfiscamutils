#' Set main directories for the project. Check existence of all directories and report.
#' Sensitivity groups will have the base model prepended.
#'
#' @param nongit_dir The full path containing non-version-controlled things
#' such as data, model runs from iSCAM, and reference materials. Default is "reponame-nongit"
#' @param models_dir The full path in which the iSCAM model directories are located
#' @param base_model_dir Name of the base model directory which is a subdirectory of `models`
#' @param bridge_models_dir Name of the subdirectory of `models_dir` that
#' contains the bridge model directories
#' @param bridge_models_dirs A vector of subdirectory names in `models_dir/bridge_models_dir`
#'  that each contain an individual iSCAM bridge model
#' @param sens_models_dir Name of the subdirectory of `models_dir` that
#' contains the sensitivity model directories
#' @param sens_models_dirs A vector of subdirectory names in `models_dir/sens_models_dir`
#'  that each contain an individual iSCAM sensitivity model
#'
#' @return A list of seven items, the first two are the same as the input arguments with the same name.
#' The 3rd is the full path of the base model.
#' The 4th is the full path of the bridge models directory.
#' The 5th contains a vector of the full paths to the bridge models which are inside the 4th directory.
#' The 6th is the full path of the sensitivity models directory.
#' The 7th contains a vector of the full paths to the sensitivity models which are inside the 6th directory.
#' @importFrom purrr map_lgl
#' @export
set_dirs <- function(nongit_dir = file.path(dirname(here()), paste0(basename(here()), "-nongit")),
                     models_dir = file.path(nongit_dir, "models"),
                     base_model_dir = "base",
                     bridge_models_dir = "001-bridge-models",
                     bridge_models_dirs = NULL,
                     sens_models_dir = "002-sens-models",
                     sens_models_dirs = NULL){

  stopifnot(!is.null(nongit_dir))
  if(!dir.exists(nongit_dir)){
    stop("Non-Git directory does not exist:\n",
         nongit_dir, call. = FALSE)
  }
  stopifnot(!is.null(models_dir))
  if(!dir.exists(models_dir)){
    stop("Models directory does not exist:\n",
         models_dir, call. = FALSE)
  }
  stopifnot(!is.null(base_model_dir))
  base_model_dir_full <- file.path(models_dir, base_model_dir)
  if(!dir.exists(base_model_dir_full)){
    stop("Base model directory does not exist:\n",
         base_model_dir_full, call. = FALSE)
  }

  bridge_models_dir_full <- file.path(models_dir, bridge_models_dir)
  if(!dir.exists(bridge_models_dir_full)){
    stop("Bridge models directory does not exist:\n",
         bridge_models_dir_full, call. = FALSE)
  }
  bridge_models_dirs_full <- NULL
  if(!is.null(bridge_models_dirs)){
    bridge_models_dirs_full <- map(bridge_models_dirs, ~{
      x <- file.path(bridge_models_dir_full, .x)
      dir_existence <- map_lgl(x, ~{dir.exists(.x)})
      if(!all(dir_existence)){
        stop("Some Sensitivity model directories do not exist:\n",
             paste0(x[!dir_existence], collapse = "\n"),
             call. = FALSE)
      }
      x
    })
  }

  stopifnot(!is.null(sens_models_dir))
  sens_models_dir_full <- file.path(models_dir, sens_models_dir)
  if(!dir.exists(sens_models_dir_full)){
    stop("Sensitivity models directory does not exist:\n",
         sens_models_dir_full, call. = FALSE)
  }
  sens_models_dirs_full <- NULL
  if(!is.null(sens_models_dirs)){
    sens_models_dirs_full <- map(sens_models_dirs, ~{
      x <- file.path(sens_models_dir_full, .x)
      dir_existence <- map_lgl(x, ~{dir.exists(.x)})
      if(!all(dir_existence)){
        stop("Some Sensitivity model directories do not exist:\n",
             paste0(x[!dir_existence], collapse = "\n"),
             call. = FALSE)
      }
      c(base_model_dir_full, x)
    })
  }

  list(nongit_dir = nongit_dir,
       models_dir = models_dir,
       base_model_dir = base_model_dir_full,
       bridge_models_dir = file.path(models_dir, bridge_models_dir),
       bridge_models_dirs = bridge_models_dirs_full,
       sens_models_dir = file.path(models_dir, sens_models_dir),
       sens_models_dirs = sens_models_dirs_full)
}

#' Load models and set up lists and classes for the base model, bridge model groups, and
#' sensitivity model groups
#'
#' @param main_dirs Output list from [set_dirs()]
#' @param bridge_models_text A list of vectors of text strings to show in the legends for bridge
#' model plots, one name for each model, where the list elements represent a group of models
#' @param sens_models_text A list of vectors of text strings to show in the legends for sensitivity
#' model plots, one name for each model, where the list elements represent a group of models
#' @param ... Arguments to pass to [create_rds_file()]
#'
#' @return A list of three items, the base_model inside a single-element list, the list of
#' bridge model groups, and the list of sensitivity model groups. `bridge_grp` and `sens_grp`
#' are groups lists of models which are to be compared with each other in the document.
#' This simplifies plotting and table functions
#' @importFrom purrr map_chr flatten
#' @export
#' @examples
#' \dontrun{
#' library(gfiscamutils)
#' bridge_models_dirs <- list(c("01-base",
#'                              "02-bridge-update-data"),
#'                            c("01-base",
#'                              "03-bridge-update-survey"))
#' bridge_models_text <- list(c("base model",
#'                              "Add new data"),
#'                            c("base model",
#'                              "Add new survey index point"))
#' sens_models_dirs <- list(c("01-base",
#'                            "10-high-m",
#'                            "11-low-m"),
#'                          c("01-base",
#'                            "12-high-sigma-r"))
#' sens_models_text <- list(c("base model",
#'                            "Increase prior for M",
#'                            "Decrease prior for M"),
#'                          c("base model",
#'                            "Increase prior for sigma R"))
#' drs <- set_dirs(base_model_dir = "base",
#'                 bridge_models_dirs = bridge_models_dirs,
#'                 sens_model_dirs = sens_model_dirs)
#' delete_files_ext(drs$models, ext = "rds") # If you want to delete all rds files
#' model_setup <- function(drs,
#'                         bridge_models_text = bridge_models_text,
#'                         sens_models_text = sens_models_text,
#'                         overwrite_rds_files = TRUE)
#' }
model_setup <- function(main_dirs = NULL,
                        bridge_models_text = NULL,
                        sens_models_text = NULL,
                        ...){

  if(is.null(main_dirs[1])){
    stop("main_dirs is NULL. Set main_dirs to the output of set_dirs()", call. = FALSE)
  }

  if(is.null(bridge_models_text[1])){
    warning("bridge_models_text is NULL. Using bridge model directory names for plot legends")
    bridge_models_text <- basename(main_dirs$bridge_models_dirs)
    bridge_models_text <- bridge_models_text %>% map(~{factor(.x, levels = .x)})
  }

  if(is.null(sens_models_text[1])){
    warning("sens_models_text is NULL. Using sens model directory names for plot legends")
    sens_models_text <- basename(main_dirs$sens_models_dirs)
    sens_models_text <- sens_models_text %>% map(~{factor(.x, levels = .x)})
  }

  # model_list is a list of three lists, one for the base model, one for the bridge models (also a list),
  # and one for the sensitivity models (also a list)
  model_list <- list(list(main_dirs$base_model_dir),
                     main_dirs$bridge_models_dirs,
                     main_dirs$sens_models_dirs)

  j <- map(model_list, function(.x, ...){
    models <- NULL
    if(!is.null(.x)){
      unique_models_dirs <- .x %>%
        flatten() %>%
        unique() %>%
        map_chr(~{.x})

      nul <- map(unique_models_dirs, function(x, ...){create_rds_file(x, ...)}, ...)

      # This ensures that each unique model is loaded only once, even if it is in multiple
      # sensitivity groups
      unique_models <- map(unique_models_dirs, ~{load_rds_file(.x)}) %>%
        set_names(unique_models_dirs)

      # sens_models is a list of lists of sensitivities of the same structure as sens_models_dirs.
      # the base_model is first in each sensitivity group list
      models <- map(.x, ~{
        map(.x, ~{
          unique_models[[match(.x, unique_models_dirs)]]
        })
      })
    }
  }, ...)

  base_model <- j[[1]][[1]]
  names(base_model) <- "Base model"
  class(base_model) <- mdl_lst_cls

  bridge_grps <- j[[2]]
  bridge_grps <- map2(bridge_grps, bridge_models_text, ~{
    names(.x) <- factor(.y)
    class(.x) <- mdl_lst_cls
    .x
  })
  class(bridge_grps) <- mdl_grp_cls

  sens_grps <- j[[3]]
  sens_grps <- map2(sens_grps, sens_models_text, ~{
    names(.x) <- factor(.y)
    class(.x) <- mdl_lst_cls
    .x
  })
  class(sens_grps) <- mdl_grp_cls

  list(base_model = base_model,
       bridge_grps = bridge_grps,
       sens_grps = sens_grps)
}