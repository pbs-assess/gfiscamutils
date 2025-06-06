#' Load models and set up lists and classes for the base model, bridge
#' model groups, and sensitivity model groups
#'
#' @details
#' If any of the text lists are `NULL`, default description text will be
#' assigned and a warning given. If the object "models" is already in
#' existence, that will be used and only the names will be changed. This
#' allows for easy and fast switching between French and English.
#'
#' @param drs Output list from [set_dirs()]
#' @param bridge_models_desc A list of vectors of text strings to show in
#' the legends for bridge model plots, one name for each model, where the
#' list elements represent a group of models
#' @param sens_models_desc A list of vectors of text strings to show in
#' the legends for sensitivity model plots, one name for each model, where
#' the list elements represent a group of models
#' @param retro_models_desc A list of vectors of text strings to show in
#' the legends for retrospective model plots, one name for each model,
#' where the list elements represent a group of models
#' @param ... Arguments to pass to [create_rds_file()]
#' @param base_models_desc A list of descriptions for the base models
#'
#' @return A list of items, the base_model inside a single-element
#' list, the list of bridge model groups, sensitivity model groups, request
#' model groups, test model groups, and retrospective model groups. These
#' groups are lists of models which are to be compared with each other in
#' the document. This simplifies plotting and table functions
#' @export
model_setup <- function(drs = NA,
                        base_models_desc = NA,
                        bridge_models_desc = NA,
                        sens_models_desc = NA,
                        retro_models_desc = NA,
                        ...){

  if(is.null(drs[1]) || is.na(drs[1])){
    stop("`drs` is `NULL` or `NA`. Set drs to the output of `set_dirs()`",
         call. = FALSE)
  }

  # Set `NULL` descriptions to defaults where possible (directories exist)
  if(!is.list(base_models_desc)){
    base_models_desc <- list(base_models_desc)
  }
  lst <- list(base_models_desc,
              bridge_models_desc,
              sens_models_desc,
              retro_models_desc)
  names(lst) <- c("base_models_dirs",
                  "bridge_models_dirs",
                  "sens_models_dirs",
                  "retro_models_dirs")
  model_lst <- drs[names(lst)]

  # If the descriptions are not present, use the directory names as
  # descriptions
  iter <- 1
  model_desc_lst <- map2(lst, model_lst, function(grp_descs, grp_drs){
    nm <- names(lst)[iter]
    if(is.null(grp_descs[[1]]) || is.na(grp_descs[1])){
      message("`", nm, "` has a `NULL` or `NA` description.\nAttempting ",
              "to use directory names for plot legends.")

      if(is.null(grp_drs[1]) || is.na(grp_drs[1])){
        message("  - Directory names for `", nm, "` are also `NULL` or `NA`")
        message("  - Cannot set up default descriptions for folders that ",
                "do not exist\n")
        NULL
      }else{
        message("  - Successfully set descriptions for `", nm, "` to ",
                "directory names.\n")
        iter <<- iter + 1
        return(map(grp_drs, ~{
          basename(.x)
        }))
      }
    }else{
      iter <<- iter + 1
      return(grp_descs)
    }
    iter <<- iter + 1
    NA
  })

  # A vector of unique model directories
  unique_models_dirs <- model_lst[!is.na(model_lst)] |>
    flatten() |>
    flatten() |>
    as.character() |>
    unique()

  # For each type (base, bridge, sens, request, test) extract unique groups,
  # load them only once if duplicates (to save time/memory) and match with
  # where they belong in the list according to model_list
  if(exists("models")){
    # If the loading has already been done, just reset the model names so it is
    # easy and fast to switch back and forth from English to French
    model_grps <- map2(models, model_desc_lst, \(model_grp, descs){
      map2(model_grp, descs, \(mdls, desc){
        names(mdls) <- desc
        mdls <- map2(mdls, desc, \(mdl, dsc){
          attr(mdl, "model_desc") <- dsc
          mdl <- mdl |>
            `class<-`(mdl_cls)

          mdl
        }) |>
          `class<-`(mdl_lst_cls)

        mdls
      })
    })
  }else{
    model_grps <- map2(model_lst,
                       model_desc_lst,
                       \(type, type_nm, ...){

      if(!is.null(type[1]) && !is.na(type[1])){
        # Check that the RDS files exists for these models
        walk(unique_models_dirs, \(path, ...){
          fn <- file.path(dirname(path), paste0(basename(path), ".rds"))
          if(!file.exists(fn)){
            stop("RDS file missing: `", fn, "`", call. = FALSE)
            #create_rds_file(path, ...)
          }}, ...)

        # Load the models in from the RDS files
        unique_models <- map(unique_models_dirs, function(path){
          fn <- file.path(dirname(path), paste0(basename(path), ".rds"))
          readRDS(fn)
        })
      }

      # Populate actual model output
      models <- map2(type, type_nm, \(dirs, descs){
        if(is.na(type[1])){
          return(NA)
        }

        map2(dirs, descs, \(dr, desc){
          tmp <- unique_models[[match(dr, unique_models_dirs)]]
          # Add description to the model
          # Example of how to access description for bridge model
          # group 1 model 3:
          # attr(models$bridge_models_dirs[[1]][[3]], "desc")
          attr(tmp, "model_desc") <- desc
          tmp <- tmp |>
            `class<-`(mdl_cls)
          # Assign description text to the model (from bridge_model_text and sens_model_text)
          tmp
        }) |>
          `names<-`(descs) |>
          `class<-`(mdl_lst_cls)
      })
    })

    names(model_grps) <- c("base_grps",
                           "bridge_grps",
                           "sens_grps",
                           "retro_grps")
  }

  model_grps
}
