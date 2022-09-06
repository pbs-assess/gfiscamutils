#' Replace gear names in objects in an iSCAM output list, from an RDS file
#' and re-save the file
#'
#' @param fn The RDS file to modify
#' @param ... Arguments passed to [replace_gear_names()]
#'
#' @return Nothing
#' @export
replace_gear_name_and_save <- function(fn, ...){

  if(!file.exists(fn)){
    stop("The file `", fn, "` does not exist", call. = FALSE)
  }

  model <- readRDS(fn)
  model <- replace_gear_names(model, ...)
  saveRDS(model, fn)
  invisible()
}

#' Change the gear names in already-loaded models
#'
#' @description
#' Change the gear names in already-loaded models. This function takes
#' a list of outputs from [load_iscam_files()] and changes the gear names
#' and/or abbreviations inside them
#'
#' @param models A list of outputs of [load_iscam_files()]
#' @param old_gear_names A vector of the old gear names to replace.
#' See model object (`model$dat$gear_names`) to see what they currently are
#' @param old_gear_abbrevs A vector of the old gear abbreviations to replace.
#' See model object (`model$dat$gear_abbrevs`) to see what they currently are
#' @param new_gear_names A vector of the new gear names. Must be the same
#' length as `old_gear_names`
#' @param new_gear_abbrevs A vector of the new gear abbrevs. Must be the same
#' length as `old_gear_abbrevs`
#'
#' @importFrom dplyr mutate
#' @return A modified list of `models`
#' @export
replace_gear_names <- function(models,
                               old_gear_names = NULL,
                               new_gear_names = NULL,
                               old_gear_abbrevs = NULL,
                               new_gear_abbrevs = NULL){

  if(is.null(models)){
    stop("The list of models (`models`) is `NULL`", call. = FALSE)
  }

  if(length(old_gear_names) != length(new_gear_names)){
    stop("Length of `old_gear_names` is not equal to the length of ",
         "`new_gear_names`", call. = FALSE)
  }
  if(length(old_gear_abbrevs) != length(new_gear_abbrevs)){
    stop("Length of `old_gear_abbrevs` is not equal to the length of ",
         "`new_gear_abbrevs`", call. = FALSE)
  }
  cls <- class(models)
  mdls <- imap(models, function(model, model_name){
    if(!is.null(old_gear_names)){
      map2(old_gear_names, new_gear_names, ~{
      inds <- grepl(.x, model$dat$gear_names)
        if(any(inds)){
          model$dat$gear_names[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$fleet_gear_names)
        if(any(inds)){
          model$dat$fleet_gear_names[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$index_gear_names)
        if(any(inds)){
          model$dat$index_gear_names[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$age_gear_names)
        if(any(inds)){
          model$dat$age_gear_names[inds] <<- .y
        }
        if(!is.null(model$mcmccalcs$selest_quants)){
          model$mcmccalcs$selest_quants <<-
            model$mcmccalcs$selest_quants |>
            mutate(gear = ifelse(gear == .x, .y, gear))
        }
        if(!is.null(model$mcmccalcs$agefit_quants)){
          model$mcmccalcs$agefit_quants <<-
            model$mcmccalcs$agefit_quants |>
            mutate(gear = ifelse(gear == .x, .y, gear))
        }
        if(!is.null(model$mcmccalcs$ageresids_quants)){
          model$mcmccalcs$ageresids_quants <<-
            model$mcmccalcs$ageresids_quants |>
            mutate(gear = ifelse(gear == .x, .y, gear))
        }
      })
    }
    if(!is.null(old_gear_abbrevs)){
      map2(old_gear_abbrevs, new_gear_abbrevs, ~{
        inds <- grepl(.x, model$dat$gear_abbrevs)
        if(any(inds)){
          model$dat$gear_abbrevs[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$fleet_gear_abbrevs)
        if(any(inds)){
          model$dat$fleet_gear_abbrevs[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$index_gear_abbrevs)
        if(any(inds)){
          model$dat$index_gear_abbrevs[inds] <<- .y
        }
        inds <- grepl(.x, model$dat$age_gear_abbrevs)
        if(any(inds)){
          model$dat$age_gear_abbrevs[inds] <<- .y
        }
      })
    }
    model
  })
  class(mdls) <- cls
  mdls
}
