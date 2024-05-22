#' Translate the gear names and abbreviations in the models list for all
#' models to either French of English
#'
#' @details Depends on the return value of fr() which is just the value
#' of the R global option `french`
#'
#' @param models A list of the models
#' @param curr_gear_lang The language that the gear names are currently
#' stored as.
#'
#' @return A modified `models` list
#' @export
set_gear_language <- function(models, curr_gear_lang = NULL){

  if(is.null(models[1])){
    stop("You must supply `models`")
  }

  if(is.null(curr_gear_lang)){
    stop("You must supply `curr_gear_lang`")
  }

  base_model <- models$base_grps[[1]][[1]]
  gear_names <- base_model$dat$gear_names
  gear_abbrevs <- base_model$dat$gear_abbrevs

  if(fr()){
    if(curr_gear_lang == "en"){
      # Translate all gear names to French
      # allow_missing used here in case this code was already run, in which case
      # translation is not necessary
      gear_names_fr <- map_chr(gear_names, ~{
        en2fr(.x, allow_missing = TRUE)
      })
      gear_abbrevs_fr <- map_chr(gear_abbrevs, ~{
        en2fr(.x, allow_missing = TRUE)
      })

      # Base model
      models[[1]][[1]] <-
        replace_gear_names(models[[1]][[1]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_fr,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_fr)
      models$bridge_grps[[1]] <-
        replace_gear_names(models$bridge_grps[[1]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_fr,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_fr)
      models$bridge_grps[[2]] <-
        replace_gear_names(models$bridge_grps[[2]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_fr,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_fr)
    }
  }else{
    if(curr_gear_lang == "fr"){
      gear_names_en <- map_chr(gear_names, ~{
        fr2en(.x, allow_missing = TRUE)
      })
      gear_abbrevs_en <- map_chr(gear_abbrevs, ~{
        fr2en(.x, allow_missing = TRUE)
      })

      # Base model
      models[[1]][[1]] <-
        replace_gear_names(models[[1]][[1]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_en,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_en)
      models$bridge_grps[[1]] <-
        replace_gear_names(models$bridge_grps[[1]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_en,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_en)
      models$bridge_grps[[2]] <-
        replace_gear_names(models$bridge_grps[[2]],
                           old_gear_names = gear_names,
                           new_gear_names = gear_names_en,
                           old_gear_abbrevs = gear_abbrevs,
                           new_gear_abbrevs = gear_abbrevs_en)
    }
  }

  models
}
