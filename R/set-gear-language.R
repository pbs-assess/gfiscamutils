#' Translate the gear names and abbreviations in the models list for all
#' models to either French of English
#'
#' @details Depends on the return value of fr() which is just the value
#' of the R global option `french`
#'
#' @param models A list of the models
#'
#' @return A modified `models` list of class `mdl_lst_cls` which is an ISCAM
#' model list
#' @export
set_gear_language <- function(models){

  if(is.null(models[1])){
    stop("You must supply `models`")
  }

  mdls <- models |>
    # For each model type group in the models list
    map(\(type_grp){
      # For each model group in the model type group
      type_grp |> map(\(grp){
        # For each model in each models group
        grp <- grp |> map(\(model){

          model$dat$gear_names <- tr(model$dat$gear_names)
          model$dat$age_gear_names <- tr(model$dat$age_gear_names)
          model$dat$index_gear_names <- tr(model$dat$index_gear_names)
          model$dat$fleet_gear_names <- tr(model$dat$fleet_gear_names)

          model$dat$gear_abbrevs <- tr(model$dat$gear_abbrevs)
          model$dat$age_gear_abbrevs <- tr(model$dat$age_gear_abbrevs)
          model$dat$index_abbrevs <- tr(model$dat$index_abbrevs)
          model$dat$index_gear_abbrevs <- tr(model$dat$index_gear_abbrevs)
          model$dat$fleet_gear_abbrevs <- tr(model$dat$fleet_gear_abbrevs)

          model
        })
        class(grp) <- mdl_lst_cls
        grp
      })
    })

  class(mdls) <- mdl_lst_cls
  mdls
}
