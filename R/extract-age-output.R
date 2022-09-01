#' Extract an age structure MPD object from iSCAM output (.rep file)
#'
#' @param model An iscam model object
#' @param type One of 'obs', 'est', 'resid'
#'
#' @return A list of data frames, one for each gear
#' @importFrom tibble as_tibble
#' @export
extract_age_output <- function(model,
                               type = "obs"){
  if(class(model) == mdl_lst_cls){
    model <- model[[1]]
    if(class(model) != mdl_cls){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(!type %in% c("obs", "est", "resid")){
    stop("'type' must be one of 'obs', 'est', or 'resid'",
         call. = FALSE)
  }
  data_type <- switch(type,
                      "obs" = "d3_A",
                      "est" = "A_hat",
                      "resid" = "A_nu")
  mpd <- model$mpd
  if(is.na(mpd[1])){
    return(NA)
  }
  sage <- mpd$n_A_sage[1]
  nage <- mpd$n_A_nage[1]

  a_names <- grep(data_type, names(mpd), value = TRUE)

  j <- map(a_names, ~{
    x <- mpd[[.x]] %>% as_tibble
    if(type %in% c("est", "resid")){
      names(x) <- c("year", "gear", "area", "group", "sex", sage:nage)
    }else if(type == "obs"){
      names(x) <- c("year", "sample_size", "gear", "area", "group", "sex", sage:nage)
    }
    x
  })
  j
}
