#' Extract the number of parameters estimated in a model
#'
#' @details
#' The number is based on the iSCAM PAR file, with checks in the
#' control file for positiev (phz) which were means they were estimated
#' and not fixed
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#'
#' @return The number of estimated parameters
#' @export
get_num_params_est <- function(model){

  if(is.null(model$par)){
    stop("`model$par` is NULL. The PAR file for the model in ",
         model$path, " was not loaded in correctly",
         call. = FALSE)
  }
  num_pars_lst <- imap(model$par, function(param, param_nm){
    if(param_nm %in% c("num_params",
                       "obj_fun_val",
                       "max_gradient",
                       "log_m_nodes:")){
      return(0)
    }

    if(grepl("^theta", param_nm)){
      theta_num <- as.numeric(gsub("^theta\\[([0-9]+)\\]:$", "\\1", param_nm))
      theta_row <- model$ctl$params[theta_num, ]
      return(as.numeric(theta_row["phz"] > 0))
    }
    if(grepl("^sel_par", param_nm)){
      sel_par_sex <- gsub("^sel_par_(f|m)\\[([0-9]+)\\]:$", "\\1", param_nm)
      sel_par_num <- as.numeric(gsub("^sel_par_(f|m)\\[([0-9]+)\\]:$", "\\2", param_nm))
      sel_par_row <- model$ctl$sel[, sel_par_num]
      if(sel_par_row["estphase"] > 0){
        sel_pars <- unlist(map(param, ~{strsplit(as.character(trimws(.x)), split = " ")[[1]]}))
        return(length(sel_pars))
      }else{
        return(0)
      }
    }
    pars <- unlist(map(param, ~{strsplit(as.character(trimws(.x)), split = " ")[[1]]}))
    length(pars)
  })

  sum(unlist(num_pars_lst[lengths(num_pars_lst) > 0]))
}

#' Get a vector of values (median, CI interval, and end year) for
#' a parameter of your choice
#'
#' @param model An iSCAM model object as created in [load_iscam_files()]
#' @param param The name of a parameter
#' @param digits The number of digits to return in all numerical output
#'
#' @return A vector of 3 values, as stated in the decription
#' @export
get_parvals <- function(model, param, digits = 0){

  mcmccalcs <- model$mcmccalcs
  p_quants <- as_tibble(mcmccalcs$params_quants)
  if(param %in% names(p_quants)){
    val <- p_quants[[param]]
    med <- f(val[2], digits)
    ci <- paste0(f(val[1], digits),
                 "--",
                 f(val[3], digits),
                 " (width ",
                 f(round(val[3], digits) - round(val[1], digits), digits),
                 ")")
    endyr <- max(names(as_tibble(mcmccalcs$sbt_quants)))
  }else{
    quant_col <- paste0(param, "_quants")
    if(is.null(mcmccalcs[[quant_col]])){
      endyr <- NA
      med <- NA
      ci <- NA
    }else{
      endyr <- max(names(as_tibble(mcmccalcs[[quant_col]])))
      parm <- as_tibble(mcmccalcs[[quant_col]])
      end_out <- pull(parm[endyr])
      med <- f(end_out[2], digits)
      ci <- paste0(f(end_out[1], digits),
                   "--",
                   f(end_out[3], digits),
                   " (width ",
                   f(end_out[3] - end_out[1], digits), ")")

    }
  }
  c(med = med, ci = ci, endyr = endyr)
}


#' Get a set of parameter values from the models within the model group lists
#'
#' @param model_grp_lst Example: models$bridge_grps or models$sens_grps
#' @param remove_first If `TRUE`, remove the first model within each group
#' (sens groups have base model as the first one)
#' @param frac_digits The number of digits to report for values less than one
#' such as depletion
#' @param large_digits The number of digits to report for large numbers like
#' spawning biomass or B0
#'
#' @return A group list of parameter values
#' @export
get_group_parvals <- function(model_grp_lst,
                              remove_first = FALSE,
                              frac_digits = 2,
                              large_digits = 0){

  map(model_grp_lst, function(grp){
    models <- map2(grp, seq_along(grp), ~{
      if(.y == 1 && remove_first){
        return(NULL)
      }
      list(bo = get_parvals(.x, param = "bo", digits = large_digits),
           sbo = get_parvals(.x, param = "sbo", digits = large_digits),
           sbt = get_parvals(.x, param = "sbt", digits = large_digits),
           depl = get_parvals(.x, param = "depl", digits = frac_digits),
           m_female = get_parvals(.x, param = "m_female", digits = frac_digits),
           m_male = get_parvals(.x, param = "m_male", digits = frac_digits),
           h = get_parvals(.x, param = "h", digits = frac_digits))
    })
    models[lengths(models) > 1]
  })
}

#' Translate from English to French
#'
#' @details
#' If options(french) has been set, the translation will happen,
#' If options(french) is `NULL` or `FALSE`, no translation will happen
#'
#' @param x A character vector to translate
#' @param ... Arguments to pass to [rosettafish::en2fr()]
#'
#' @return The possibly translated character vector
#' @importFrom rosettafish en2fr
#' @importFrom csasdown fr
#' @export
tr <- function(x, ...){
  en2fr(x, translate = fr(), ...)
}

#' Create an expression from a string that may have a latex string in it
#'
#' @param text The string
#'
#' @return An object with classes `latexexpression` and `expression`. See
#' [latex2exp::TeX()]
#' @importFrom latex2exp TeX
#' @export
tex <- function(text){

  unname(TeX(text))
}

#' Read in model RDS files, modify the path attribute, and save
#'
#' @param model_fns A vector of model RDS filenames to modify
#' @param paths A vector of paths to replace in the files. Must be the
#' same length as `model_fns`
#'
#' @importFrom purrr walk2
#' @export
modify_model_path <- function(model_fns, paths){

  if(is.null(model_fns) || is.null(model_fns)){
    stop("Neither `model_fns` nor `paths` can be `NULL`", call. = FALSE)
  }

  if(length(model_fns) != length(paths)){
    stop("`model_fns` and `paths` must be the same length", call. = FALSE)
  }

  walk2(model_fns, paths, function(fn, pth){
    if(file.exists(fn)){
      model <- readRDS(fn)
      model$path <- pth
      model$mcmcpath <- file.path(pth, "mcmc")
      saveRDS(model, fn)
      message("Updated path for file '", fn, "'\n")
    }else{
      warning("File '", fn, "' does not exist", call. = FALSE)
    }
  })
}

#' Create a one-row tibble from a vector of values
#'
#' @details If the vector `vec` is a named vector, the names will become the
#' column names of the resulting data frame. If the `nms` vector is provided,
#' the names of the data frame will be set to those names, regardless of
#' whether or not the vector `vec` had names.
#'
#' @param vec A vector of single values
#' @param nms A vector of names to set as column names in the resulting
#' data frame. Must be the same length as `vec`
#'
#' @return A [tibble::tibble()] with one row
#' @export
vec2df <- function(vec, nms = NULL){

  if(!is.null(nms) && length(vec) != length(nms)){
    stop("The number of names supplied does not match the number ",
         "of elements in `vec`", call. = FALSE)
    names(df) <- nms
  }

  df <- vec |>
    enframe(name = NULL) |>
    t() |>
    as_tibble()

  if(!is.null(names(vec))){
    names(df) <- names(vec)
  }

  if(!is.null(df) && !is.null(nms)){
    names(df) <- nms
  }

  df
}

#' Create a gear lookup table showing which gear numbers are associated with
#' which gear names for the various type of gears
#'
#' @details
#' `type` 'all' includes all gears in the model in the order they are found in
#' the data file
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param type The type of gear to include in the table.
#'
#' @return A data frame which is the lookup table
#' @export
gear_lu_table <- function(model,
                          type = c("all", "age", "index", "fleet")){

  type <- match.arg(type)

  if(!is_iscam_model(model)){
    stop("`model` is not a valid iSCAM model", call. = FALSE)
  }
  gear_lst <- list(all = model$dat$gear_names,
                   fleet = model$dat$fleet_gear_names,
                   index = model$dat$index_gear_names,
                   age = model$dat$age_gear_names)

  gear_lst %>% imap(~{
    .x %>%
      enframe(name = "gear") %>%
      rename(gear_name = value) %>%
      mutate(type = .y)
  }) %>%
    bind_rows() %>%
    filter(type == !!type)
}

#' Moves a ggplot legend to an empty facet if one exists
#'
#' @details
#' If an empty facet does not exist, an error will be thrown
#' If the input object `g` is not a [ggplot2::ggplot()] object nor a
#' [ggplot2::ggplot_gtable()] object, the original object is returned.
#'
#' @param g A [ggplot2::ggplot()]
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom lemon reposition_legend
#' @export
move_legend_to_empty_facet <- function(g){

  if(!(inherits(g, "gtable"))){
    if(inherits(g, "ggplot")){
      gp <- ggplotGrob(g)
    }else{
      message("This is neither a ggplot object nor a grob ",
              "generated from ggplotGrob. Returning original plot.")
      return(g)
    }
  }else{
    gp <- g
  }

  # check for unfilled facet panels
  facet_panels <- grep("^panel", gp$layout$name)
  empty_facet_panels <- sapply(facet_panels,
                               \(i){
                                 "zeroGrob" %in% class(gp$grobs[[i]])
                               })
  empty_facet_panels_ind <- facet_panels[empty_facet_panels]

  # establish name of empty panels
  empty_facet_panels <- gp$layout[empty_facet_panels_ind, ]
  names <- empty_facet_panels$name
  if(!length(names)){
    stop("There are no empty facets to place the legend in")
  }

  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  reposition_legend(g, "center", panel = names)
}

#' Determine if a model is a valid iSCAM model
#'
#' @details
#' Check that the model has class `mdl_cls` and that
#' the attribute `model_desc` is set to something other than `NULL`
#' @param model A proposed iscam model object
#' @return Logical
#' @importFrom purrr imap_chr
#' @export
is_iscam_model <- function(model){
  if(mdl_cls %in% class(model)){
    if(!is.null(attributes(model)$model_desc)){
      return(TRUE)
    }
  }
  FALSE
}

#' Determine if a list of models is a valid iSCAM model list
#'
#' @details
#' Checks not only that the list has class `mdl_lst_cls` but that each list
#' element is a valid iscam model object.
#' @param models A list of proposed iscam model list objects
#' @return Logical
is_iscam_model_list <- function(models){

  if(mdl_lst_cls %in% class(models)){
    mdls <- map_lgl(models, ~{
      is_iscam_model(.x)
    })
    if(all(mdls)){
      return(TRUE)
    }
  }
  FALSE
}

#' Determine if a list is a valid iSCAM model group
#'
#' @details
#' Checks not only that the list has class `mdl_grp_cls` but that each list
#' element is a valid iscam model list object.
#' @param lst A proposed iscam model group object
#' @return Logical
is_iscam_model_group <- function(lst){
  if(mdl_grp_cls %in% class(lst)){
    mdl_lsts <- map_lgl(lst, ~{
      is_iscam_model_list(.x)
    })
    if(all(mdl_lsts)){
      return(TRUE)
    }
  }
  FALSE
}

#' Calculate age fits / age residuals and selectivity estimates quantiles
#'
#' @param lst A list as output by [load_longer()]
#' of either MCMC age fits, age residuals, or selectivity estimates
#' @param type The type of quantiles to calculate
#' @param probs A vector of three values for quantiles calculations
#' @export
calc_longer_quants <- function(lst,
                               type = c("age", "sel"),
                               probs = c(0.025, 0.5, 0.975)){

  type <- match.arg(type)

  if(type == "age" && !"year" %in% names(lst)){
    stop("`lst` does not contain column `year` for type `age`",
         call. = FALSE)
  }else if(type == "sel" && !"start_year" %in% names(lst)){
    stop("`lst` does not contain column `start_year` for type `sel`",
         call. = FALSE)
  }

  if(type == "sel"){
    df <- lst |>
      calc_quantiles_by_group(grp_cols = c("gear", "block", "start_year", "end_year", "sex"))
  }else{
    df <- lst |>
      calc_quantiles_by_group(grp_cols = c("gear", "year", "sex"))
  }
  df
}

#' Get a properly-typeset version of the parameter name
#'
#' @details
#' Includes expressions which have special characters (greek)
#' and super/subscripts. Also italicizes estimated parameter names.
#'
#' @param name iscam parameter name to make fancy
#' @param subst Logical. If `TRUE`, return all `substitute()`
#' expressions which are necessary for rendering by certain functions.
#' If `FALSE`, return all `expression()` expressions
#'
#' @return an R expression which represents the fancy version of the parameter name
#' @export
get_fancy_expr <- function(name, subst = FALSE){

  if(length(grep("^q_gear[1-9]+$", name))){
    digit <- as.numeric(sub("^q_gear([1-9]+)$", "\\1", name))
    return(substitute(italic(q)[digit], list(digit = digit)))
  }
  if(length(grep("^q[1-9]+$", name))){
    digit <- as.numeric(sub("^q([1-9]+)$", "\\1", name))
    return(substitute(italic(q)[digit], list(digit = digit)))
  }

  if(length(grep("^log_q_gear[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q_gear([1-9]+)$", "\\1", name))
    return(substitute("ln("*italic(q)[digit]*")", list(digit = digit)))
  }
  if(length(grep("^log_q[1-9]+$", name))){
    digit <- as.numeric(sub("^log_q([1-9]+)$", "\\1", name))
    return(substitute("ln("*italic(q)[digit]*")", list(digit = digit)))
  }

  # Get the fancy name for selectivity parameters
  #
  # @param nm The parameter name starting with selage or selsd
  # @return The fancy name or `NULL`
  get_sel_name <- function(nm){

    sel_pat <- "^sel(age|sd)([0-9]+)_(male|female)_block([0-9]+)$"
    is_age <- gsub(sel_pat, "\\1", nm) == "age"
    is_sd <- gsub(sel_pat, "\\1", nm) == "sd"
    flt <- gsub(sel_pat, "\\2", nm)
    sex <- ifelse(gsub(sel_pat, "\\3", nm) == "female", "f", "m")
    blk <- gsub(sel_pat, "\\4", nm)
    flt_sex_blk <- paste0(flt, ",", sex, ",", blk)

    if(is_age){
      bquote(hat(italic(a))[.(flt_sex_blk)])
    }else if(is_sd){
      bquote(hat(italic(gamma))[.(flt_sex_blk)])
    }else{
      "ConvErr"
    }
  }

  if(length(grep("^selage|^selsd", name))){
    return(get_sel_name(name))
  }

  switch(name,
         "ro" = if(subst) substitute(italic(R)[0]) else expression(italic(R)[0]),
         "rbar" = if(subst) substitute(bar(italic(R))) else expression(bar(italic(R))),
         "rinit" = if(subst) substitute(bar(italic(R))[init]) else expression(bar(italic(R))[init]),
         "m" = if(subst) substitute(italic(M)) else expression(italic(M)),
         "m1" = if(subst) substitute(italic(M)[Male]) else expression(italic(M)[Male]),
         "m2" = if(subst) substitute(italic(M)[Female]) else expression(italic(M)[Female]),
         "bo" = if(subst) substitute("B"[0]) else expression("B"[0]),
         "sbo" = if(subst) substitute("SB"[0]) else expression("SB"[0]),
         "vartheta" = if(subst) substitute(vartheta) else expression(vartheta),
         "rho" = if(subst) substitute(rho) else expression(rho),
         "bmsy" = if(subst) substitute("B"[MSY]) else expression("B"[MSY]),
         "msy" = if(subst) substitute("MSY") else expression("MSY"),
         "msy1" = if(subst) substitute("MSY"[1]) else expression(MSY[1]),
         "msy2" = if(subst) substitute("MSY"[2]) else expression(MSY[2]),
         "msy3" = if(subst) substitute("MSY"[3]) else expression(MSY[3]),
         "msy4" = if(subst) substitute("MSY"[4]) else expression(MSY[4]),
         "msy5" = if(subst) substitute("MSY"[5]) else expression(MSY[5]),
         "fmsy" = if(subst) substitute("F"[MSY]) else expression("F"[MSY]),
         "fmsy1" = if(subst) substitute("FMSY"[1]) else expression("F"[MSY1]),
         "fmsy2" = if(subst) substitute("FMSY"[2]) else expression("F"[MSY2]),
         "fmsy3" = if(subst) substitute("FMSY"[3]) else expression("F"[MSY3]),
         "fmsy4" = if(subst) substitute("FMSY"[4]) else expression("F"[MSY4]),
         "fmsy5" = if(subst) substitute("FMSY"[5]) else expression("F"[MSY5]),
         "umsy" = if(subst) substitute("U"[MSY]) else expression("U"[MSY]),
         "umsy1" = if(subst) substitute("UMSY"[1]) else expression("U"[MSY1]),
         "umsy2" = if(subst) substitute("UMSY"[2]) else expression("U"[MSY2]),
         "umsy3" = if(subst) substitute("UMSY"[3]) else expression("U"[MSY3]),
         "umsy4" = if(subst) substitute("UMSY"[4]) else expression("U"[MSY4]),
         "umsy5" = if(subst) substitute("UMSY"[5]) else expression("U"[MSY5]),
         "ssb" = if(subst) substitute("SSB") else expression("SSB"),
         "log_ro" = if(subst) substitute("ln("*italic(R)[0]*")") else expression("ln("*italic(R)[0]*")"),
         "h" = if(subst) substitute(italic("h")) else expression(italic("h")),
         "log_m_sex1" = if(subst) substitute("ln("*italic(M)[Male]*")") else expression("ln("*italic(M)[Male]*")"),
         "log_m_sex2" = if(subst) substitute("ln("*italic(M)[Female]*")") else expression("ln("*italic(M)[Female]*")"),
         "log_rbar"  = if(subst) substitute("ln("*bar(italic(R))*")") else expression("ln("*bar(italic(R))*")"),
         "log_rinit" = if(subst) substitute("ln("*bar(italic(R)[init])*")") else expression("ln("*bar(italic(R)[init])*")"))
}

#' Get a version of iSCAM parameter names in latex math format (i.e. $name$)
#'
#' @details
#' Vectorized.
#' Includes expressions which have special characters (greek)
#' and super/subscripts.
#'
#' @param names A vector of iSCAM parameter names to make fancy
#'
#' @return an R expression which represents the parameter name in latex math format
#' @export
get_fancy_names <- function(names){

  map_chr(names, ~{

    # Variants on reference point values (0.2B0, 0.8BMSY, FMSY_fleet, UMSY_fleet, etc)
    if(length(grep("^[0-9\\.]+B0$", .x))){
      refpt <- gsub("([0-9\\.]+)B0", "\\1", .x)
      return(paste0("$", refpt, "B_\\mathrm{0}$"))
    }
    if(length(grep("^[0-9,]+B0$", .x))){
      refpt <- gsub("([0-9,]+)B0", "\\1", .x)
      return(paste0("$", refpt, "B_\\mathrm{0}$"))
    }
    if(length(grep("^[0-9\\.]+BMSY$", .x))){
      refpt <- gsub("([0-9\\.]+)BMSY", "\\1", .x)
      return(paste0("$", refpt, "B_\\mathrm{", tr("MSY"), "}$"))
    }
    if(length(grep("^[0-9,]+BMSY$", .x))){
      refpt <- gsub("([0-9,]+)BMSY", "\\1", .x)
      return(paste0("$", refpt, "B_\\mathrm{", tr("MSY"), "}$"))
    }
    if(length(grep("^msy_fleet[0-9]+$", .x))){
      fleet <- gsub("^msy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$", tr("MSY"), "_\\mathrm{", fleet, "}$"))
    }
    if(length(grep("^fmsy_fleet[0-9]+$", .x))){
      fleet <- gsub("^fmsy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$F_\\mathrm{", tr("MSY"), "_", fleet, "}$"))
    }
    if(length(grep("^umsy_fleet[0-9]+$", .x))){
      fleet <- gsub("^umsy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$U_\\mathrm{", tr("MSY"), "_", fleet, "}$"))
    }
    if(length(grep("^f_fleet[0-9]+_[0-9]+$", .x))){
      fleet <- gsub("^f_fleet([0-9]+)_[0-9]+$", "\\1", .x)
      year <- gsub("^f_fleet[0-9]+_([0-9]+)$", "\\1", .x)
      return(paste0("$F_\\mathrm{", year, "_", fleet, "}$"))
    }
    if(length(grep("^sbt_[0-9]+$", .x))){
      year <- gsub("^sbt_([0-9]+)$", "\\1", .x)
      return(paste0("$SB_\\mathrm{", year, "}$"))
    }
    # Catchability parameters
    if(length(grep("^q_\\{.*\\}$", .x))){
      x <- gsub("(\\{.*\\})", "\\\\mathrm\\1", .x)
      return(paste0("$", x, "$"))
    }
    if(length(grep("^q_gear[0-9]+$", .x))){
      digit <- as.numeric(sub("^q_gear([0-9]+)$", "\\1", .x))
      return(paste0("$q_\\mathrm{", digit, "}$"))
    }
    if(length(grep("^q[0-9]+$", .x))){
      digit <- as.numeric(sub("^q([0-9]+)$", "\\1", .x))
      return(paste0("$q_\\mathrm{", digit, "}$"))
    }
    if(length(grep("^log_q_gear[0-9]+$", .x))){
      digit <- as.numeric(sub("^log_q_gear([0-9]+)$", "\\1", .x))
      return(paste0("$\\log(q_\\mathrm{", digit, "})$"))
    }
    if(length(grep("^log_q[0-9]+$", .x))){
      digit <- as.numeric(sub("^log_q([0-9]+)$", "\\1", .x))
      return(paste0("$\\log(q_\\mathrm{", digit, "})$"))
    }

    # Selectivity parameters
    # @param nm The parameter name starting with selage or selsd
    # @return The fancy name or `NULL`
    get_sel_name <- function(nm){
      if(length(grep("sel_age50", nm))){
        j <- sub("sel_age50_", "", nm)
        sex <- sub("_gear[0-9]+", "", j)
        sex <- ifelse(sex == "male",
                      ifelse(fr(),
                             "homme",
                             "male"),
                      ifelse(fr(),
                             "femme",
                             "female"))
        flt <- sub("female_gear|male_gear", "\\1", j)
        sexflt <- paste0(sex, ",", flt)
        paste0("$\\hat{a}_\\mathrm{", sexflt, "}$")
      }else if(length(grep("sel_sd50", nm))){
        j <- sub("sel_sd50_", "", nm)
        sex <- sub("_gear[0-9]+", "", j)
        sex <- ifelse(sex == "male",
                      ifelse(fr(),
                             "homme",
                             "male"),
                      ifelse(fr(),
                             "femme",
                             "female"))
        flt <- sub("female_gear|male_gear", "", j)
        sexflt <- paste0(sex, ",", flt)
        paste0("$\\hat{\\gamma}_\\mathrm{", sexflt, "}$")
      }else if(length(grep("sel_(age|sd)_", nm))){
        pat <- "^sel_(age|sd)_(male|female)_(\\{.*\\})$"
        age_sd <- gsub(pat, "\\1", nm)
        sex <- gsub(pat, "\\2", nm)
        sex <- ifelse(sex == "male",
                      ifelse(fr(),
                             "homme",
                             "male"),
                      ifelse(fr(),
                             "femme",
                             "female"))
        flt <- gsub(pat, "\\3", nm)
        sexflt <- paste0(sex, ",", flt)
        if(age_sd == "age"){
          paste0("$\\hat{a}_\\mathrm{", sexflt, "}$")
        }else if(age_sd == "sd"){
          paste0("$\\hat{\\gamma}_\\mathrm{", sexflt, "}$")
        }else{
          stop("age_sd in selectivity parameter name is something other than 'age' or 'sd'",
               call. = FALSE)
        }
      }else{
        stop("selectivity parameter name is not recognized",
             call. = FALSE)
      }
    }

    if(length(grep("^sel_age|^sel_sd", .x))){
      return(get_sel_name(.x))
    }

    out_nm <- switch(.x,
           "ro" = "$R_\\mathrm{0}$",
           "log_ro" = "$log(R_\\mathrm{0})$",
           "rbar" = "$\\overline{R}$",
           "log_rbar"  = "$log(\\overline{R})$",
           "rinit" = "$\\overline{R}_\\mathrm{init}$",
           "log_rinit" = "$log(\\overline{R}_\\mathrm{init}$)",
           "h" = "$h$",
           "f" = "$F$",
           "m" = "$M$",
           "vartheta" = "$\\vartheta$",
           "rho" = "$\\rho$",
           "so" = "$so$",
           "beta" = "$\\beta$",
           "phie" = "$\\phi_e$",
           "bo" = "$B_\\mathrm{0}$",
           "sbo" = "$SB_\\mathrm{0}$",
           "ssb" = "$SSB$",
           "SSB" = "$SSB$",
           "m" = "$M$",
           "m1" = "$M_1$",
           "m2" = "$M_2$",
           "m_sex1" = paste0("$M_\\mathrm{",
                             tr("male"),
                             "}$"),
           "m_sex2" = paste0("$M_\\mathrm{",
                             tr("male"),
                             "}$"),
           "log_m_sex1" = paste0("$log(M_\\mathrm{",
                                 tr("male"),
                                 "})$"),
           "log_m_sex2" = paste0("$log(M_\\mathrm{",
                                 tr("female"),
                                 "})$"),
           "bmsy" = paste0("$B_\\mathrm{", tr("MSY"), "}$"),
           "msy" = paste0("$", tr("MSY"), "$"),
           "msy1" = paste0("$", tr("MSY"), "_\\mathrm{1}$"),
           "msy2" = paste0("$", tr("MSY"), "_\\mathrm{2}$"),
           "msy3" = paste0("$", tr("MSY"), "_\\mathrm{3}$"),
           "msy4" = paste0("$", tr("MSY"), "_\\mathrm{4}$"),
           "msy5" = paste0("$", tr("MSY"), "_\\mathrm{5}$"),
           "msy_fleet1" = paste0("$", tr("MSY"), "_\\mathrm{1}$"),
           "msy_fleet2" = paste0("$", tr("MSY"), "_\\mathrm{2}$"),
           "msy_fleet3" = paste0("$", tr("MSY"), "_\\mathrm{3}$"),
           "msy_fleet4" = paste0("$", tr("MSY"), "_\\mathrm{4}$"),
           "msy_fleet5" = paste0("$", tr("MSY"), "_\\mathrm{5}$"),
           "fmsy" = paste0("$F_\\mathrm{", tr("MSY"), "}$"),
           "fmsy1" = paste0("$F_\\mathrm{", tr("MSY"), "_1}$"),
           "fmsy2" = paste0("$F_\\mathrm{", tr("MSY"), "_2}$"),
           "fmsy3" = paste0("$F_\\mathrm{", tr("MSY"), "_3}$"),
           "fmsy4" = paste0("$F_\\mathrm{", tr("MSY"), "_4}$"),
           "fmsy5" = paste0("$F_\\mathrm{", tr("MSY"), "_5}$"),
           "fmsy_fleet1" = paste0("$F_\\mathrm{", tr("MSY"), "_1}$"),
           "fmsy_fleet2" = paste0("$F_\\mathrm{", tr("MSY"), "_2}$"),
           "fmsy_fleet3" = paste0("$F_\\mathrm{", tr("MSY"), "_3}$"),
           "fmsy_fleet4" = paste0("$F_\\mathrm{", tr("MSY"), "_4}$"),
           "fmsy_fleet5" = paste0("$F_\\mathrm{", tr("MSY"), "_5}$"),

           "umsy" = paste0("$U_\\mathrm{", tr("MSY"), "}$"),
           "umsy1" = paste0("$U_\\mathrm{", tr("MSY"), "_1}$"),
           "umsy2" = paste0("$U_\\mathrm{", tr("MSY"), "_2}$"),
           "umsy3" = paste0("$U_\\mathrm{", tr("MSY"), "_3}$"),
           "umsy4" = paste0("$U_\\mathrm{", tr("MSY"), "_4}$"),
           "umsy5" = paste0("$U_\\mathrm{", tr("MSY"), "_5}$"),
           "umsy_fleet1" = paste0("$U_\\mathrm{", tr("MSY"), "_1}$"),
           "umsy_fleet2" = paste0("$U_\\mathrm{", tr("MSY"), "_2}$"),
           "umsy_fleet3" = paste0("$U_\\mathrm{", tr("MSY"), "_3}$"),
           "umsy_fleet4" = paste0("$U_\\mathrm{", tr("MSY"), "_4}$"),
           "umsy_fleet5" = paste0("$U_\\mathrm{", tr("MSY"), "_5}$"))
  })
}

#' Deprecated
#' Extract the model class objects from the list of model lists,
#' and merge them into a single model list containing all the model
#' class objects
#'
#' @param ... one or more lists of iscam model objects
#'
#' @return an iscam model object list
#' @export
c.model.list <- function(...){
  .Defunct()
  lst <- list(...)
  ret.lst <- NULL
  ind <- 1
  for(i in 1:length(lst)){
    if(class(lst[[i]]) != mdl_lst_cls){
      stop("List element ", i, " is not of the class '", mdl_lst_cls, "'.")
    }
    for(j in 1:length(lst[[i]])){
      if(class(lst[[i]][[j]]) != mdl_cls){
        stop("Sublist element ", j, " of list element ", i,
             " is not of the class '", mdl_cls, "'.")
      }
      ret.lst[[ind]] <- lst[[i]][[j]]
      ind <- ind + 1
    }
  }
  class(ret.lst) <- mdl_lst_cls
  ret.lst
}

#' Calculation of sigma and tau from rho and vartheta
#'
#' @details
#' Total variance is given by ϑ (`vartheta`) and the proportion of the total
#' variance that is associated with observation errors is given by ρ (`rho`),
#' the variance is partitioned into observation errors (σ^2) (`sigma`^2) and
#' process errors (τ^2) (`tau`^2).
#'
#' @param rho Parameter rho from iscam model
#' @param vartheta Parameter vartheta from iscam model
#' (actually vartheta squared in the iSCAM equation T5.2)
#'
#' @return A vector of two values, sigma and tau
#' @export
calc_sig_tau <- function(rho, vartheta){

  tau <- sqrt((1 - rho) / vartheta)
  sigma <- sqrt(rho / vartheta)
  c(sigma = sigma, tau = tau)
}

#' Calculation of rho and vartheta from sigma and tau
#'
#' @param sig Parameter sigma
#' @param tau Parameter tau
#'
#' @seealso calc_sig_tau
#'
#' @return A vector of two values, rho and vartheta
#' @export
calc_rho_vartheta <- function(sig, tau){
  rho <- sig ^ 2 / (tau ^ 2 + sig ^2)
  vartheta <- (1 - rho) / tau ^ 2
  c(rho = rho, vartheta = vartheta)
}

#' Calculate the shape parameters (alpha and beta) for a Beta distribution
#' given the mean and standard deviation
#'
#' @param mu Mean of the distribution
#' @param cv Coefficient of variation of the distribution
#'
#' @return A vector of two values, alpha and beta
#' @export
calc_beta_params <- function(mu, cv) {
  sd <- mu * cv
  alpha <- ((1 - mu) / sd ^ 2 - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  c(alpha = alpha, beta = beta)
}

#' Calculate the mean and CV for a Beta distribution given the alpha and
#' beta shape parameters
#'
#' @param alpha Shape parameter 1
#' @param beta Shape parameter 2
#'
#' @return A vector of two values, mean and CV
#' @export
calc_beta_mean_cv <- function(alpha, beta) {
  mu <- alpha / (alpha + beta)
  sd <- sqrt(alpha * beta / ((alpha + beta) ^ 2 * (alpha + beta + 1)))
  c(mean = mu, cv = sd / mu)
}

#' Read ADMB-generated psv file
#'
#' @details
#' The psv file is generated when the MCMC is run in ADMB and is used to
#' perform the calculations needed for the -mceval step. It contains the
#' posterior values in binary format.
#'
#' @param fn Filename
#' @param n_samples Number of samples
#' @return A [base::matrix]
read_psv <- function(fn,
                     n_samples = 10000){

  file_n <- file(fn, "rb")
  n_par <- readBin(file_n, what = integer(), n = 1)
  out <- readBin(file_n, what = numeric(), n = n_par * n_samples)
  out <- matrix(out, byrow = TRUE, ncol = n_par)
  close(file_n)
  out
}
