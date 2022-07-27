#' Create a one-row tibble from a vector of values
#'
#' @param vec A vector
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

  if(!is.null(df)){
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
move_legend_to_empty_facet <- function(g) {
  if(!(inherits(g, "gtable"))){
    if(inherits(g, "ggplot")){
      gp <- ggplotGrob(g) # convert to grob
    }else{
      message("This is neither a ggplot object nor a grob ",
              "generated from ggplotGrob. Returning original plot.")
      return(g)
    }
  }else{
    gp <- g
  }

  # check for unfilled facet panels
  facet_panels <- grep("^panel", gp[["layout"]][["name"]])
  empty_facet_panels <- sapply(facet_panels,
                               function(i){
                                 "zeroGrob" %in% class(gp[["grobs"]][[i]])
                                 })
  empty_facet_panels <- facet_panels[empty_facet_panels]

  # establish name of empty panels
  empty_facet_panels <- gp[["layout"]][empty_facet_panels, ]
  names <- empty_facet_panels$name
  if(!length(names)){
    stop("There are no empty facets to place the legend in", call. = FALSE)
  }
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  reposition_legend(g, 'center', panel = names)
}

#' Determine if a model is a valid iSCAM model
#'
#' @details
#' Check that the model has class `mdl_cls` and that
#' the attribute `model_desc` is set to something other than `NULL`
#' @param model A proposed iscam model object
#' @return Logical
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
#' @param model A proposed iscam model list object
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
#'  @param lst A list as output by [load_special()]
#'  of either MCMC age fits, age residuals, or selectivity estimates
#'  @param probs A vector of three values for quantiles calculations
#'  @export
calc_special_quants <- function(lst,
                                probs = c(0.025, 0.5, 0.975)){

  if("year" %in% names(lst)){
    year <- sym("year")
  }else if("start_year" %in% names(lst)){
    year <- sym("start_year")
  }else{
    stop("`lst` does not contain column `year` or `start_year`")
  }
  gear_lst <- lst %>% split(~gear)
  imap(gear_lst, ~{
    sex_lst <- split(.x, ~sex)
    sexes <- as.numeric(names(sex_lst))
    # Remove names so that .y in the following loop is an iterator, not the name
    names(sex_lst) <- NULL
    imap(sex_lst, ~{
      # Making a 'bare-bones' data frame by removing these columns makes the
      # following calls simpler. They are appended to the resulting data frame
      # afterwards
      bare_df <- .x %>%
        select(-c(gear, post, sex))
      lower <- bare_df %>%
        group_by(!!year) %>%
        summarize_all(quantile, probs = probs[1]) %>%
        ungroup() %>%
        mutate(quant = paste0(probs[1] * 100, "%"))
      med <- bare_df %>%
        group_by(!!year) %>%
        summarize_all(quantile, probs = probs[2]) %>%
        ungroup() %>%
        mutate(quant = paste0(probs[2] * 100, "%"))
      upper <- bare_df %>%
        group_by(!!year) %>%
        summarize_all(quantile, probs = probs[3]) %>%
        ungroup() %>%
        mutate(quant = paste0(probs[3] * 100, "%"))

      bind_rows(lower, med, upper) %>%
        mutate(sex = sexes[.y]) %>%
        select(year, sex, quant, everything())
    }) %>%
      bind_rows() %>%
      mutate(gear = .y) %>%
      select(gear, everything())
  }) %>%
    bind_rows()
}

#' Get a properly-typeset version of the parameter name
#'
#' @details
#' Includes expressions which have special characters (greek)
#' and super/subscripts. Also italicizes estimated parameter names.
#'
#' @param name iscam parameter name to make fancy
#' @param substr Logical. If `TRUE`, return all `substitute()`
#' expressions which are necessary for rendering by certain functions.
#' If `FALSE`, return all `expression()` expressions
#'
#' @return an R expression which represents the fancy version of the parameter name
#' @importFrom gfutilities firstup
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
    if(length(grep("selage", nm))){
      j <- sub("selage", "", nm)
      sex <- sub("[0-9]+_", "\\1", j)
      flt <- sub("_female|_male", "\\1", j)
      sexflt <- paste0(firstup(sex), ",", flt)
      bquote(hat(italic(a))[.(sexflt)])
    }else if(length(grep("selsd", nm))){
      j <- sub("selsd", "", nm)
      sex <- sub("[0-9]+_", "\\1", j)
      flt <- sub("_female|_male", "\\1", j)
      sexflt <- paste0(firstup(sex), ",", flt)
      bquote(hat(italic(gamma))[.(sexflt)])
    }else{
      NULL
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
#' @param name A vector of iSCAM parameter names to make fancy
#'
#' @return an R expression which represents the parameter name in latex math format
#' @export
get_fancy_names <- function(names){

  map_chr(names, ~{

    # Variants on reference point values (0.2B0, 0.8BMSY, FMSY_fleet, UMSY_fleet, etc)
    if(length(grep("^[0-9\\.]+B0$", .x))){
      refpt <- gsub("([0-9\\.]+)B0", "\\1", .x)
      return(paste0("$", refpt, "B_0$"))
    }
    if(length(grep("^[0-9,]+B0$", .x))){
      refpt <- gsub("([0-9,]+)B0", "\\1", .x)
      return(paste0("$", refpt, "B_0$"))
    }
    if(length(grep("^[0-9\\.]+BMSY$", .x))){
      refpt <- gsub("([0-9\\.]+)BMSY", "\\1", .x)
      return(paste0("$", refpt, "B_{MSY}$"))
    }
    if(length(grep("^[0-9,]+BMSY$", .x))){
      refpt <- gsub("([0-9,]+)BMSY", "\\1", .x)
      return(paste0("$", refpt, "B_{MSY}$"))
    }
    if(length(grep("^msy_fleet[0-9]+$", .x))){
      fleet <- gsub("^msy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$MSY_", fleet, "$"))
    }
    if(length(grep("^fmsy_fleet[0-9]+$", .x))){
      fleet <- gsub("^fmsy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$F_{MSY_", fleet, "}$"))
    }
    if(length(grep("^umsy_fleet[0-9]+$", .x))){
      fleet <- gsub("^umsy_fleet([0-9]+)$", "\\1", .x)
      return(paste0("$U_{MSY_", fleet, "}$"))
    }
    if(length(grep("^f_fleet[0-9]+_[0-9]+$", .x))){
      fleet <- gsub("^f_fleet([0-9]+)_[0-9]+$", "\\1", .x)
      year <- gsub("^f_fleet[0-9]+_([0-9]+)$", "\\1", .x)
      return(paste0("$F_{", year, "_", fleet, "}$"))
    }
    if(length(grep("^sbt_[0-9]+$", .x))){
      year <- gsub("^sbt_([0-9]+)$", "\\1", .x)
      return(paste0("$SB_{", year, "}$"))
    }
    # Catchability parameters
    if(length(grep("^q_\\{.*\\}$", .x))){
      return(paste0("$", .x, "$"))
    }
    if(length(grep("^q_gear[0-9]+$", .x))){
      digit <- as.numeric(sub("^q_gear([0-9]+)$", "\\1", .x))
      return(paste0("$q_{", digit, "}$"))
    }
    if(length(grep("^q[0-9]+$", .x))){
      digit <- as.numeric(sub("^q([0-9]+)$", "\\1", .x))
      return(paste0("$q_{", digit, "}$"))
    }
    if(length(grep("^log_q_gear[0-9]+$", .x))){
      digit <- as.numeric(sub("^log_q_gear([0-9]+)$", "\\1", .x))
      return(paste0("$log(q_{", digit, "})$"))
    }
    if(length(grep("^log_q[0-9]+$", .x))){
      digit <- as.numeric(sub("^log_q([0-9]+)$", "\\1", .x))
      return(paste0("$log(q_{", digit, "})$"))
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
        paste0("$\\hat{a}_{", sexflt, "}$")
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
        paste0("$\\hat{\\gamma}_{", sexflt, "}$")
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
          paste0("$\\hat{a}_{", sexflt, "}$")
        }else if(age_sd == "sd"){
          paste0("$\\hat{\\gamma}_{", sexflt, "}$")
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

    switch(.x,
           "ro" = "$R_0$",
           "log_ro" = "$log(R_0)$",
           "rbar" = "$\\overline{R}$",
           "log_rbar"  = "$log(\\overline{R})$",
           "rinit" = "$\\overline{R}_{init}$",
           "log_rinit" = "$log(\\overline{R}_{init}$)",
           "h" = "$h$",
           "f" = "$F$",
           "m" = "$M$",
           "vartheta" = "$\\vartheta$",
           "rho" = "$\\rho$",
           "bo" = "$B_0$",
           "sbo" = "$SB_0$",
           "ssb" = "$SSB$",
           "SSB" = "$SSB$",
           "m1" = ifelse(fr(), "$M_{homme}$", "$M_{male}$"),
           "m2" = ifelse(fr(), "$M_{femme}$", "$M_{female}$"),
           "m_sex1" = ifelse(fr(), "$M_{homme}$", "$M_{male}$"),
           "m_sex2" = ifelse(fr(), "$M_{femme}$", "$M_{female}$"),
           "log_m_sex1" = ifelse(fr(), "$log(M_{homme})$", "$log(M_{male})$"),
           "log_m_sex2" = ifelse(fr(), "$log(M_{femme})$", "$log(M_{female})$"),
           "bmsy" = "$B_{MSY}$",
           "msy" = "$MSY$",
           "msy1" = "$MSY_1$",
           "msy2" = "$MSY_2$",
           "msy3" = "$MSY_3$",
           "msy4" = "$MSY_4$",
           "msy5" = "$MSY_5$",
           "msy_fleet1" = "$MSY_1$",
           "msy_fleet2" = "$MSY_2$",
           "msy_fleet3" = "$MSY_3$",
           "msy_fleet4" = "$MSY_4$",
           "msy_fleet5" = "$MSY_5$",
           "fmsy" = "$F_{MSY}$",
           "fmsy1" = "$F_{MSY_1}$",
           "fmsy2" = "$F_{MSY_2}$",
           "fmsy3" = "$F_{MSY_3}$",
           "fmsy4" = "$F_{MSY_4}$",
           "fmsy5" = "$F_{MSY_5}$",
           "fmsy_fleet1" = "$F_{MSY_1}$",
           "fmsy_fleet2" = "$F_{MSY_2}$",
           "fmsy_fleet3" = "$F_{MSY_3}$",
           "fmsy_fleet4" = "$F_{MSY_4}$",
           "fmsy_fleet5" = "$F_{MSY_5}$",
           "umsy" = "$U_{MSY}$",
           "umsy1" = "$U_{MSY_1}$",
           "umsy2" = "$U_{MSY_2}$",
           "umsy3" = "$U_{MSY_3}$",
           "umsy4" = "$U_{MSY_4}$",
           "umsy5" = "$U_{MSY_51}$",
           "umsy_fleet1" = "$U_{MSY_1}$",
           "umsy_fleet2" = "$U_{MSY_2}$",
           "umsy_fleet3" = "$U_{MSY_3}$",
           "umsy_fleet4" = "$U_{MSY_4}$",
           "umsy_fleet5" = "$U_{MSY_5}$")
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
#' @param sd Standard deviation of the distribution
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
#' posterior values in binary fowmat.
#'
#' @param fn Filename
#' @param nsamples Number of samples
read_psv <- function(fn, n_samples = 10000){

  file_n <- file(fn, "rb")
  n_par <- readBin(file_n, what = integer(), n = 1)
  out <- readBin(file_n, what = numeric(), n = n_par * n_samples)
  out <- matrix(out, byrow = TRUE, ncol = n_par)
  close(file_n)
  out
}
