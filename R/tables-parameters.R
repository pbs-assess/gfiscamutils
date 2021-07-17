#' Make a table of parameter estimate comparisons for MPD models.
#' Also show B0, Fmsy. Bmsy, and msy
#'
#' @param models A list of output models from [arrowtooth::model_setup()]
#' @param digits Number of digits to show
#' @param french If `TRUE` translate to French
#' @param model_col_widths Widths for columns, except the Parameter column
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return An [csasdown::csas_table()]
#' @importFrom stringr str_detect
#' @importFrom kableExtra column_spec linebreak row_spec
#' @export
param_est_mpd_table <- function(models,
                                digits = 3,
                                french = FALSE,
                                model_col_widths = "5em",
                                ...){

  ctrls <- map(models, ~{
    .x$ctl
  })
  # Assume all models have the same primary parameters
  params <- ctrls[[1]]$params
  param_names <- rownames(params)
  mpds <- map(models, ~{
    .x$mpd
  })
  # Get catchability names so that we can add the extra rows for models with fewer indices,
  # so that binding the data frames together by column later works
  q_names <- map(models, ~{
    .x$dat$index_abbrevs
  }) %>%
    flatten %>%
    map_chr(~{.x}) %>%
    unique()

  # Maximum number of fleets. All models with less will have blank rows added for the missing gears
  num_fleets <- map_int(models, ~{
    length(.x$mpd$msy)
  }) %>%
    max
  fleet_nums <- seq_len(num_fleets)[-1]

  # Extract parameter estimates
  param_names[param_names == "h"] <- "steepness"
  param_inds <- match(param_names, names(mpds[[1]]))
  param_ests <- map2(mpds, seq_along(mpds), ~{
    x <- .x[param_inds] %>% as.data.frame() %>% t() %>% as_tibble(rownames = "param")
    x <- x %>% select(1:2) %>% `names<-`(c("param", "value"))
    if(length(.x[param_inds]$log_m) == 2){
      m_female <- .x[param_inds]$log_m[2]
    }else{
      m_female <- NA
    }
    m_rowind <- which(x$param == "log_m")
    pre <- x[1:m_rowind, ] %>% mutate(param = ifelse(param == "log_m", "log_m_male", param))
    log_m_female <- tibble(param = "log_m_female", value = m_female)
    pre <- pre %>% bind_rows(log_m_female)
    post <- x[(m_rowind + 1):nrow(x), ]
    x <- bind_rows(pre, post)
    # Remove logs
    log_inds <- which(grepl("^log_", x$param))
    x$value[log_inds] <- exp(x$value[log_inds])
    x$param[log_inds] <- gsub("log_", "", x$param[log_inds])

    # Add B0, SB0
    y <- tibble(param = c("bo", "sbo", "bmsy"),
                value = c(.x$bo, .x$sbo, .x$bmsy))
    x <- x %>% bind_rows(y)

    # Add MSY-based reference points
    y <- tibble(param = c("msy", "fmsy"),
                value = c(.x$msy[1], .x$fmsy[1]))
    x <- x %>% bind_rows(y)
    # Multiple fishing fleets some blank
    if(length(fleet_nums)){
      for(i in fleet_nums){
        new_fleet_msy <- y %>%
          mutate(value = case_when(param == "msy" ~ .x$msy[i],
                                   param == "fmsy" ~ .x$fmsy[i],
                                   TRUE ~ NA_real_),
                 param = case_when(param == "msy" ~ paste0("msy", i),
                                   param == "fmsy" ~ paste0("fmsy", i),
                                   TRUE ~ ""))
        x <- x %>% bind_rows(new_fleet_msy)
      }
    }
    # Add q estimates
    catchability <- .x$q
    names(catchability) <- paste0("q - ", models[[.y]]$dat$index_abbrevs)
    catchability <- catchability %>% as_tibble(rownames = "param")
    x <- x %>% bind_rows(catchability)
    indices_not_incl <- q_names[!q_names %in% models[[.y]]$dat$index_abbrevs]
    if(length(indices_not_incl)){
      indices_not_incl <- tibble(param = paste0("q - ", indices_not_incl), value = NA)
      x <- x %>% bind_rows(indices_not_incl)
    }
    # Only keep param names in first data frame so when bound together they don't repeat
    if(.y != 1){
      x <- x %>% select(value)
    }
    x
  }) %>% bind_cols()
  names(param_ests)[-1] <- names(models)

  param_ests <- param_ests %>%
    mutate_at(vars(-param), function(x){format(round(x, digits), digits = 3, nsmall = 3)}) %>%
    mutate_at(vars(-param), function(x){ifelse(grepl(" +NA", x), "--", x)})

  # Rename the parameters with their latex values (where possible)
  param_latex_col <- param_ests %>% select(param)
  param_latex_col <- param_latex_col %>%
    mutate(param = case_when(param == "ro" ~ "$R_0$",
                             param == "steepness" ~ "$h$",
                             param == "m_male" ~ "$M_{male}$",
                             param == "m_female" ~ "$M_{female}$",
                             param == "rbar" ~ "$\\overline{R}$",
                             param == "rinit" ~ "$\\overline{R}_{\\mli{init}}$",
                             param == "rho" ~ "$\\rho$",
                             param == "vartheta" ~ "$\\vartheta$",
                             param == "tau" ~ "$\\tau$",
                             param == "sigma" ~ "$\\sigma$",
                             param == "bo" ~ "$B_0$",
                             param == "sbo" ~ "$SB_0$",
                             param == "msy" ~ "$MSY$",
                             param == "fmsy" ~ "$F_{MSY}$",
                             param == "msy2" ~ "$MSY_{fleet2}$",
                             param == "fmsy2" ~ "$F_{MSY_{fleet2}}$",
                             param == "bmsy" ~ "$B_{MSY}$",
                             str_detect(param, "^q - ") ~ gsub("q - ((\\w+\\s*)+)$", "$q_{\\1}$", param),
                             TRUE ~ param))

  param_ests <- param_ests %>%
    select(-param) %>%
    bind_cols(param_latex_col) %>%
    rename(Parameter = param) %>%
    select(Parameter, everything())

  tab <- csas_table(param_ests,
                    col.names = names(param_ests),
                    align = c("l", rep("r", ncol(param_ests) - 1)),
                    ...)

  # Add group separation lines
  num_sex <- map_dbl(models, ~{
    .x$dat$num.sex
  })
  any_two_sex <- any(num_sex > 1)
  num_pars <- models[[1]]$ctl$num.params + ifelse(any_two_sex, 1, 0)
  tab <- tab %>%
    row_spec(num_pars, hline_after = TRUE) %>%
    row_spec(num_pars + 5 + 2 * (num_fleets - 1), hline_after = TRUE)

  if(!is.null(model_col_widths)){
    tab <- tab %>%
      column_spec(2:ncol(param_ests), width = model_col_widths)
  }
  tab
}

#' Make a table showing number of parameters estimated and prior parameterizations
#'
#' @param model A single output model from [arrowtooth::model_setup()]
#' @param french If `TRUE` translate to French
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align
#' @importFrom purrr pmap_dfr
#' @importFrom dplyr add_row
param_settings_table <- function(model,
                                 french = FALSE,
                                 ...){

  params <- model$ctl$params %>% as_tibble(rownames = "param")
  params_out <- params %>%
    mutate(param = case_when(param == "log_ro" ~ "Log recruitment [$ln(R0)$]",
                             param == "h" ~ "Steepness [$h$]",
                             param == "log_m" ~ "Log natural mortality [$ln(M)$]",
                             param == "log_m_male" ~ "Log natural mortality (male) [$ln(M_{male})$]",
                             param == "log_m_female" ~ "Log natural mortality (female) [$ln(M_{female})$]",
                             param == "log_rbar" ~ "Log mean recruitment [$ln(\\overline{R})$]",
                             param == "log_rinit" ~ "Log initial recruitment [$\\overline{R}_{\\mli{init}}$]",
                             param == "rho" ~ "Variance ratio, rho [$\\rho$]",
                             param == "vartheta" ~ "Inverse total variance, kappa [$\\kappa$]",
                             TRUE ~ "")) %>%
    select(param)

  # Row-by-row loop
  j <- params %>%
    pmap_dfr(function(...) {
      current <- tibble(...)
      row <- current
      if(row$phz < 1){
        tibble(numest = 0,
               bounds = "Fixed",
               prior = paste0("$", f(row$ival, 3), "$"))
      }else if(row$prior == 0){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = "Uniform")
      }else if(row$prior == 1){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0("Normal($\\mu=\\ln(", round(exp(row$p1), 1), "), \\sigma=", row$p2, "$)"))
      }else if(row$prior == 2){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0("Lognormal($\\ln(", row$p1, "), ", row$p2, "$)"))
      }else if(row$prior == 3){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0("Beta($\\alpha=", row$p1, ", \\beta=", row$p2, "$)"))
      }else if(row$prior == 4){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0("Gamma($k=", row$p1, ", \\theta=", row$p2, "$)"))
      }
    })
  params_out <- params_out %>%
    bind_cols(j) %>%
    mutate(numest = ifelse(param == "Log natural mortality [$ln(M)$]" & numest != 0, model$dat$num.sex, numest))

  sel <- model$ctl$sel %>% as_tibble(rownames = "param")
  indices <- model$dat$indices %>%
    map(~{as_tibble(.x)}) %>%
    bind_rows()

  index_gear_nums <- unique(indices$gear) + 1
  cols <- 2:(ncol(sel))
  fish_gear_nums <- cols[!cols %in% index_gear_nums]
  index_sel <- sel %>%
    select(param, index_gear_nums)
  fish_sel <- sel %>%
    select(param, fish_gear_nums)
  # Get number estimated by looking at the phase row in the index_sel and fish_sel data frames
  surv_est <- index_sel %>% filter(param == "estphase") %>% select(-param) %>% `>`(0) %>% sum
  fish_est <- fish_sel %>% filter(param == "estphase") %>% select(-param) %>% `>`(0) %>% sum
  # Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  params_out <- params_out %>%
    add_row(param = "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)",
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") %>%
    add_row(param = "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)",
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") %>%
    add_row(param = "Survey age at 50\\% logistic selectivity ($\\hat{a}_k$)",
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None") %>%
    add_row(param = "Survey SD of logistic selectivity ($\\hat{\\gamma}_k$)",
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None")

  # Catchability
  q <- model$ctl$surv.q
  num_inds <- model$ctl$num.indices
  params_out <- params_out %>%
    add_row(param = "Survey catchability ($q_k$)",
            numest = num_inds,
            bounds = "[0, 1]",
            prior = paste0("Normal($",  round(exp(q[2, 1]), 1), ", ", round(q[3, 1], 1) ,"$)"))

  # Fishing mortality and recruitment parameters
  par <- model$par
  num_f_params <- length(par$log_ft_pars)
  num_rec_params <- length(par$log_rec_devs)
  num_init_rec_params <- length(par$init_log_rec_devs)
  params_out <- params_out %>%
    add_row(param = "Log fishing mortality values ($\\Gamma_{k,t}$)",
            numest = num_f_params,
            bounds = "[-30, 3]",
            prior = "[-30, 3]") %>%
    add_row(param = "Log recruitment deviations ($\\omega_t$)",
            numest = num_rec_params,
            bounds = "None",
            prior = "Normal($0, \\tau$)") %>%
    add_row(param = "Initial log recruitment deviations ($\\omega_{init,t}$)",
            numest = num_init_rec_params,
            bounds = "None",
            prior = "Normal($0, \\tau$)")

  names(params_out) <- c("Parameter",
                         "Number estimated",
                         "Bounds [low, high]",
                         "Prior (mean, SD) (single value = fixed)")

  tab <- csas_table(params_out,
                    col.names = names(params_out),
                    align = c("l", rep("r", ncol(params_out) - 1)),
                    ...) %>%
    column_spec(2, width = "5em") %>%
    column_spec(3, width = "7em") %>%
    column_spec(4, width = "15em")

  tab
}

#' Make a table of the catchability parameters - Herring specific
#'
#' @param var the variance parameter results data as loaded from
#' variance-parameter-results.csv
#' @param which.model which model to make the table for, 1 = AM1, 2 = AM2
#' @param tab the contents of the csv file as read in by [readr::read_csv()]
#' @param model an iscam model object
#' @param model.am2 an iscam model object
#' @param models a list of iscam model objects
#' @param model.names a vector of names of the models
#' @param type 1=biomass, 2=recruitment, 3=F, 4=U, 5=depletion
#' @param syr start year for table
#' @param tabular.environment latex tabular environment value
#' @param xcaption caption to appear in the calling document
#' @param xlabel the label used to reference the table in latex
#' @param font.size size of the font for the table
#' @param space.size size of the vertical spaces for the table
#' @param placement latex code for placement of the table in document
#' @param am1.lst A list of the AM1 iscam model objects
#' @param am2.lst A list of the AM2 iscam model objects
#' @param digits Number of decimal points to show
#' @param qa.lst List of AM1 models for q sensitivities
#' @param qb.lst List of AM1 models for q sensitivities
#' @param qc.lst List of AM1 models for q sensitivities#'
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align
#' @importFrom xtable xtable
make.catchability.parameters.table <- function(am1.lst,
                                               am2.lst = NULL,
                                               digits = 3,
                                               xcaption = "default",
                                               xlabel   = "default",
                                               font.size = 9,
                                               space.size = 10,
                                               placement = "H"){

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  lst <- list()
  st <- c("HG", "PRD", "CC", "SOG", "WCVI")
  for(i in 1:5){
    mdl <- am1.lst[[i]][[1]]
    mc <- mdl$mcmccalcs
    p.quants <- as.data.frame(mc$p.quants)
    ## AM1
    ctl.am1 <- mdl$ctl
    ## AM1 q estimates
    q1.est.am1 <- f(p.quants$q1[2], digits)
    q2.est.am1 <- f(p.quants$q2[2], digits)
    ## AM1 q priors
    q.am1 <- as.data.frame(t(ctl.am1$surv.q))
    q.am1$priormeanlog <- exp(q.am1$priormeanlog)
    p.vals.am1.s <- paste0("Normal($",
                           f(q.am1$priormeanlog[1], 3),
                           ", ",
                           f(q.am1$priorsd[1], 3),
                           "$)")
    p.vals.am1.d <- paste0("Normal($",
                           f(q.am1$priormeanlog[2], 3),
                           ", ",
                           f(q.am1$priorsd[2], 3),
                           "$)")
    sb.end.am1 <- f(mc$r.quants[3, 3], digits)
    sbo.am1 <- f(p.quants$sbo[2], digits)
    depl.am1 <- f(mc$depl.quants[,ncol(mc$depl.quants) - 1][2], digits)

    if(!is.null(am2.lst)){
      ## AM2
      mdl <- am2.lst[[i]][[1]]
      mc <- mdl$mcmccalcs
      p.quants <- as.data.frame(mc$p.quants)
      ctl.am2 <- mdl$ctl
      ## AM2 q estimates
      q1.est.am2 <- f(p.quants$q1[2], digits)
      q2.est.am2 <- f(p.quants$q2[2], digits)
      ## AM2 q priors
      q.am2 <- as.data.frame(t(ctl.am2$surv.q))
      q.am2$priormeanlog <- exp(q.am2$priormeanlog)
      p.vals.am2.s <- paste0("Normal($",
                             f(q.am2$priormeanlog[1], 3),
                             ", ",
                             f(q.am2$priorsd[1], 3),
                             "$)")
      p.vals.am2.d <- paste0("Normal($",
                             f(q.am2$priormeanlog[2], 3),
                             ", ",
                             f(q.am2$priorsd[2], 3),
                             "$)")
      sb.end.am2 <- f(mc$r.quants[3, 3], digits)
      sb.end.yr <- gsub("sb", "", rownames(mc$r.quants)[3])
      sbo.am2 <- f(p.quants$sbo[2], digits)
      depl.am2 <- f(mc$depl.quants[,ncol(mc$depl.quants) - 1][2], digits)
    }

    lst[[i]] <- rbind(c(st[i],
                      "AM1",
                      "Surface",
                      "None",
                      q1.est.am1,
                      q2.est.am1,
                      p.vals.am1.s,
                      sb.end.am1,
                      sbo.am1,
                      depl.am1),
                    c(st[i],
                      "AM1",
                      "Dive",
                      "None",
                      q1.est.am1,
                      q2.est.am1,
                      p.vals.am1.d,
                      sb.end.am1,
                      sbo.am1,
                      depl.am1))
    if(!is.null(am2.lst)){
      lst[[i]] <- rbind(lst[[i]],
                        c(st[i],
                          "AM2",
                          "Surface",
                          "None",
                          q1.est.am2,
                          q2.est.am2,
                          p.vals.am2.s,
                          sb.end.am2,
                          sbo.am2,
                          depl.am2),
                        c(st[i],
                          "AM2",
                          "Dive",
                          "None",
                          q1.est.am2,
                          q2.est.am2,
                          p.vals.am2.d,
                          sb.end.am2,
                          sbo.am2,
                          depl.am2))
    }
  }
  tab <- do.call(rbind, lst)
  colnames(tab) <- c(latex.bold("SAR"),
                     latex.bold("Model"),
                     latex.bold("Survey"),
                     latex.bold("Bounds"),
                     latex.mlc(c("Estimated",
                                 "q1")),
                     latex.mlc(c("Estimated",
                                 "q2")),
                     latex.bold("Prior (mean, SD)"),
                     latex.bold(paste0("SB\\subscr{",
                                       sb.end.yr,
                                       "}")),
                     latex.bold("SB\\subscr{0}"),
                     latex.mlc(c("Depletion",
                                 paste0("SB\\subscr{",
                                        sb.end.yr,
                                        "}/SB\\subscr{0}"))))

  addtorow <- list()
  addtorow$pos <- list(4, 8, 12, 16)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}

#' Make a table of parameter estimates and priors
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
make.parameters.est.table <- function(model,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H",
                                      translate = FALSE){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  mc <- model$mcmccalcs
  p.quants <- mc$p.quants
  mcmc.names <- colnames(p.quants)

  ## Append MPD values
  mpd <- model$mpd
  mpd.names <- names(mpd)

  ## Remove selectivity parameters
  mcmc.names <- mcmc.names[-grep("sel.*", mcmc.names)]
  mcmc.names <- mcmc.names[-grep("s?bo", mcmc.names)]
  p.quants <- p.quants[,-grep("sel.*", colnames(p.quants))]
  p.quants <- p.quants[,-grep("bo", colnames(p.quants))]

  mpd.param.vals <- NULL
  for(pname in mcmc.names){
    ## This is hack code because iscam is not outputting the same parameter
    ##  names for MPD and MCMC runs
    if(pname == "h"){
      pname <- "steepness"
    }
    match.q <- grep("q[[:digit:]]+",
                    pname)
    q.pars <- mpd$q
    if(length(match.q) > 0){
      ## The parameter starts with "q"
      split.val <- strsplit(pname,
                            "[^[:digit:]]")[[1]]
      q.num <- as.numeric(split.val[length(split.val)])
      this.par <- q.pars[q.num]
    }else if(pname != "sbo"){
      ## Match the mcmc name with the mpd name. Q and selectivity are special
      ##  cases, they must be extracted from vectors and matrices respectively
      this.par <- mpd[match(pname, mpd.names)]
    }
    mpd.param.vals <- c(mpd.param.vals, this.par)
  }
  names(mpd.param.vals) <- mcmc.names
  tab <- rbind(p.quants, as.numeric(mpd.param.vals))
  row.n <- rownames(tab)
  row.n[length(row.n)] <- en2fr("MPD", translate)
  rownames(tab) <- row.n
  tab <- f(t(tab), digits)

  ## The next set of names only pertains to the ARF assessment, the q's
  ##  and sel's are modified to line up with each other.
  new.col <- c("$R_0$",
               "$h$",
               "$M$",
               "$\\overline{R}$",
               "$\\overline{R}_{\\mli{init}}$",
               "$\\rho$",
               "$\\vartheta$",
               "$q_1$",
               "$q_2$",
               "$\\tau$",
               "$\\sigma$")
  col.names <- colnames(tab)
  col.names <- latex.bold(latex.perc(col.names))
  col.names <- c(latex.bold(en2fr("Parameter", translate)), col.names)
  tab <- cbind(new.col, tab)
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

#' Make a table of reference points
#'
#' @rdname make.parameters.table
#' @param translate Logical. Translate to french if TRUE
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
make.ref.points.table <- function(model.am2,
                                  digits = 3,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H",
                                  translate = FALSE){

  if(class(model.am2) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model.am2 list is incorrect.")
    }
  }

  tab.am2 <- model.am2$mcmccalcs$r.quants
  row.names <- tab.am2[,1]
  if( translate ) {
    row.names <- gsub( pattern="SB", replacement=en2fr("SB", translate),
                       x=row.names )
    row.names <- gsub( pattern="Proportion aged",
                       replacement=en2fr("Proportion aged", translate),
                       x=row.names )
  }
  col.names.am2 <- colnames(tab.am2)
  col.names.am2[1] <- paste0("\\textbf{", en2fr("Reference point", translate), "}")
  ## Remove latex rownames
  tab.am2 <- as.matrix(tab.am2[,-1])
  tab.am2 <- apply(tab.am2, c(1, 2) , as.numeric)
  ## Format the non-proportion data to digits
  n.row <- nrow(tab.am2)
  tab.am2.non <- tab.am2[-c(n.row - 1, n.row), ]
  tab.am2.non <- f(tab.am2.non, digits)
  ## Format the proportion-at-age to two digits only
  tab.am2.prop <- tab.am2[c(n.row - 1, n.row), ]
  tab.am2.prop <- f(tab.am2.prop, 2)
  tab.am2 <- rbind(tab.am2.non, tab.am2.prop)
  # Proportions don't actually have confidence bounds
  tab.am2[6,1] <- "-"
  tab.am2[6,3] <- "-"
  tab.am2[10,1] <- "-"
  tab.am2[10,3] <- "-"
  tab.am2[11,1] <- "-"
  tab.am2[11,3] <- "-"

  tab <- cbind(row.names,
               as.data.frame(tab.am2))

  colnames(tab) <- col.names.am2

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c( "\\toprule", "\\bottomrule" )

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        hline.after = c(0),
        add.to.row = addtorow,
        booktabs = TRUE)
}

#' Make a table for values such as biomass and recruitment
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
make.value.table <- function(model,
                             type,
                             syr,
                             digits = 3,
                             xcaption = "default",
                             xlabel   = "default",
                             font.size = 9,
                             space.size = 10,
                             placement = "H",
                             tabular.environment = "tabular",
                             translate = FALSE){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  if(type == 1){
    out.dat <- model$mcmccalcs$sbt.quants
  }else if(type == 2){
    out.dat <- model$mcmccalcs$recr.quants
  }else if(type == 3){
    out.dat <- model$mcmccalcs$f.mort.quants[[1]]
  }else if(type == 4){
    out.dat <- model$mcmccalcs$u.mort.quants[[1]]
  }else if(type == 5){
    out.dat <- model$mcmccalcs$depl.quants
  }else{
    stop("Type ", type, " not implemented.")
  }

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  col.names <- en2fr(colnames(tab), translate, allow_missing = TRUE)
  col.names[1] <- en2fr("Year", translate)
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

#' Make a table with both spawning biomass and depletion in it.
#' Based on [make.value.table()], but wider with both and extra headers
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
#' @importFrom rosettafish en2fr
make.biomass.depletion.table <- function(model,
                                         syr,
                                         digits = 3,
                                         xcaption = "default",
                                         xlabel   = "default",
                                         font.size = 9,
                                         space.size = 10,
                                         placement = "H",
                                         tabular.environment = "tabular",
                                         translate = FALSE){

  if(class(model) == model.lst.class){
    model <- model[[1]]
    if(class(model) != model.class){
      stop("The structure of the model list is incorrect.")
    }
  }

  out.dat <- model$mcmccalcs$sbt.quants
  out.dat <- rbind(out.dat, model$mcmccalcs$depl.quants)

  tab <- f(t(out.dat), digits)
  tab <- cbind(rownames(tab), tab)
  tab <- tab[tab[,1] >= syr,]
  ## Remove the projection year (last row)
  tab <- tab[-nrow(tab),]

  col.names <- en2fr(colnames(tab), translate, allow_missing = TRUE )
  col.names[1] <- en2fr("Year", translate)
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  addtorow <- list()
  addtorow$pos <- list(-1, nrow(tab))
  addtorow$command <- c(paste0("\\toprule",
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold(en2fr("Spawning biomass", translate))),
                               latex.amp(),
                               latex.mcol(4,
                                          "c",
                                          latex.bold(en2fr("Depletion", translate))),
                               latex.nline,
                               latex.cmidr("2-5", "lr"),
                               " ",
                               latex.cmidr("6-9", "lr")),
                        "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = c(0),
        booktabs = TRUE,
        tabular.environment = tabular.environment)
}

#' Make a table of the sensitivity parameter information as found in
#' the CSV file in the data directory
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
make.sens.parameter.table <- function(tab,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){


  tab <- sub("\\|", ",", as.matrix(tab))
  colnames(tab) <- c(latex.bold("Scenario"),
                     latex.bold("Description"),
                     latex.bold("Parameters"))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        booktabs = TRUE)
}

#' Make a table of the values of q, including quantiles
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
make.sens.q.table <- function(models,
                              model.names,
                              digits = 3,
                              xcaption = "default",
                              xlabel   = "default",
                              font.size = 9,
                              space.size = 10,
                              placement = "H"){

  quants <- lapply(models, function(x){
    t(f(x$mcmccalcs$q.quants, digits))})
  tab <- do.call(cbind, quants)
  tab <- cbind(rownames(tab), tab)
  col.names <- colnames(tab)
  col.names[1] <- "$q_k$"
  col.names <- latex.bold(latex.perc(col.names))
  colnames(tab) <- col.names

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  com <- paste0("\\toprule",
                latex.bold("Index"),
                latex.amp())
  for(i in 1:length(quants)){
    com <- paste0(com,
                  latex.mcol(ncol(quants[[i]]),
                             "c",
                             latex.bold(model.names[i])),
                  ifelse(i != length(quants), latex.amp(), ""))
  }
  com <- paste(com, latex.nline)
  addtorow$command <- com

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}

#' Make a table for the variance results data
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
#' @importFrom dplyr filter select mutate rename %>% arrange
#' @importFrom tidyr spread gather
make.variance.table <- function(var,
                                which.model = 1,
                                digits = 3,
                                xcaption = "default",
                                xlabel   = "default",
                                font.size = 9,
                                space.size = 10,
                                placement = "H"){

  if(which.model == 1){
    model <- "AM1"
  }else if(which.model == 2){
    model <- "AM2"
  }else{
    stop("which.model must be 1 or 2.")
  }

  var.res <- var %>%
    as_tibble( ) %>%
    filter( Model == model ) %>%
    select( Parameter, SensitivityCase, InitialValues, EstimatedValues ) %>%
    rename( Case=SensitivityCase, Initial=InitialValues, Estimated=EstimatedValues ) %>%
    gather( Initial, Estimated, key="Type", value="Value" ) %>%
    unite( CaseType, Case, Type ) %>%
    mutate( CaseType=factor(CaseType, levels=unique(CaseType)) ) %>%
    spread( CaseType, Value ) %>%
    mutate( Parameter=factor(Parameter, levels=unique(variance.results$Parameter)) ) %>%
    arrange( Parameter )

  var.res <- as.data.frame(var.res)
  ## Change the order of the columns
  tab <- var.res[,c(1,2,8,3,9,4,10,5,11,6,12,7,13)]
  tab[,1] <- c("Log recruitment ($ln(R_0)$)",
               "Steepness ($h$)",
               "Log natural mortality ($ln(M)$)",
               "Log mean recruitment ($\\ln(\\overline{R})$)",
               "Log initial recruitment ($\\ln(\\overline{R}_{init})$)",
               "Variance ratio, rho ($\\rho$)",
               "Inverse total variance, kappa ($\\kappa$)",
               "Sigma ($\\sigma$)",
               "Tau ($\\tau$)")

  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste0(latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("Base")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("1")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("2")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("3")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("4")),
                             latex.amp(),
                             latex.mcol(2,
                                        "c",
                                        latex.bold("5")),
                             latex.nline,
                             ## underscores
                             latex.cmidr("2-3", "lr"),
                             " ",
                             latex.cmidr("4-5", "lr"),
                             " ",
                             latex.cmidr("6-7", "lr"),
                             " ",
                             latex.cmidr("8-9", "lr"),
                             " ",
                             latex.cmidr("10-11", "lr"),
                             " ",
                             latex.cmidr("12-13", "lr"),
                             latex.bold("Leading Parameters"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.amp(),
                             latex.bold("Initial"),
                             latex.amp(),
                             latex.bold("Estimated"),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        booktabs = TRUE)
}

#' Make a table of catchability parameters for sensitivities
#'
#' @rdname make.parameters.table
#'
#' @return an xtable
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align latex.perc latex.cmidr
#' @importFrom xtable xtable
#' @importFrom dplyr filter select mutate rename %>% arrange
#' @importFrom tidyr spread gather
make.catchability.parameters.table.q.sens <- function(qa.lst,
                                                      qb.lst,
                                                      qc.lst,
                                                      digits = 3,
                                                      xcaption = "default",
                                                      xlabel   = "default",
                                                      font.size = 9,
                                                      space.size = 10,
                                                      placement = "H"){

  ## Catchability  parameters
  ## q is a data frame with 1 column for each survey and 3 rows:
  ## 1 - prior type:
  ##      0) Uniformative prior
  ##      1) normal prior density for log(q)
  ##      2) random walk in q
  ## 2 - prior log(mean)
  ## 3 - prior SD

  lst <- list()
  st <- c("HG", "PRD", "CC", "SOG", "WCVI")
  for(i in 1:5){
    mdl.qa <- qa.lst[[i]][[1]]
    mdl.qb <- qb.lst[[i]][[1]]
    mdl.qc <- qc.lst[[i]][[1]]
    mc.qa <- mdl.qa$mcmccalcs
    mc.qb <- mdl.qb$mcmccalcs
    mc.qc <- mdl.qc$mcmccalcs
    p.quants.qa <- as.data.frame(mc.qa$p.quants)
    p.quants.qb <- as.data.frame(mc.qb$p.quants)
    p.quants.qc <- as.data.frame(mc.qc$p.quants)
    ## AM1
    ctl.qa <- mdl.qa$ctl
    ctl.qb <- mdl.qb$ctl
    ctl.qc <- mdl.qc$ctl
    ## AM1 q estimates
    q1.est.qa <- f(p.quants.qa$q1[2], digits)
    q2.est.qa <- f(p.quants.qa$q2[2], digits)
    q1.est.qb <- f(p.quants.qb$q1[2], digits)
    q2.est.qb <- f(p.quants.qb$q2[2], digits)
    q1.est.qc <- f(p.quants.qc$q1[2], digits)
    q2.est.qc <- f(p.quants.qc$q2[2], digits)
    ## AM1 q priors
    q.qa <- as.data.frame(t(ctl.qa$surv.q))
    q.qb <- as.data.frame(t(ctl.qb$surv.q))
    q.qc <- as.data.frame(t(ctl.qc$surv.q))
    q.qa$priormeanlog <- exp(q.qa$priormeanlog)
    q.qb$priormeanlog <- exp(q.qb$priormeanlog)
    q.qc$priormeanlog <- exp(q.qc$priormeanlog)
    p.vals.qa <- paste0("Normal($",
                        f(q.qa$priormeanlog[1], 3),
                        ", ",
                        f(q.qa$priorsd[1], 3),
                        "$)")
    p.vals.qb <- paste0("Normal($",
                        f(q.qb$priormeanlog[1], 3),
                        ", ",
                        f(q.qb$priorsd[1], 3),
                        "$)")
    p.vals.qc <- paste0("Normal($",
                        f(q.qc$priormeanlog[1], 3),
                        ", ",
                        f(q.qc$priorsd[1], 3),
                        "$)")
    sb.end.qa <- f(mc.qa$r.quants[3, 3], digits)
    sb.end.qb <- f(mc.qb$r.quants[3, 3], digits)
    sb.end.qc <- f(mc.qc$r.quants[3, 3], digits)
    sb.end.yr <- gsub("sb", "", rownames(mc.qa$r.quants)[3])
    sbo.qa <- f(p.quants.qa$sbo[2], digits)
    sbo.qb <- f(p.quants.qb$sbo[2], digits)
    sbo.qc <- f(p.quants.qc$sbo[2], digits)
    depl.qa <- f(mc.qa$depl.quants[,ncol(mc.qa$depl.quants) - 1][2], digits)
    depl.qb <- f(mc.qb$depl.quants[,ncol(mc.qb$depl.quants) - 1][2], digits)
    depl.qc <- f(mc.qc$depl.quants[,ncol(mc.qc$depl.quants) - 1][2], digits)

    lst[[i]] <- rbind(c(st[i],
                      "AM1",
                      "None",
                      q1.est.qa,
                      q2.est.qa,
                      p.vals.qa,
                      sb.end.qa,
                      sbo.qa,
                      depl.qa),
                    c(st[i],
                      "AM1",
                      "None",
                      q1.est.qb,
                      q2.est.qb,
                      p.vals.qb,
                      sb.end.qb,
                      sbo.qb,
                      depl.qb),
                    c(st[i],
                      "AM1",
                      "None",
                      q1.est.qc,
                      q2.est.qc,
                      p.vals.qc,
                      sb.end.qc,
                      sbo.qc,
                      depl.qc))
  }
  tab <- do.call(rbind, lst)
  colnames(tab) <- c(latex.bold("SAR"),
                     latex.bold("Model"),
                     latex.bold("Bounds"),
                     latex.mlc(c("Estimated",
                                 "q1")),
                     latex.mlc(c("Estimated",
                                 "q2")),
                     latex.bold("Prior (mean, SD)"),
                     latex.bold(paste0("SB\\subscr{",
                                       sb.end.yr,
                                       "}")),
                     latex.bold("SB\\subscr{0}"),
                     latex.mlc(c("Depletion",
                                 paste0("SB\\subscr{",
                                        sb.end.yr,
                                        "}/SB\\subscr{0}"))))

  addtorow <- list()
  addtorow$pos <- list(3, 6, 9, 12)
  addtorow$command <- c("\\midrule ",
                        "\\midrule ",
                        "\\midrule ",
                        "\\midrule ")
  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)
}
