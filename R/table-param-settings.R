#' Create a table summarizing parameterization of an iSCAM model
#'
#' @description
#' Create a table showing number of parameters estimated and prior
#' parameterizations
#'
#' @param model A single output model
#' @param col_widths The width of all columns other than the first. If `NULL`,
#' do not set
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @export
#' @importFrom gfutilities latex.bold latex.mlc latex.size.str get.align
#' @importFrom purrr pmap_dfr
#' @importFrom dplyr add_row
table_param_settings <- function(model,
                                 col_widths = "5em",
                                 ...){

  params <- model$ctl$params |>
    as_tibble(rownames = "param")

  if(fr()){
    params_out <- params |>
      mutate(param = case_when(param == "log_ro" ~ "Le recrutement de log [$\\ln(R_\\mathrm{0})$]",
                               param == "h" ~ "La pente [$h$]",
                               param == "log_m" ~ "La mortalité naturelle de log [$\\ln(M)$]",
                               param == "log_m_male" ~ "La mortalité naturelle de log (homme) [$\\ln(M_\\mathrm{male})$]",
                               param == "log_m_female" ~ "La mortalité naturelle de log (femme) [$\\ln(M_\\mathrm{female})$]",
                               param == "log_rbar" ~ "Le recrutement moyen de log [$\\ln(\\overline{R})$]",
                               param == "log_rinit" ~ "Le recrutement initial de log [$\\overline{R}_\\mathrm{init}$]",
                               param == "rho" ~ "Le ratio de variance [$\\rho$]",
                               param == "vartheta" ~ "La variance totale inverse [$\\vartheta^2$]",
                               TRUE ~ "")) |>
      select(param)
  }else{
    params_out <- params |>
      mutate(param = case_when(param == "log_ro" ~ "Log recruitment [$\\ln(R_\\mathrm{0})$]",
                               param == "h" ~ "Steepness [$h$]",
                               param == "log_m" ~ "Log natural mortality [$\\ln(M)$]",
                               param == "log_m_male" ~ "Log natural mortality (male) [$\\ln(M_\\mathrm{male})$]",
                               param == "log_m_female" ~ "Log natural mortality (female) [$\\ln(M_\\mathrm{female})$]",
                               param == "log_rbar" ~ "Log mean recruitment [$\\ln(\\overline{R})$]",
                               param == "log_rinit" ~ "Log initial recruitment [$\\overline{R}_\\mathrm{init}$]",
                               param == "rho" ~ "Variance ratio [$\\rho$]",
                               param == "vartheta" ~ "Inverse total variance [$\\vartheta^2$]",
                               TRUE ~ "")) |>
      select(param)
  }
  # Row-by-row loop
  j <- params |>
    pmap_dfr(function(...) {
      current <- tibble(...)
      row <- current
      if(row$phz < 1){
        tibble(numest = 0,
               bounds = ifelse(fr(),
                               "Fixé",
                               "Fixed"),
               prior = paste0("$", f(row$ival, 3), "$"))
      }else if(row$prior == 0){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = ifelse(fr(),
                              "Uniforme",
                              "Uniform"))
      }else if(row$prior == 1){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0(ifelse(fr(),
                                     "Normale($\\mu=\\ln(",
                                     "Normal($\\mu=\\ln("),
                              round(exp(row$p1), 1), "), \\sigma=", row$p2, "$)"))
      }else if(row$prior == 2){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0(ifelse(fr(),
                                     "Lognormale($\\ln(",
                                     "Lognormal($\\ln("),
                              row$p1, "), ", row$p2, "$)"))
      }else if(row$prior == 3){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0(ifelse(fr(),
                                     "Bêta($\\alpha=",
                                     "Beta($\\alpha="),
                              row$p1, ", \\beta=", row$p2, "$)"))
      }else if(row$prior == 4){
        tibble(numest = 1,
               bounds = paste0("[", row$lb, ", ", row$ub, "]"),
               prior = paste0("Gamma($k=", row$p1, ", \\theta=", row$p2, "$)"))
      }
    })
  params_out <- params_out |>
    bind_cols(j) |>
    mutate(numest = ifelse((param == "Log natural mortality [$ln(M)$]" |
                              param == "La mortalité naturelle de log [$ln(M)$]") &
                             numest != 0, model$dat$num.sex, numest))

  sel <- model$ctl$sel |> as_tibble(rownames = "param")
  indices <- model$dat$indices |>
    map(~{as_tibble(.x)}) |>
    bind_rows()

  index_gear_nums <- unique(indices$gear) + 1
  cols <- 2:(ncol(sel))
  fish_gear_nums <- cols[!cols %in% index_gear_nums]
  index_sel <- sel |>
    select(param, index_gear_nums)
  fish_sel <- sel |>
    select(param, fish_gear_nums)
  # Get number estimated by looking at the phase row in the index_sel and fish_sel data frames
  surv_est <- index_sel |> filter(param == "estphase") |>
    select(-param) %>% `>`(0) |>
    sum()
  fish_est <- fish_sel |> filter(param == "estphase") |>
    select(-param) %>% `>`(0) |>
    sum()
  # Hardwired bounds of 0,1 for age-at-50% and 0,Inf for age-at-50% SD
  params_out <- params_out |>
    add_row(param = ifelse(fr(),
                           "Âge de la pêche à une sélectivité logistique de 50 \\% ($\\hat{a}_k$)",
                           "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)"),
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = ifelse(fr(),
                           "Pêche SD de la sélectivité logistique ($\\hat{\\gamma}_\\mathrm{k}$)",
                           "Fishery SD of logistic selectivity ($\\hat{\\gamma}_\\mathrm{k}$)"),
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = ifelse(fr(),
                           "Âge de l'enquête à 50 \\% de sélectivité logistique ($\\hat{a}_\\mathrm{k}$)",
                           "Survey age at 50\\% logistic selectivity ($\\hat{a}_\\mathrm{k}$)"),
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = ifelse(fr(),
                           "Enquête SD de la sélectivité logistique ($\\hat{\\gamma}_\\mathrm{k}$)",
                           "Survey SD of logistic selectivity ($\\hat{\\gamma}_\\mathrm{k}$)"),
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None")

  # Catchability
  q <- model$ctl$surv.q
  num_inds <- model$ctl$num.indices
  params_out <- params_out |>
    add_row(param = ifelse(fr(),
                           "La capturabilité dans les relevés ($q_\\mathrm{k}$)",
                           "Survey catchability ($q_\\mathrm{k}$)"),
            numest = num_inds,
            bounds = "[0, 1]",
            prior = paste0(ifelse(fr(),
                                  "Normale($",
                                  "Normal($"),
                           round(exp(q[2, 1]), 1), ", ", round(q[3, 1], 1) ,"$)"))

  # Fishing mortality and recruitment parameters
  par <- model$par
  num_f_params <- length(par$log_ft_pars)
  num_rec_params <- length(par$log_rec_devs)
  num_init_rec_params <- length(par$init_log_rec_devs)
  params_out <- params_out |>
    add_row(param = ifelse(fr(),
                           "Valeurs logarithmiques de la mortalité par pêche  ($\\Gamma_\\mathrm{k,t}$)",
                           "Log fishing mortality values ($\\Gamma_\\mathrm{k,t}$)"),
            numest = num_f_params,
            bounds = "[-30, 3]",
            prior = "[-30, 3]") |>
    add_row(param = ifelse(fr(),
                           "Écarts logarithmiques de recrutement ($\\omega_\\mathrm{t}$)",
                           "Log recruitment deviations ($\\omega_\\mathrm{t}$)"),
            numest = num_rec_params,
            bounds = "None",
            prior = ifelse(fr(),
                           "Normale($0, \\tau$)",
                           "Normal($0, \\tau$)")) |>
    add_row(param = ifelse(fr(),
                           "Écarts initiaux de recrutement logarithmique ($\\omega_\\mathrm{init,t}$)",
                           "Initial log recruitment deviations ($\\omega_\\mathrm{init,t}$)"),
            numest = num_init_rec_params,
            bounds = ifelse(fr(),
                            "Aucun",
                            "None"),
            prior = ifelse(fr(),
                           "Normale($0, \\tau$)",
                           "Normal($0, \\tau$)"))

  names(params_out) <- c(tr("Parameter"),
                         tr("Number estimated"),
                         tr("Bounds [low, high]"),
                         ifelse(fr(),
                                "Priorité (moyenne, SD) (valeur unique = fixe)",
                                "Prior (mean, SD) (single value = fixed)"))

  tab <- csas_table(params_out,
                    col.names = names(params_out),
                    align = c("l", rep("r", ncol(params_out) - 1)),
                    format = "latex",
                    ...)


  if(!is.null(col_widths)){
    tab <- tab |>
      column_spec(2:3, width = col_widths) |>
      column_spec(4, width = "15em")
  }

  tab
}

