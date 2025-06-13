#' Create a table summarizing parameterization of an iSCAM model
#'
#' @description
#' Create a table showing number of parameters estimated and prior
#' parameterizations
#'
#' @param model A single output model
#' @param ret_df Logical. If `TRUE` return a data frame with the values,
#' instead of the [knitr::kable()] formatted table
#' @param col_widths The width of all columns other than the first. If `NULL`,
#' do not set
#' @param bold_header Logical. If `TRUE` make the table header row boldface
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @export
table_param_settings <- function(model,
                                 ret_df = FALSE,
                                 col_widths = "5em",
                                 bold_header = TRUE,
                                 ...){

  params <- model$ctl$params |>
    as_tibble(rownames = "param")

  if(fr()){
    params_out <- params |>
      mutate(param = case_when(param == "log_ro" ~ "Le recrutement de log [$\\ln(R_\\mathrm{0})$]",
                               param == "h" ~ "La pente [$h$]",
                               param == "log_m" ~ "La mortalit\u00E9 naturelle de log [$\\ln(M)$]",
                               param == "log_m_male" ~ "La mortalit\u00E9 naturelle de log (homme) [$\\ln(M_\\mathrm{male})$]",
                               param == "log_m_female" ~ "La mortalit\u00E9 naturelle de log (femme) [$\\ln(M_\\mathrm{female})$]",
                               param == "log_rbar" ~ "Le recrutement moyen de log [$\\ln(\\overline{R})$]",
                               param == "log_rinit" ~ "Le recrutement initial de log [$\\ln(\\overline{R}_\\mathrm{init})$]",
                               param == "rho" ~ "Rapport de variance, erreur d'observation [$\\rho$]",
                               param == "vartheta" ~ "La variance totale [$\\vartheta^2$]",
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
                               param == "log_rinit" ~ "Log initial recruitment [$\\ln(\\overline{R}_\\mathrm{init})$]",
                               param == "rho" ~ "Variance ratio, observation error [$\\rho$]",
                               param == "vartheta" ~ "Total variance [$\\vartheta^2$]",
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
                               "Fix\u00E9",
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
                                     "B\u00EAta($\\alpha=",
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
                              param == "La mortalit\u00E9 naturelle de log [$ln(M)$]") &
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
                           "\u00C2ge de la p\u00EAche \u00E0 une s\u00E9lectivit\u00E9 logistique de 50 \\% ($\\hat{a}_k$)",
                           "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)"),
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "Uniform") |>
    add_row(param = ifelse(fr(),
                           "P\u00EAche SD de la s\u00E9lectivit\u00E9 logistique ($\\hat{\\gamma}_\\mathrm{k}$)",
                           "Fishery SD of logistic selectivity ($\\hat{\\gamma}_\\mathrm{k}$)"),
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "Uniform") |>
    add_row(param = ifelse(fr(),
                           "\u00C2ge de l'enqu\u00EAte \u00E0 50 \\% de s\u00E9lectivit\u00E9 logistique ($\\hat{a}_\\mathrm{k}$)",
                           "Survey age at 50\\% logistic selectivity ($\\hat{a}_\\mathrm{k}$)"),
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "Uniform") |>
    add_row(param = ifelse(fr(),
                           "Enqu\u00EAte SD de la s\u00E9lectivit\u00E9 logistique ($\\hat{\\gamma}_\\mathrm{k}$)",
                           "Survey SD of logistic selectivity ($\\hat{\\gamma}_\\mathrm{k}$)"),
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "Uniform")

  # Catchability
  q <- model$ctl$surv.q
  num_inds <- model$ctl$num.indices
  params_out <- params_out |>
    add_row(param = ifelse(fr(),
                           "La capturabilit\u00E9 dans les relev\u00E9s ($q_\\mathrm{k}$)",
                           "Survey catchability ($q_\\mathrm{k}$)"),
            numest = num_inds,
            bounds = "[0, 1]",
            prior = paste0(ifelse(fr(),
                                  "Normale($",
                                  "Normal($"),
                           round(exp(q[2, 1]), 1), ", ", round(q[3, 1], 1) ,"$)"))

  # Fishing mortality and recruitment parameters
  par <- model$par
  num_f_params <- length(unlist(map(par$`log_ft_pars:`, ~{strsplit(as.character(trimws(.x)), split = " ")[[1]]})))
  num_rec_params <- length(unlist(map(par$`log_rec_devs:`, ~{strsplit(as.character(trimws(.x)), split = " ")[[1]]})))
  num_init_rec_params <- length(unlist(map(par$`init_log_rec_devs:`, ~{strsplit(as.character(trimws(.x)), split = " ")[[1]]})))
  params_out <- params_out |>
    add_row(param = ifelse(fr(),
                           "Valeurs logarithmiques de la mortalit\u00E9 par p\u00EAche  ($\\Gamma_\\mathrm{k,t}$)",
                           "Log fishing mortality values ($\\Gamma_\\mathrm{k,t}$)"),
            numest = num_f_params,
            bounds = "[-30, 3]",
            prior = "[-30, 3]") |>
    add_row(param = ifelse(fr(),
                           "\u00C9carts logarithmiques de recrutement ($\\omega_\\mathrm{t}$)",
                           "Log recruitment deviations ($\\omega_\\mathrm{t}$)"),
            numest = num_rec_params,
            bounds = ifelse(fr(),
                            "Aucun",
                            "None"),
            prior = ifelse(fr(),
                           "Normale($0, \\tau$)",
                           "Normal($0, \\tau$)")) |>
    add_row(param = ifelse(fr(),
                           "\u00C9carts initiaux de recrutement logarithmique ($\\omega_\\mathrm{init,t}$)",
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
                                "Priorit\u00E9 (moyenne, SD) (valeur unique = fixe)",
                                "Prior (mean, SD) (single value = fixed)"))

  if(bold_header){
    names(params_out) <- paste0("\\textbf{", names(params_out), "}")
  }

  if(ret_df){
    return(params_out)
  }

  tab <- csas_table(params_out,
                    col.names = names(params_out),
                    align = c("l", rep("r", ncol(params_out) - 1)),
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    bold_header = bold_header,
                    ...)

  if(!is.null(col_widths)){
    tab <- tab |>
      column_spec(2:3, width = col_widths) |>
      column_spec(4, width = "15em")
  }

  tab
}

