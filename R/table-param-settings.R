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

  params_out <- params |>
    mutate(param = case_when(param == "log_ro" ~ "Log recruitment [$ln(R0)$]",
                             param == "h" ~ "Steepness [$h$]",
                             param == "log_m" ~ "Log natural mortality [$ln(M)$]",
                             param == "log_m_male" ~ "Log natural mortality (male) [$ln(M_{male})$]",
                             param == "log_m_female" ~ "Log natural mortality (female) [$ln(M_{female})$]",
                             param == "log_rbar" ~ "Log mean recruitment [$ln(\\overline{R})$]",
                             param == "log_rinit" ~ "Log initial recruitment [$\\overline{R}_{\\mli{init}}$]",
                             param == "rho" ~ "Variance ratio, rho [$\\rho$]",
                             param == "vartheta" ~ "Inverse total variance, kappa [$\\kappa$]",
                             TRUE ~ "")) |>
    select(param)

  # Row-by-row loop
  j <- params |>
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
  params_out <- params_out |>
    bind_cols(j) |>
    mutate(numest = ifelse(param == "Log natural mortality [$ln(M)$]" & numest != 0, model$dat$num.sex, numest))

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
    add_row(param = "Fishery age at 50\\% logistic selectivity ($\\hat{a}_k$)",
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = "Fishery SD of logistic selectivity ($\\hat{\\gamma}_k$)",
            numest = fish_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = "Survey age at 50\\% logistic selectivity ($\\hat{a}_k$)",
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None") |>
    add_row(param = "Survey SD of logistic selectivity ($\\hat{\\gamma}_k$)",
            numest = surv_est,
            bounds = "[0, 1]",
            prior = "None")

  # Catchability
  q <- model$ctl$surv.q
  num_inds <- model$ctl$num.indices
  params_out <- params_out |>
    add_row(param = "Survey catchability ($q_k$)",
            numest = num_inds,
            bounds = "[0, 1]",
            prior = paste0("Normal($",  round(exp(q[2, 1]), 1), ", ", round(q[3, 1], 1) ,"$)"))

  # Fishing mortality and recruitment parameters
  par <- model$par
  num_f_params <- length(par$log_ft_pars)
  num_rec_params <- length(par$log_rec_devs)
  num_init_rec_params <- length(par$init_log_rec_devs)
  params_out <- params_out |>
    add_row(param = "Log fishing mortality values ($\\Gamma_{k,t}$)",
            numest = num_f_params,
            bounds = "[-30, 3]",
            prior = "[-30, 3]") |>
    add_row(param = "Log recruitment deviations ($\\omega_t$)",
            numest = num_rec_params,
            bounds = "None",
            prior = "Normal($0, \\tau$)") |>
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
                    format = "latex",
                    ...)


  if(!is.null(col_widths)){
    tab <- tab |>
      column_spec(2:3, width = col_widths) |>
      column_spec(4, width = "15em")
  }

  tab
}

