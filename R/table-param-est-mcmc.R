#' Create a table of parameter estimates for iSCAM models
#'
#' @description
#' Create a table parameter estimates and priors for iSCAM models
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param digits Number of decimal places for the values in the table
#' @param probs A 3-element vector of probabilities that appear in the output
#' data frames. This is provided in case the data frames have more than three
#' different quantile levels
#' @param model_col_widths Widths for columns, except the Parameter column
#' @param ... Arguments to pass to [csasdown::csas_table()]
#'
#' @return A [csasdown::csas_table()]
#' @export
table_param_est_mcmc <- function(model,
                                 digits = 2,
                                 probs = c(0.025, 0.5, 0.975),
                                 model_col_widths = NULL,
                                 ...){

  if(!is_iscam_model(model)){
    stop("`model` does not have class `gfiscamutils::mdl_cls`.",
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI",
         call. = FALSE)
  }

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")
  # In case the decimals have been changed to commas, change them back
  prob_cols <- gsub(",", ".", prob_cols)

  params_quants <- model$mcmccalcs$params_quants |>
    t() |>
    as_tibble(rownames = "param")


  sel_pat <- "^sel_(age|sd)50_(male|female)_gear([0-9]+)_block([0-9]+)$"

  params_quants <- params_quants |>
    mutate(is_selex = ifelse(grepl("^sel", param),
                             TRUE,
                             FALSE))

  year_range <- paste0(model$dat$start.yr, "-", model$dat$end.yr)

  non_selex_df <- params_quants |>
    filter(!is_selex) |>
    select(-is_selex) |>
    mutate(gear = "--", sex = "--", `Year range` = year_range)

  # Deal with the M parameters
  m_inds <- grep("^m_sex[0-2]*", non_selex_df$param)
  if(length(m_inds)){
    non_selex_df <- non_selex_df |>
      mutate(sex = ifelse(param == "m_sex1",
                          "male",
                          sex),
             sex = ifelse(param == "m_sex2",
                          "female",
                          sex),
             param = ifelse(param == "m_sex1",
                            "m1",
                            param),
             param = ifelse(param == "m_sex2",
                            "m2",
                            param))

    m_param_nms <- non_selex_df |>
      filter(grepl("^m[0-9]+$", param)) |>
      pull(param)
    m_sex_inds <- as.numeric(gsub("m([0-9]+)", "\\1", m_param_nms))
  }
  # Deal with q parameters
  q_real_nms <- model$dat$index_gear_names
  q_inds <- grep("^q_gear", non_selex_df$param)
  if(length(q_inds)){
    q_param_nms <- non_selex_df |>
      filter(grepl("^q_gear", param)) |>
      pull(param)
    q_gear_inds <- as.numeric(gsub("q_gear([0-9]+)", "\\1", q_param_nms))
    non_selex_df[q_inds, "gear"] <- q_real_nms[q_gear_inds]
  }
  # Deal with MSY parameters
  flt_real_nms <- model$dat$fleet_gear_names
  flt_inds <- grep("fleet", non_selex_df$param)
  if(length(flt_inds)){
    flt_param_nms <- non_selex_df |>
      filter(grepl("fleet", param)) |>
      pull(param)
    flt_gear_inds <- as.numeric(gsub(".*_fleet([0-9]+)$", "\\1", flt_param_nms))
    non_selex_df[flt_inds, "gear"] <- flt_real_nms[flt_gear_inds]
  }
  # Deal with selectivity parameters
  non_selex_df <- non_selex_df |>
    mutate(param = get_fancy_names(param))

  selex_df <- params_quants |>
    filter(is_selex) |>
    select(-is_selex) |>
    mutate(gear = gear_names[as.numeric(gsub(sel_pat, "\\3", param))]) |>
    mutate(sex = gsub(sel_pat, "\\2", param)) |>
    mutate(block = as.numeric(gsub(sel_pat, "\\4", param))) |>
    mutate(gear_num = as.numeric(gsub(sel_pat, "\\3", param)))

  sel_nms <- selex_df$param

  block_year_ranges <- model$mcmccalcs$selest_quants |>
    select(gear, block, start_year, end_year) |>
    distinct() |>
    mutate(`Year range` = paste0(start_year, "-", end_year)) |>
    select(-c(start_year, end_year))

  selex_df <- selex_df |>
    left_join(block_year_ranges, by = c("gear", "block")) |>
    mutate(param = gsub("^sel_(age|sd)50(.*)$", "\\1", param))

  selex_df <- selex_df |>
    mutate(param = ifelse(param =="age",
                          paste0("$\\hat{a}_\\mathrm{", gear_num, ",", substr(sex, 1, 1), ",", block, "}$"),
                          ifelse(param == "sd",
                                 paste0("$\\hat{\\gamma}_\\mathrm{", gear_num, ",", substr(sex, 1, 1), ",", block, "}$"),
                                 param))) |>
    select(-gear_num) |>
    select(-block)

  params_quants <- non_selex_df |>
    bind_rows(selex_df) |>
    select(param, gear, sex, `Year range`, everything())

  nms <- names(params_quants)

  nms[1:3] <- c(ifelse(fr(), "Paramètre", "Parameter"),
                ifelse(fr(), "Engrenage", "Gear"),
                ifelse(fr(), "Sexe", "Sex"))

  if(fr()){
    nms <- gsub("%", " %", nms)
  }
  names(params_quants) <- nms

  quants <- imap_chr(prob_cols, ~{
    mtch <- grep(.x, names(params_quants), value = TRUE)
    if(!length(mtch)){
      stop("One of the values in `probs` does not appear in the MCMC output data\n",
           .x, call. = FALSE)
    }
    mtch
  })

  names(params_quants) <- gsub("%", "\\\\%", names(params_quants))

  params_quants <-  mutate_if(params_quants, is.numeric, ~{f(., digits)})

  out <- csas_table(params_quants,
                    format = "latex",
                    align = rep("r", ncol(params_quants)),
                    col_names_align = rep("r", ncol(params_quants)),
                    ...)

  if(!is.null(model_col_widths)){
    out <- out |>
      column_spec(2:ncol(params_quants), width = model_col_widths)
  }

  out
}
