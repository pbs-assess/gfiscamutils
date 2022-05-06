#' Make a table of reference points for MCMC iSCAM models
#'
#' @description
#' Make a table of reference points for MCMC iSCAM models
#'
#' @inheritParams table_param_est_mcmc
#' @param bo_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.2B0 and 0.4B0 by default
#' @param bmsy_refpts Vector of two proportional values for the limit reference
#' point and Upper stock reference. Values are 0.4BMSY and 0.8BMSY by default
#' @param ord A vector of numbers signifying the order in which the rows should
#' appear in the table based on what they were by default. If `NULL` the
#' default order will be shown, then based on what you see, you decide what
#' row order to supply to this argument. If too short or too long a vector
#' is supplied, an error will be thrown
#'
#' @return A [csasdown::csas_table()]
#' @importFrom purrr imap_dfr
#' @export
table_ref_points_mcmc <- function(models,
                                  type = c("median", "ci"),
                                  bo_refpts = c(0.2, 0.4),
                                  bmsy_refpts = c(0.4, 0.8),
                                  digits = 2,
                                  probs = c(0.025, 0.5, 0.975),
                                  model_col_widths = "5em",
                                  ord = NULL,
                                  ...){

  type <- match.arg(type)

  if(is_iscam_model(models)){
    models <- list(models)
    class(models) <- mdl_lst_cls
  }

  if(!is_iscam_model_list(models)){
    stop("`models` does not have class `gfiscamutils::mdl_lst_cls`.",
         call. = FALSE)
  }

  if(length(probs) != 3){
    stop("`probs` has length ", length(probs), " but must be a vector of three values ",
         "representing lower CI, median, and upper CI",
         call. = FALSE)
  }

  param_col_name <- ifelse(fr(), "Paramètre", "Parameter")

  # Match the given probs with their respective quant columns
  prob_cols <- paste0(prettyNum(probs * 100), "%")

  tab_lst <- imap(models, ~{

    j <- .x$mcmccalcs$params_quants %>%
      t() %>%
      as_tibble(rownames = "param")

    quants <- imap_chr(prob_cols, ~{
      mtch <- grep(.x, names(j), value = TRUE)
      if(!length(mtch)){
        stop("One of the values in `probs` does not appear in the MCMC output data\n",
             .x, call. = FALSE)
      }
      mtch
    })
    # Select out the 'parameters' to use for the table
    j <- j %>%
      filter(grepl("^sbo$|^bmsy$|^fmsy_|^umsy_|^msy", param))
    end_yr <- .x$dat$end.yr

    # Add fishing mortality in the final year
    ft <- .x$mcmccalcs$ft_quants %>%
      imap_dfr(~{
        tmp <- .x %>%
          as_tibble(rownames = "quants") %>%
          filter(quants != "MPD") %>%
          select(quants, !!sym(as.character(end_yr))) %>%
          t() %>%
          as_tibble(rownames = "param")
        names(tmp)[-1] <- tmp[1, -1]
        tmp <- tmp[-1, ]

        tmp %>%
          mutate(param = paste0("f_fleet", .y, "_", param)) %>%
          mutate_at(vars(-param), ~{as.numeric(.)})
      })
    # Add spawning biomass in the final year and next year
    sbt <- .x$mcmccalcs$sbt_quants %>%
      as_tibble(rownames = "quants") %>%
      filter(quants != "MPD") %>%
      select(quants,
             !!sym(as.character(end_yr)),
             !!sym(as.character(end_yr + 1))) %>%
      t() %>%
      as_tibble(rownames = "param")
    names(sbt)[-1] <- sbt[1, -1]
    sbt <- sbt[-1, ] %>%
      mutate(param = paste0("sbt_", param)) %>%
      mutate_at(vars(-param), ~{as.numeric(.)})
    j <- j %>%
      bind_rows(ft) %>%
      bind_rows(sbt)
    # Add ref point calculations for B0 and BMSY
    bo_refpt_df <- map(bo_refpts, ~{
      nm <- paste0(.x, "B0")
      row <- j %>% filter(param == "sbo")
      row$param <- nm
      row %>% mutate_at(vars(-param), function(refpt = .x){.x * refpt})
    }) %>%
      bind_rows()
    j <- j %>%
      bind_rows(bo_refpt_df)
    bmsy_refpt_df <- map(bmsy_refpts, ~{
      nm <- paste0(.x, "BMSY")
      row <- j %>% filter(param == "bmsy")
      row$param <- nm
      row %>% mutate_at(vars(-param), function(refpt = .x){.x * refpt})
    }) %>%
      bind_rows()
    j <- j %>%
      bind_rows(bmsy_refpt_df)

    median_df <<- j %>%
      mutate(val = f(!!sym(quants[2]), digits)) %>%
      select(param, val)
    ci_df <<- j %>%
      mutate(val = paste0(trimws(f(!!sym(quants[1]), digits)),
                          "-",
                          trimws(f(!!sym(quants[3]), digits)))) %>%
      select(param, val)

    if(type == "median" || length(models) == 1){
      out <- median_df
    }else{
      out <- ci_df
    }
    out
  })

  # Full join them all, one by one
  tab <- tab_lst[[1]]
  if(length(models) > 1){
    for(i in 2:length(tab_lst)){
      tab <- full_join(tab, tab_lst[[i]], by = "param")
    }
  }

  if(length(models) == 1){
    # Add the credible interval column
    tab <- full_join(tab, ci_df, by = "param") %>%
      mutate(param = get_fancy_names(param))
    if(fr()){
      names(tab) <- c("Point de référence", "Médiane", "Intervalle crédible")
    }else{
      names(tab) <- c("Reference point", "Median", "Credible interval")
    }
  }else{
    tab <- tab %>%
      mutate(param = get_fancy_names(param)) %>%
      rename(!!sym(param_col_name) := param)
    names(tab)[-1] <- names(models)
  }

  # Replace NA's in the table with dashes
  tab[is.na(tab)] <- "--"

  if(!is.null(ord)){
    if(length(ord) < nrow(tab) || length(ord) > nrow(tab)){
      stop("`ord` is not the same length as the number of rows in tab (",
           nrow(tab),
           call. = FALSE)
    }
    if(!all(diff(sort(ord)) == 1)){
      stop("`ord` does not contain all the values from 1 to ", nrow(tab),
           call. = FALSE)
    }
    tab <- tab[ord, ]
  }
  out <- csas_table(tab,
                    format = "latex",
                    align = rep("r", ncol(tab)),
                    col_names_align = rep("r", ncol(tab)),
                    ...)

  if(!is.null(model_col_widths)){
    out <- out %>%
      column_spec(2:ncol(tab), width = model_col_widths)
  }

  out
}

