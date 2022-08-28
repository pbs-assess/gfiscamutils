#' Plot MCMC autocorrelations for iSCAM models
#'
#' @description
#' Autocorrelation plots for parameters in an MCMC run
#'
#' @inheritParams plot_traces_mcmc
#' @family MCMC diagnostics plots
#' @param ... Arguments passed to [base::plot()]
#'
#' @return Nothing. Plots a grid of autocorrelation plots
#' @export
plot_autocorr_mcmc <- function(model,
                               plot_sel = NULL,
                               param_rm = c("rho",
                                            "vartheta"),
                               list_param_names = FALSE,
                               text_title_size = 12,
                               rows_cols = c(5, 4),
                               ...){

  oldpar <- par()
  on.exit(par(oldpar))

  if(is_iscam_model_list(model) && length(model) == 1){
    model <- model[[1]]
  }

  if(!is_iscam_model(model)){
    if(is_iscam_model_list(model)){
      stop("`model` is not an iscam model object, it is an iscam model ",
           "list object")
    }
    stop("`model` is not an iscam model object")
  }

  # Set up model description for the title
  model_desc <- as.character(attributes(model)$model_desc)

  mc <- model$mcmc$params %>%
    as_tibble()

  if(!is.null(plot_sel)){
    if(plot_sel){
      # Choose only selectivity columns and remove any with the same
      # values (fixed)
      mc <- mc %>%
        select(contains("sel")) |>
        map_df(~{`if`(var(.x) == 0, NULL, .x)})
    }else{
      mc <- mc %>%
        select(-contains("sel"))
    }
  }

  if(list_param_names){
    message("The names of the parameters this function will plot (by default) are:")
    print(names(mc))
    return(invisible())
  }

  # Remove parameters requested for removal, ignoring erroneous ones
  if(!is.null(param_rm)){
    param_rm <- imap(param_rm, ~{
      if(!.x %in% names(mc)){
        warning("Parameter name '", .x, "' not found in model$mcmc$params, ignoring")
        return(NULL)
      }
      .x
    })
    param_rm <- param_rm[!sapply(param_rm, is.null)] %>%
      map_chr(~{.x})
    if(!is.null(param_rm) && length(param_rm)){
      mc <- mc %>%
        select(-param_rm)
    }
  }

  par(mfrow = rows_cols,
      oma = c(2, 3, 1, 1),
      mai = c(0.2, 0.4, 0.3, 0.2))

  walk(names(mc), function(param, ...){
    param_name <- get_fancy_expr(param)
    param_dat <- mc %>%
      select(param)
    autocorr_plot(param_dat,
                  main = param_name,
                  auto_layout = FALSE,
                  ...)
  }, ...)
  invisible()
}
