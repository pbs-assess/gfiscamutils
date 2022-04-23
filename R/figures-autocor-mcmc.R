#' Autocorrelation plots for parameters in an MCMC run
#'
#' @rdname plot_traces_mcmc
#'
#' @family MCMC diagnostics plots
#' @return A [ggplot2::ggplot()] object made from the [forecast::ggAcf()] function
#' @importFrom forecast ggAcf
#' @export
plot_autocor <- function(model,
                         plot_sel = NULL,
                         param_rm = c("rho",
                                      "vartheta"),
                         lag = 100,
                         list_param_names = FALSE,
                         text_title_size = 12,
                         ...){

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
      mc <- mc %>%
        select(contains("sel"))
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

  g_lst <- imap(names(mc), ~{
    param_name <- get_fancy_name(.x)
    param_dat <- mc %>%
      select(.x)

    g <- ggAcf(param_dat, lag.max = lag) +
      ggtitle(param_name) +
      ylab("") +
      xlab("") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = "bold.italic", hjust = 0.5))

    })

  if(is.null(text_title_size)){
    plot_grid(plotlist = g_lst, ...)
  }else{
    title <- ggdraw() +
      draw_label(model_desc,
                 size = text_title_size,
                 x = 0.5)
    p <- plot_grid(plotlist = g_lst, ...)
    plot_grid(title, p, ncol = 1, rel_heights = c(0.05, 1), ...)
  }

}
