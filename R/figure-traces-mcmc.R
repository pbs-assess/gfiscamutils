#' Trace plots for parameters in an MCMC run
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param plot_sel Logical. If `TRUE`, plot only the selectivity parameters.
#' If `FALSE`, plot only non-selectivity parameters
#' @param param_rm A vector of parameter names to remove. If any are not found
#' in the output, a warning will be issued. If `NULL`, nothing is removed
#' @param list_param_names Logical. If `TRUE`, list the parameter names that will be plot
#' by this function, but do not actually plot it. This is to help you to use the
#' `param_rm` argument, so you know what the names to be removed can be
#' @param ... Additional arguments passed to [cowplot::plot_grid()]
#'
#' @return A [ggplot2::gglot()] object
#' @export
plot_traces_mcmc <- function(model,
                             plot_sel = FALSE,
                             param_rm = NULL,
                             list_param_names = FALSE,
                             ...){

  if(class(model) != mdl_cls){
    if(class(model) == mdl_lst_cls){
      stop("The `model` argument is of class `gfiscamutils::mdl_lst_cls`. ",
           "This function requires that `model` be an object of class ",
           "`gfiscamutils::mdl_cls`. Select one element of the list (which ",
           "is a single model), and modify it like this, then call this function ",
           "again with `model` as your argument:\n\n",
           "model <- list(model)\n",
           "class(model) <- mdl_lst_cls\n")
    }
    stop("The `model` argument is not of class `gfiscamutils::mdl_cls`")
  }

  mc <- model$mcmc$params %>%
    as_tibble()

  if(plot_sel){
    mc <- mc %>%
      select(contains("sel"))
  }else{
    mc <- mc %>%
      select(-contains("sel"))
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
      select(.x) %>%
      rename(y = !!.x) %>%
      mutate(x = row_number())

    g <- ggplot(param_dat, aes(x, y)) +
      geom_point(size = 0.5, color = "blue") +
      geom_line(color = "darkblue") +
      ggtitle(param_name) +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(face = "bold.italic", hjust = 0.5))
  })

  plot_grid(plotlist = g_lst, ...)
}
