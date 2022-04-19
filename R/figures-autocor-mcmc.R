#' Autocorrelation plots for parameters in an MCMC run
#'
#' @rdname plot_traces_mcmc
#'
#' @return A [ggplot2::ggplot()] object made from the [forecast::ggAcf()] function
#' @importFrom forecast ggAcf
#' @export
plot_autocor <- function(model,
                         plot_sel = FALSE,
                         param_rm = NULL,
                         lag = 100,
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
      select(.x)

    g <- ggAcf(param_dat, lag.max = lag) +
      ggtitle(param_name) +
      ylab("") +
      xlab("") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = "bold.italic", hjust = 0.5))

    })

  plot_grid(plotlist = g_lst, ...)
}
