#' Plot parameter values from each posterior for the estimated parameters
#'
#' @param model An iscam model
#' @param plot_sel Logical. If `TRUE`, plot only the selectivity parameters.
#' If `FALSE`, plot only non-selectivity parameters
#' @param param_rm A vector of parameter names to remove. If any are not found
#' in the output, a warning will be issued. If `NULL`, nothing is removed
#'
#' @return A [ggplot2::gglot()] object
#' @export
plot_traces <- function(model, plot_sel = FALSE, param_rm = NULL){

  if(class(model) != mdl_cls){
    stop("The `model` argument is not a gfiscamutils::mdl_cls class")
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

  # To list the parameter names, put a browser here and call this
  # in browser debug session:
  # names(mc)

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
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(face = "bold.italic", hjust = 0.5))
  })

  plot_grid(plotlist = g_lst)
}
