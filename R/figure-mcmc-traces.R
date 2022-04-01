#' Plot parameter values from each posterior for the estimated parameters
#'
#' @param model An iscam model
#' @param plot_sel Logical. If `TRUE`, plot only the selectivity parameters.
#' If `FALSE`, plot only non-selectivity parameters
#'
#' @return A [ggplot2::gglot()] object
#' @export
plot_traces <- function(model, plot_sel = FALSE){

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

  g_lst <- imap(names(mc), ~{
    param_name <- get_latex_name(.x)
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
