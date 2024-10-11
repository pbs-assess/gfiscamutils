#' Plot MCMC traces for iSCAM models
#'
#' @description
#' Trace plots for parameters in an MCMC run
#'
#' @family MCMC diagnostics plots
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param plot_sel Logical. If `TRUE`, plot only the selectivity parameters.
#' If `FALSE`, plot only non-selectivity parameters. If `NULL`, plot all parameters
#' except those in `param_rm`
#' @param param_rm A vector of parameter names to remove. If any are not found
#' in the output, a warning will be issued. If `NULL`, nothing is removed
#' @param list_param_names Logical. If `TRUE`, list the parameter names that will be plot
#' by this function, but do not actually plot it. This is to help you to use the
#' `param_rm` argument, so you know what the names to be removed can be
#' @param text_title_size Add the model description as a title with this font size. The text
#' comes from the `model_desc` attribute of `model`. If this is `NULL`, don't show a title
#' @param ... Additional arguments passed to [cowplot::plot_grid()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_traces_mcmc <- function(model,
                             plot_sel = FALSE,
                             param_rm = c("rho",
                                          "vartheta"),
                             list_param_names = FALSE,
                             text_title_size = 12,
                             ...){

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

  g_lst <- imap(names(mc), ~{
    param_name <- get_fancy_expr(.x)
    param_dat <- mc %>%
      select(.x) %>%
      rename(y = !!.x) %>%
      mutate(x = row_number())

    g <- ggplot(param_dat, aes(x, y)) +
      #geom_point(size = 0.01, color = "blue") +
      geom_line(color = "darkblue",
                size = 0.05) +
      ggtitle(param_name) +
      ylab("") +
      xlab("") +
      theme(plot.title = element_text(face = "bold.italic", hjust = 0.5),
            plot.margin = margin(r = 0, b = 0, l = 0, unit = "pt")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.55, vjust = 0.5))

  })

  if(is.null(text_title_size)){
    patchwork::wrap_plots(g_lst)
    # plot_grid(plotlist = g_lst, ...)
  }else{
    title <- ggdraw() +
      draw_label(tex(model_desc),
                 size = text_title_size,
                 x = 0.5)
    p <- plot_grid(plotlist = g_lst, ...)
    plot_grid(title, p, ncol = 1, rel_heights = c(0.05, 1), ...)
  }
}
