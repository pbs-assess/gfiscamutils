#' Plot MCMC pairs for iSCAM models
#'
#' @description
#' Pairs plots for parameters in an MCMC run
#'
#' @details
#' Show correlations between parameters as scatterplots in the lower
#' triangular area, density of each parameter in the diagonals, and the correlation
#' value with text size scaled to be larger for higher correlations in the upper
#' triangular area
#'
#' @inheritParams plot_traces_mcmc
#' @family MCMC diagnostics plots
#'
#' @return A [ggplot2::ggplot()] object made from the [GGally::ggpairs()] function
#' @importFrom GGally ggpairs ggally_cor wrap ggally_text ggally_densityDiag
#' @importFrom ggplot2 layer_scales
#' @export
plot_pairs_mcmc <- function(model,
                            plot_sel = NULL,
                            param_rm = c("rho",
                                         "vartheta"),
                            list_param_names = FALSE,
                            text_title_size = 12){

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
  nms <- imap(names(mc), ~{
    get_fancy_expr(.x, subst = TRUE)
  })
  mc <- mc[1:50, ]
  names(mc) <- nms

  # Plot for the panels in the upper triangle of the pairs plot
  # @param data The plot data (passed from [ggAlly::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping  (passed from [ggAlly::ggpairs()])
  # @param ... Other arguments passed from [ggAlly::ggpairs()]
  # @return
  upper_triangle <- function(data,
                            mapping,
                            color = I("grey50"),
                            sizeRange = c(2.5, 3.5),
                            ...) {

    # Get the x and y data to use the other code
    x <- unlist(data[quo_name(mapping$x) == names(data)])
    y <- unlist(data[quo_name(mapping$y) == names(data)])

    ct <- cor.test(x,y)
    # Create significance stars
    # sig <- symnum(
    #   ct$p.value, corr = FALSE, na = FALSE,
    #   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    #   symbols = c("***", "**", "*", ".", " ")
    # )

    r <- unname(ct$estimate)
    rt <- format(r, digits = 2)[1]

    # Since we can't print it to get the strsize, just use the max size range
    cex <- max(sizeRange)

    # Helper function to calculate a usable size
    percent_of_range <- function(percent, range) {
      percent * diff(range) + min(range, na.rm = TRUE)
    }

    # Plot the cor value
    ggally_text(label = as.character(rt),
                mapping = aes(),
                xP = 0.5,
                yP = 0.5,
                size = I(percent_of_range(cex * abs(r), sizeRange)),
                color = color,
                ...) +
    # Add the significance stars
    # geom_text(aes_string(x = 0.8, y = 0.8),
    #             label = sig,
    #             size = I(cex),
    #             color = color,
    #             ...) +

    # Remove all the background stuff and wrap it with a dashed line
    theme_classic() +
    theme(panel.background = element_rect(color = color,
                                          linetype = "longdash"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank())
  }

  # Plot for the panels in the lower triangle of the pairs plot
  # @param data The plot data (passed from [ggAlly::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping  (passed from [ggAlly::ggpairs()])
  # @param ... Other arguments passed from [ggAlly::ggpairs()]
  # @return
  lower_triangle <- function(data, mapping, ...){

    # Round the axis tick values on each panel of the pairs plot to two
    # decimals unless they are all integers, then show no decimal points
    # @param x A vector of axis tick labels to format
    # @return A formatted vector of strings
    round_axis_vals <- function(x){
      if(all(x[!is.na(x)] %% 1 == 0)){
        sprintf("%d", as.integer(x))
      }else{
        sprintf("%.2f", x)
      }
    }

    ggplot(data = data, mapping = mapping) +
      geom_point(color = I("blue")) +
      geom_smooth(method = "lm", color = I("black"),
                  ...) +
      scale_y_continuous(labels = round_axis_vals) +
      scale_x_continuous(labels = round_axis_vals) +
      theme(#axis.ticks = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))
  }

  # Plot for the panels in the diagonals of the pairs plot
  # @param data The plot data (passed from [ggAlly::ggpairs()])
  # @param mapping The [ggplot2::aes()] aes mapping  (passed from [ggAlly::ggpairs()])
  # @param ... Other arguments passed from [ggAlly::ggpairs()]
  # @return
  diagonals <- function(data, mapping, ...){
    ggally_densityDiag(data, mapping) +
      theme(axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
  }

  g <- ggpairs(mc,
               labeller = "label_parsed",
               upper = list(continuous = upper_triangle),
               diag = list(continuous = diagonals),
               lower = list(continuous = lower_triangle)) +
    # Rotate the right-hand parameter names so they are the same as the rest
    theme(strip.text.y.right = element_text(angle = 0))

  if(!is.null(text_title_size)){
    g <- g + ggtitle(tex(model_desc)) +
      theme(plot.title = element_text(hjust = 0.5, size = text_title_size))
  }

  g
}
