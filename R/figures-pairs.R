#' Plot the estimated parameters in a model against each other
#' as a pairs plot
#'
#' @param model An iscam model
#'
#' @return A [ggplot2::ggplot()] object made from the [GGally::ggpairs()] function
#' @importFrom GGally ggpairs ggally_cor wrap ggally_text ggally_densityDiag
#' @importFrom ggplot2 layer_scales
#' @export
plot_pairs <- function(model, plot_sel = FALSE, param_rm = NULL){

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
  nms <- imap(names(mc), ~{
    get_fancy_name(.x, subst = TRUE)
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
    # Add the sig stars
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

  g
}
