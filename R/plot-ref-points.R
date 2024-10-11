#' Plot reference point boxplots
#'
#' @param model An iscam model object (class [mdl_cls])
#'
#' @return A [cowplot::plot_grid()] of [ggplot2::ggplot()] box plots
#' @importFrom ggplot2 geom_histogram
#' @param ref_pts A vector of the names of reference points to plot
#' @param ref_desc A vector of fancy descriptions for the values in `ref_pts`
#' @param color The histogram color (outline of bars)
#' @param fill The histogram bar fill color
#' @export
plot_ref_points <- function(model,
                            ref_pts = c("sbo",
                                        "bmsy",
                                        "msy1",
                                        "msy2",
                                        "umsy1",
                                        "umsy2"),
                            ref_desc = c(expression(italic(SB)[0]),
                                         expression(italic(B)[MSY]),
                                         expression(italic(MSY)[1]),
                                         expression(italic(MSY)[2]),
                                         expression(italic(U[MSY])[1]),
                                         expression(italic(U[MSY])[2])),
                            color = "black",
                            fill = "steelblue"){

  if(fr()){
    ref_desc <- c(expression(italic(BR)[0]),
                  expression(italic(B)[RMD]),
                  expression(italic(RMD)[1]),
                  expression(italic(RMD)[2]),
                  expression(italic(U[RMD])[1]),
                  expression(italic(U[RMD])[2]))
  }
  ref_pts_syms <- syms(ref_pts)

  params <- model$mcmc$params |>
    as_tibble() |>
    select(!!!ref_pts_syms)

  plot_lst <- map(seq_along(ref_pts), ~{
    j <- params[, .x]
    k <- enframe(rep(1, nrow(params)), name = NULL)
    names(k) <- "unity"
    j <- bind_cols(k, j)
    names(j)[2] <- "value"
    ggplot(j, aes(x = value)) +
      geom_histogram(color = color,
                     fill = fill) +
      expand_limits(x = 0) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ylab("") +
      xlab("") +
      ggtitle(ref_desc[.x])
  })

  plot_grid(plotlist = plot_lst, ncol = 2)
}
