#' Plot reference point boxplots
#'
#' @param model An iscam model object (class [mdl_cls])
#'
#' @return A [cowplot::plot_grid()] of [ggplot2::ggplot()] box plots
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
                                         expression(italic(UMSY)[1]),
                                         expression(italic(UMSY)[2]))){

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
    ggplot(j, aes(x = unity, y = value)) +
      geom_boxplot() +
      expand_limits(y = 0) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ylab("") +
      xlab("") +
      ggtitle(ref_desc[.x])
  })

  plot_grid(plotlist = plot_lst, ncol = 2)
}
