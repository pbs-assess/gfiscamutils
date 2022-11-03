#' Plot DFO-style reference point figure with horizontal boxplots and zones
#' bounded by B0 reference points
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param biomass_col The colomn name that you want to shoe output for.
#' This column will be divided by the SB0 value to get a depletion for
#' the year in the name of the column
#'
#' @export
plot_dfo_ref_point_figure <- function(model,
                                      biomass_col = "B2023",
                                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975)){

  sym_biomass_col <- sym(biomass_col)
  yr <- as.numeric(str_extract(biomass_col, pattern = "[0-9]+"))

  sbo <- model$mcmccalcs$params$sbo

  # Make a list by TAC (Catch level)
  tac_lst <- as_tibble(model$mcmc$proj) |>
    split(~TAC)

  # This is the depletion quantile table for the year given by the
  # argument `biomass_col`
  depl_quants_yr <- map_dfr(tac_lst, ~{
    tac <- unique(.x$TAC)
    q <- .x |> mutate(depl = !!sym_biomass_col / sbo) |> pull(depl) |>
      quantile(probs = probs)
    c(tac, q)
  }) |>
    t() |>
    as_tibble() |>
    setNames(c("tac", "lo", "lomed", "med", "medhi", "hi")) |>
    mutate(tac = as.integer(tac))

  g <- ggplot(depl_quants_yr, aes(x = tac, y = med)) +
    geom_hline(aes(yintercept = 0.2), color = "red", size = 1) +
    geom_hline(aes(yintercept = 0.4), linetype = "dashed", color = "green", size = 1) +
    geom_pointrange(aes(ymin = lomed, ymax = medhi), size = 1, fatten = 1) +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = 0.5) +
    scale_x_continuous(labels = as.character(depl_quants_yr$tac), breaks = depl_quants_yr$tac) +
    ylab(paste0("Relative spawning biomass in ", yr)) +
    xlab(paste0("Catch in ", yr - 1, " (thousand t)")) +
    coord_flip()
  g
}
