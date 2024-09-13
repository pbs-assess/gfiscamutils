#' Plot DFO-style reference point figure with horizontal boxplots and zones
#' bounded by B0 reference points
#'
#' @param model An iscam model object (class [mdl_cls])
#' @param biomass_col The colomn name that you want to shoe output for.
#' This column will be divided by the SB0 value to get a depletion for
#' the year in the name of the column
#' @param proj_catch_vals The catch values to include in the plot. If `NULL`,
#' include all values present in the list from the output
#' (`model$mcmccalcs$proj_sbt_quants`). If non-null, is a vector of values
#' of catch to filter the projection biomass table on. Only show those values
#' in the plot
#' @param line_thickness Thickness of reference point lines (vertical lines)
#'
#' @export
plot_ref_points_dist_mcmc <- function(model,
                                      biomass_col = "B2023",
                                      probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
                                      proj_catch_vals = NULL,
                                      line_thickness = 0.75){

  sym_biomass_col <- sym(biomass_col)
  yr <- as.numeric(str_extract(biomass_col, pattern = "[0-9]+"))

  sbo <- model$mcmccalcs$params$sbo

  proj <- model$mcmc$proj
  if(!is.null(proj_catch_vals[1])){
    proj <- proj |>
      filter(TAC %in% proj_catch_vals)
  }
  # Make a list by TAC (Catch level)
  tac_lst <- as_tibble(proj) |>
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

  # X-axis tick label specs
  brks <- c(0.0, 0.2, 0.35, 0.4, 0.6, 0.8, 1.0)
  cols <- c("black", "red", "blue", "green", "black", "black")
  sz <- c(10, 14, 14, 14, 10, 10)

  g <- depl_quants_yr |>
    ggplot(aes(x = tac, y = med)) +
    geom_hline(aes(yintercept = 0.2),
               color = "red",
               size = line_thickness) +
    geom_hline(aes(yintercept = 0.35),
               linetype = "dotted",
               color = "blue",
               size = line_thickness) +
    geom_hline(aes(yintercept = 0.4),
               linetype = "dashed",
               color = "green",
               size = line_thickness) +
    geom_pointrange(aes(ymin = lomed,
                        ymax = medhi),
                    size = 1,
                    fatten = 1) +
    geom_pointrange(aes(ymin = lo, ymax = hi),
                    size = 0.5) +
    scale_y_continuous(limits = c(0, 1),
                       labels = brks,
                       breaks = brks,
                       expand = c(0, 0)) +
    theme(axis.text.x = element_text(color = cols, size = sz)) +
    scale_x_continuous(labels = as.character(depl_quants_yr$tac),
                       breaks = depl_quants_yr$tac) +
    ylab(paste0("Relative spawning biomass in ", yr)) +
    xlab(paste0("Catch in ", yr - 1, " (kt)")) +
    coord_flip()
  g
}
