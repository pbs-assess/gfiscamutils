#' Plot the stock-recruitment relationship for an MCMC ISCAM model
#'
#' @inheritParams plot_ts_mcmc
#' @param plot_sr_curve If `TRUE`, plot the stock-recruitment curve
#' over the points
#' @export
#' @importFrom ggplot2 geom_point geom_errorbar geom_errorbarh
#' @importFrom tidyr pivot_wider pivot_longer
plot_sr_mcmc <- function(model,
                         probs = c(0.025, 0.5, 0.975),
                         errorbar_thickness = 0.1,
                         plot_sr_curve = FALSE){

  perc <- probs * 100
  perc[!perc %% 1] <- f(perc[!perc %% 1])
  perc <- paste0(perc, "%")

  syr <- as.character(model$dat$start.yr)
  nyr <- as.character(model$dat$end.yr + 1)
  sr <- model$mcmccalcs$sr_quants |>
    as_tibble(rownames = "quant")
  rt <- model$mcmccalcs$rt_quants |>
    as_tibble(rownames = "quant")
  sbt <- model$mcmccalcs$sbt_quants |>
    as_tibble(rownames = "quant") |>
    select(-c(syr, nyr))

  # Convert the outputs to long format for plotting,
  # replace the quantiles with lo, med, hi
  #
  # @param d Data frame with 4 rows, quantiles and MPD
  # @return Long data frame
  make_long <- function(d){
    d |>
      filter(quant != "MPD") |>
      mutate(quant = ifelse(quant == perc[1],
                             "lo", ifelse(quant == perc[2],
                                           "med", ifelse(quant == perc[3],
                                                         "hi", "unknown")))) |>
      pivot_longer(-quant, names_to = "year", values_to = "value")
  }

  rt_long <- make_long(rt) |>
    pivot_wider(names_from = "quant") |>
    setNames(c("year", "lo_rt", "med_rt", "hi_rt"))
  sbt_long <- make_long(sbt) |>
    pivot_wider(names_from = "quant") |>
    setNames(c("year", "lo_sbt", "med_sbt", "hi_sbt"))

  bio <- rt_long |>
    full_join(sbt_long, by = "year")

  g <- ggplot(bio, aes(x = med_sbt, y = med_rt, color = year)) +
    geom_point(shape = 1) +
    geom_errorbar(aes(ymin = lo_rt, ymax = hi_rt), size = errorbar_thickness) +
    geom_errorbarh(aes(xmin = lo_sbt, xmax = hi_sbt), size = errorbar_thickness)

  # Add S-R ribbon
  sr <- model$mcmccalcs$sr_quants |>
    as_tibble(rownames = "quant")

  sr_long <- sr |>
    filter(quant != "MPD") |>
    mutate(quant = ifelse(quant == perc[1],
                          "lo", ifelse(quant == perc[2],
                                       "med", ifelse(quant == perc[3],
                                                     "hi", "sbt")))) |>
    pivot_longer(-quant, names_to = "year", values_to = "value") |>
    pivot_wider(names_from = "quant") |>
    select(-year)

  if(plot_sr_curve){
    g <- g +
      geom_path(data = sr_long,
                aes(x = sbt, y = med),
                color = "blue",
                inherit.aes = FALSE) +
      geom_ribbon(data = sr_long,
                  aes(x = sbt, y = med, ymin = lo, ymax = hi),
                  color = "blue",
                  alpha = 0.1,
                  linetype = "dashed",
                  size = 0.5,
                  inherit.aes = FALSE)
  }

  g <- g +
    xlab("Spawning Biomass (thousand t)") +
    ylab("Recruitment (millions)")

  g
}
