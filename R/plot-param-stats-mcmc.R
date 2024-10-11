#' Plot Autocorrelation, Effective sample size, Geweke statistic, and
#' Heidelberger & Welch statistic histograms. This is a re-coded version of
#' https://github.com/r4ss/r4ss/blob/bioscale/R/mcmc.nuisance.R
#'
#' @param model A model as returned by [load_rds_file()]
#' @param col A color to shade the bars
#' @param effn_labels If TRUE, add labels to the top of the bars on the
#' effective sample size plot
#' @param ylim_mult Multiply the maximum ylim value by this (to show
#' labels on bars better if `effn_labels` is `TRUE`)
#' @param show_ro_loc If `TRUE`, place the text "R0" over the bar in which
#' the R0 parameter falls
#' @param ro_yloc If `show_ro_loc` is `TRUE`, use this value as a y value for
#' the text
#' @param ro_yloc_arr_start If `show_ro_loc` is `TRUE`, use this value as a y
#' value for the start of the arrow pointing to the bar where R0 is
#' @param ro_yloc_arr_end If `show_ro_loc` is `TRUE`, use this value as a y
#' value for the end of the arrow pointing to the bar where R0 is
#'
#' @return A [barplot()]
#' @export
plot_param_stats_mcmc <- function(model,
                                  col = get_shade(color = "blue", opacity = 30),
                                  effn_labels = FALSE,
                                  ylim_mult = 1,
                                  show_ro_loc = FALSE,
                                  ro_yloc = 50,
                                  ro_yloc_arr_start = 60,
                                  ro_yloc_arr_end = 30){

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

  oldpar <- par("mar", "oma")
  on.exit(par(oldpar))
  par(mar = c(5, 4, 0, 0.5),
      oma = c(0, 0, 0.5, 0.5))

  parm_nm <- names(mc)
  parm_nm <- parm_nm[parm_nm != ""]

  # Remove columns where all values are equal
  mc <- map_df(mc, ~{
    if(length(unique((.x))) == 1){
      NULL
    }else{
      .x
    }
  })

  draws <- nrow(mc)
  stats <- bind_cols(enframe(rep(0, length(parm_nm)), name = NULL, value = "autocor"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "geweke"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "effn"),
                     enframe(rep(0, length(parm_nm)), name = NULL, value = "heidelwelsch"),
                     enframe(parm_nm, name = NULL, value = "label"))

  hwsums <- c(0, 0, 0)

  ro_loc <- NULL
  map2(mc, seq_along(mc), ~{
    acftemp <- acf(.x, lag.max = 1, type = "correlation", plot = FALSE)
    acoruse <- round(acftemp$acf[2], 3)
    stats$autocor[.y] <<- acoruse

    # Geweke statistic
    if(acoruse > 0.4){
      gewuse <- 3
    }else{
      geweke <- geweke.diag(mcmc(.x), frac1 = 0.1, frac2 = 0.5)
      gewuse <- round(geweke$z, 3)
    }
    if(gewuse > 3){
      gewuse <- 3
    }else if(gewuse < -3){
      gewuse <- -2.9
    }
    stats$geweke[.y] <<- gewuse

    # Effective sample size
    x <- enframe(.x, name = NULL)
    spec <- spectrum0.ar(x)$spec
    effsize <- round(ifelse(spec == 0, 0, nrow(x) * var(x) / spec), 0)
    stats$effn[.y] <<- min(effsize, draws)
    # Heidelberger and Welch statistic
    if(acoruse > 0.4){
      hwuse <- `if`(fr(), "Pas de test", "No test")
      hwsums[1] <<- hwsums[1] + 1
    }else{
      hw <- as.list(heidel.diag(mcmc(.x), pvalue = 0.05))
      if(hw[1] == 0){
        hwuse <- `if`(fr(), "\u00C9chou\u00E9", "Failed")
        hwsums[2] <<- hwsums[2] + 1
      }else if(hw[1] == 1){
        hwuse <- `if`(fr(), "Adopt\u00E9", "Passed")
        hwsums[3] <<- hwsums[3] + 1
      }
    }
    stats$heidelwelsch[.y] <<- hwuse
    NULL
  })

  # Plotting section
  par(new = FALSE,
      mfrow = c(2, 2))

  hist(stats$autocor,
       main = "",
       col = col,
       breaks = c(seq(-1, 1, by = 0.1)),
       xlim = c(-1, 1),
       xlab = tr("Autocorrelation"),
       ylab = tr("Frequency"))

  j <- hist(stats$effn,
            breaks = c(seq(0, draws, by = (draws / 10))),
            plot = FALSE)

  hist(stats$effn,
       main = "",
       ylab = "",
       xlab = `if`(fr(),
                   "Taille effective de l'\u00E9chantillon",
                   "Effective sample size"),
       breaks = c(seq(0, draws, by = (draws / 10))),
       xlim = c(0, draws),
       ylim = c(0, max(j$counts) * ylim_mult),
       labels = effn_labels,
       col = col)
  if(show_ro_loc && !is.null(ro_loc)){
    freq_ind <- first(which(ro_loc < j$breaks)) - 1
    x_loc <- (j$breaks[freq_ind] + j$breaks[freq_ind + 1]) / 2
    arrows(x0 = x_loc, y0 = ro_yloc_arr_start, x1 = x_loc, y1 = ro_yloc_arr_end, length = 0.05)
    text(x = x_loc,
         y = ro_yloc,
         paste0("R0 (", ro_loc, ")"))
  }

  hist(stats$geweke,
       main = "",
       xlab = `if`(fr(),
                   "Statistique Geweke",
                   "Geweke statistic"),
       ylab = tr("Frequency"),
       breaks = c(seq(-5, 5, by = 0.25)),
       xlim = c(-3, 3),
       right = TRUE,
       col = col)

  arg_names <- `if`(fr(),
                    c("Pas de test", "\u00C9chou\u00E9", "Adopt\u00E9"),
                    c("No test", "Failed", "Passed"))
  barplot(hwsums,
          space = 0,
          ylab = "",
          col = col,
          xlab = `if`(fr(),
                      "Statistiques Heidelberger et Welch",
                      "Heidelberger and Welch statistic"),
          names.arg = arg_names)
}
