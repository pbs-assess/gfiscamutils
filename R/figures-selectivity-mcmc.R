#' Plot the selectivity for all gears in the iscam model
#'
#' @param model An iscam model object as output by [arrowtooth::model_setup()]
#' @param show_maturity If `TRUE`, overlay the maturity ogive on the selectivity plots
#' @param last_year_only If `TRUE`, only show the last year. This would be used for non-time-varying selectivities
#' @param title The plot title. NULL means no title
#'
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_selex_mcmc <- function(model,
                            show_maturity = FALSE,
                            last_year_only = TRUE,
                            title = NULL){

  est_phz <- model$ctl$sel %>%
    as_tibble(rownames = "x") %>%
    `names<-`(c("sel_setting", model$dat$gear_abbrevs))

  est_phz <- as_tibble(cbind(nms = names(est_phz), t(est_phz)))
  nms <- est_phz %>% slice(1) %>% unlist(., use.names = FALSE)
  nms[1] <- "index"
  names(est_phz) <- nms
  est_phz <- est_phz[-1,]
  est_phz <- est_phz %>% mutate_at(vars(-index), function(x)as.numeric(x))

  # Append " (Fixed)" to indices which have a negative phase
  est_phz <- est_phz %>%
    mutate(index = ifelse(estphase < 0, paste(index, "(Fixed)"), index))

  # Extract selectivity parameters

  browser()
}
