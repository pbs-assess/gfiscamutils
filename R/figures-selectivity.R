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
plot_selex <- function(model,
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

  # Selex
  selex <- model$mpd$sel %>% as_tibble()
  age <- model$mpd$age
  log_sel <- model$mpd$log_sel %>% as_tibble() %>%
    `names<-`(c("gear", "type", "year", age)) %>%
    select(year, gear, as.character(age)) %>%
    mutate_at(vars(-c("year", "gear")), function(x){exp(x)})

  # Make long
  ls <- log_sel %>%
    pivot_longer(!c(year, gear), names_to = "Age", values_to = "value") %>%
    mutate(Year = year,
           Proportion = value,
           Age = factor(as.numeric(Age)),
           Gear = factor(gear)) %>%
    select(-year, -gear, -value) %>%
    mutate(Gear = factor(model$dat$gear_abbrevs[Gear], levels = model$dat$gear_abbrevs))

  if(last_year_only){
    ls <- ls %>%
      filter(Year == max(Year))
  }

  g <- ggplot(ls, aes(x = Age, y = Proportion, group = 1)) +
    geom_line(size = 0.5, color = "forestgreen") +
    geom_point(size = 1, color = "forestgreen") +
    facet_grid(Year ~ Gear) +
    scale_x_discrete(breaks = seq(0, max(age), 5)) +
    labs(title = title)

  if(show_maturity){
    # Add maturity ogive to selectivity plot
    model$mpd$ma
    if(model$dat$num.sex == 2){
      a50_male <- model$dat$age.at.50.mat[1]
      sigma_a50_male <- model$dat$sd.at.50.mat[1]
      a50_female <- model$dat$age.at.50.mat[2]
      sigma_a50_female <- model$dat$sd.at.50.mat[2]
      g <- g +
        geom_function(fun = function(x){1 / (1 + exp(-(x - a50_male) / sigma_a50_male))}, color = "blue")
    }else{
      a50_female <- model$dat$age.at.50.mat[1]
      sigma_a50_female <- model$dat$sd.at.50.mat[1]
    }
    g <- g +
      geom_function(fun = function(x){1 / (1 + exp(-(x - a50_female) / sigma_a50_female))}, color = "red")
  }
  g
}

