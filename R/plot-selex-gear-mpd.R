#' Plor MCMC selectivities for iSCAM models
#'
#' @description
#' Plot selectivities for a gear which has time-varying selectivity blocks (MPD)
#'
#' @inheritParams plot_selex_mcmc
#' @family Selectivity plotting functions
#'
#' @param gear A gear number to plot
#' @param title The title ([ggplot2] `title`)
#' @return A [ggplot2::ggplot()] object
#' @importFrom dplyr group_split
#' @importFrom stats setNames
#' @export
plot_tv_selex_mpd <- function(model,
                              gear = NULL,
                              title = NULL){

  if(is.null(gear)){
    stop("You must specify a gear to plot", call. = FALSE)
  }

  age <- model$mpd$age

  # Extract selectivity parameters
  sel_par <- model$mpd$sel_par_f %>% as_tibble() %>%
    `names<-`(c("gear", "block", "p1", "p2")) %>% mutate(Sex = "Female")
  if(model$dat$num.sex == 2){
    sel_par_m <- model$mpd$sel_par_m %>% as_tibble() %>%
      `names<-`(c("gear", "block", "p1", "p2")) %>% mutate(Sex = "Male")
    sel_par <- sel_par %>%
      bind_rows(sel_par_m)
  }

  sel_par <- sel_par %>% filter(gear %in% !!gear)

  nblocks <- sel_par %>%
    group_by(gear, Sex) %>%
    group_split() %>%
    map_int(~{nrow(.x)})
  if(length(which(nblocks < 2))){
    stop("The gear you selected does not have time-varying selectivity", call. = FALSE)
  }

  yrs <- model$ctl$start.yr.time.block[gear, ]
  sel_par <- sel_par %>%
    group_by(Sex) %>%
    mutate(Year = yrs) %>%
    ungroup() %>%
    select(-c(gear, block)) %>%
    mutate(Year = factor(Year),
           Sex = factor(Sex)) %>%
    select(Year, Sex, p1, p2) %>%
    group_by(Sex) %>%
    group_split()

  ages <- model$dat$start.age:model$dat$end.age

  sel_par <- map(sel_par, ~{
    x <- .x %>%
      group_by(Year) %>%
      group_split()
    k <- map(x, ~{
      row <- .x
      j <- map_dfr(ages, ~{
        row %>%
          mutate(Age = .x) %>%
          mutate(y = 1 / (1 + exp(-(Age - p1) / p2)))
      })
    }) %>%
      bind_rows()
  }) %>%
    bind_rows() %>%
    mutate(Age = factor(Age))

  # Make year block labels
  unq_years <- c(as.numeric(as.character(sort(unique(sel_par$Year)))), model$dat$end.yr)
  sel_par <- sel_par %>%
    mutate(year_ind = match(Year, unq_years))
  offset_years <- unq_years[-1] - 1
  offset_years[length(offset_years)] <- offset_years[length(offset_years)] + 1
  blocked_years <- paste0(unq_years[-length(unq_years)], "-", offset_years)
  sel_par <- sel_par %>%
    mutate(Year = blocked_years[year_ind]) %>%
    select(-year_ind) %>%
    mutate(Year = factor(Year))

  g <- ggplot(sel_par, aes(x = Age, y = y, group = Sex, color = Sex)) +
    geom_line() +
    geom_line(size = 0.5) +
    geom_point(size = 1) +
    facet_wrap(~Year) +
    scale_x_discrete(breaks = seq(0, max(ages), 5)) +
    labs(title = title) +
    scale_color_manual(values = c("red", "blue"))

  g
}
