#' Plot the selectivity for all gears in the iscam model
#'
#' @rdname plot_selex_mcmc
#' @param title The plot title. NULL means no title
#'
#' @family Selectivity plotting functions
#' @return A [ggplot2::ggplot()] object
#' @importFrom ggplot2 geom_function
#' @export
plot_selex_mpd <- function(model,
                           gear = NULL,
                           show_maturity = FALSE,
                           title = NULL){

  if(class(model) != mdl_cls){
    if(class(model) != mdl_lst_cls){
      stop("`model` is not a gfiscamutils::mdl_cls class (",mdl_cls, ")")
    }
    stop("`model` is a `gfiscamutils::mdl_lst_cls` class (",mdl_lst_cls, ")\n",
         "  It should be a `gfiscamutils::mdl_cls` class (",mdl_cls, ")")
  }

  # Extract selectivity parameters
  sel_par <- model$mpd$sel_par_f
  if(is.null(sel_par)){
    stop("MPD selectivity estimates not found, see `model$mcmc$selest` ",
         "which is created in `read_mcmc()` and `load_special()`")
  }
  sel_par <- sel_par %>%
    as_tibble() %>%
    `names<-`(c("gear", "block", "p1", "p2")) %>%
    mutate(Sex = "Female")

  gear_names <- model$dat$gear_names
  if(length(unique(sel_par$gear)) != length(gear_names)){
    stop("`model$dat$gear_names` is not the same length as the number of gears present ",
         "in the MPD selectivity parameter outputs. Check your declaration of the names ",
         "in the iSCAM data file and try again.")
  }
  if(!is.null(gear)){
    valid_gear_nums <- seq_along(gear_names)
    if(!all(gear %in% valid_gear_nums)){
      stop("One or more of the gear numbers you requested is outside the range of possible gears.\n",
           "Available gears numbers are: ", paste(valid_gear_nums, collapse = ", "), "\n",
           "Names for these are:\n", paste(gear_names, collapse = "\n"))
    }
    sel_par <- sel_par %>%
      filter(gear %in% !!gear)
    gear_names <- gear_names[gear]
  }
  sel_par <- sel_par %>%
    mutate(gear = model$dat$gear_names[gear])

  if(model$dat$num.sex == 2){
    sel_par_m <- model$mpd$sel_par_m %>%
      as_tibble() %>%
      `names<-`(c("gear", "block", "p1", "p2")) %>%
      mutate(Sex = "Male")

    if(!is.null(gear)){
      sel_par_m <- sel_par_m %>%
        filter(gear %in% !!gear)
    }
    sel_par_m <- sel_par_m %>%
      mutate(gear = model$dat$gear_names[gear])

    sel_par <- sel_par %>%
      bind_rows(sel_par_m)
  }

  sel_par_lstsex <- sel_par %>%
    split(~Sex) %>%
    map(~{
      x <- .x %>%
        split(~gear) %>%
        map_int(~{nrow(.x)})
      which(x > 1)
    })

  # sel_par_lstsex contains list of sexes with the gear numbers that are TV selectivity, and must be removed
  if(length(sel_par_lstsex) == 2 && !identical(sel_par_lstsex[[1]], sel_par_lstsex[[2]])){
    stop("The two sexes seem to have different gears with TV selectivity.", call. = FALSE)
  }
  sel_par_lstsex <- sel_par_lstsex[[1]]

  sel_par <- sel_par %>%
    filter(!gear %in% sel_par_lstsex) %>%
    select(-block)

  ages <- model$dat$start.age:model$dat$end.age

  sel_par <- sel_par %>%
    group_by(gear) %>%
    group_split() %>%
    map(~{
      x <- .x %>%
        group_by(Sex) %>%
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
    mutate(Age = as.factor(Age)) %>%
    rename(Gear = gear) %>%
    mutate(Gear = fct_relevel(Gear, gear_names)) %>%
    mutate(Sex = factor(Sex))

  g <- ggplot(sel_par, aes(x = Age, y = y, group = Sex, color = Sex)) +
    geom_line(size = 0.5) +
    geom_point(size = 1) +
    facet_wrap(~ Gear) +
    scale_x_discrete(breaks = seq(0, max(ages), 5)) +
    labs(title = title) +
    scale_color_manual(values = c("red", "blue"))

  if(show_maturity){
    # Add maturity ogive to selectivity plot
    if(model$dat$num.sex == 2){
      a50_male <- model$dat$age.at.50.mat[1]
      sigma_a50_male <- model$dat$sd.at.50.mat[1]
      a50_female <- model$dat$age.at.50.mat[2]
      sigma_a50_female <- model$dat$sd.at.50.mat[2]
      g <- g +
        geom_function(fun = function(x){1 / (1 + exp(-(x - a50_male) / sigma_a50_male))},
                      color = "blue",
                      linetype = "dashed")
    }else{
      a50_female <- model$dat$age.at.50.mat[1]
      sigma_a50_female <- model$dat$sd.at.50.mat[1]
    }
    g <- g +
      geom_function(fun = function(x){1 / (1 + exp(-(x - a50_female) / sigma_a50_female))},
                    color = "red",
                    linetype = "dashed")
  }

  suppressWarnings(print(g))
}

