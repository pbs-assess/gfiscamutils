plot_biomass_proj_mpd <- function(path){

  dat <- read_data_file(file.path(path, "arf.dat"))
  mpd <- read_report_file(file.path(path, "iscam.rep"))
  proj <- read.csv(file.path(path, "iscam_mpd_proj.csv"), header = TRUE) |>
    as_tibble()

  sbt <- mpd$sbt[-length(mpd$sbt)]
  #names(sbt) <- dat$start.yr:dat$end.yr
  sbt <- vec2df(sbt, nms = dat$start.yr:dat$end.yr)
  sbt <- sbt |>
    slice(rep(1:n(), each = nrow(proj)))

  sbt_proj <- proj |>
    select(TAC, grep("^B20[0-9]+$", names(proj))) |>
    rename(catch = TAC)
  names(sbt_proj) <- gsub("^B", "", names(sbt_proj))

  # Remove huge values
  lg <- sbt_proj > 500
  sbt_proj[lg] <- 0

  sbt <- bind_cols(sbt, sbt_proj) |>
    select(catch, everything())

  sbt_long <- sbt |>
    pivot_longer(cols = -catch, names_to = "year", values_to = "sbt") |>
    mutate(year = as.numeric(year),
           catch = factor(catch))

  g <- ggplot(sbt_long, aes(x = year, y = sbt, color = catch)) +
    geom_path() +
    geom_point() +
    ylim(0, NA)

  g
}
