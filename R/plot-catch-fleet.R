#' Plot catch by fleet
#'
#' @param catch_lst A list of catch data frames, as output from
#' [gfdata::get_catch()]
#' @param fleet_nms A vector of fleet names to use in the plot
#' @param ... Arguments passed to [gfplot::plot_catch()]
#'
#' @return Nothing
#' @export
plot_catch_fleet <- function(catch_lst,
                             fleet_nms, ...){
  ct <- map2_dfr(catch_lst, fleet_nms, ~{
    .x |>
      mutate(area = .y)
  })
  plot_catch(ct, ...)
}
