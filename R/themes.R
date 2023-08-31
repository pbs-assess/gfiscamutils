#' ggplot theme for Herring documents
#'
#' @return a customized [ggplot2::theme()]
#' @importFrom ggplot2 theme theme_bw element_rect margin element_blank
#' element_line alpha unit
#' @export
herring_theme <- function(){
  theme_bw() +
  theme(legend.box.background = element_rect(fill = alpha("white", 0.7)),
        legend.box.margin = margin(1, 1, 1, 1, "mm"),
        legend.key = element_blank(),
        legend.margin = margin(),
        legend.text.align = 1,
        panel.grid.major = element_line(colour = "darkgrey", size = 0.2),
        panel.grid.minor = element_line(colour = "darkgrey", size = 0.1),
        legend.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"))
}
