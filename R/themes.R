#' ggplot theme for gfiscam
#'
#' @return a customized [ggplot2::theme()]
#' @importFrom ggplot2 theme theme_set theme_bw element_rect margin element_blank element_line alpha unit
#' @export
gfiscam_theme <- function(){
  theme_bw() +
  theme(legend.box.background = element_rect(fill = alpha("white", 0.7)),
        legend.box.margin = margin(1, 1, 1, 1, "mm"),
        legend.key = element_blank(),
        legend.margin = margin(),
        legend.text.align = 0,
        legend.background = element_rect(fill = "transparent"),
        #strip.background = element_rect(fill = "transparent"),
        strip.background = element_blank(),
        #strip.text.x = element_text(face = "bold", color = "black"),
        #panel.spacing.x=unit(3, "lines"),
        #plot.margin = unit(c(0.1, 0.6, 0.1, 0.1), "lines"),
        plot.margin = margin(12, 12, 12, 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
        )
}
